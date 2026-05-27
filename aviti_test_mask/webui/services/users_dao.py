"""SQLite DAO for the USERS table (session auth).

Mirrors the JobsDAO style in services/db.py — raw SQL, no ORM, explicit
column allowlists to defeat caller-controlled column injection. The USERS
table itself lives in services/db.py's SCHEMA_SQL alongside JOBS so a
single ``conn.executescript(SCHEMA_SQL)`` brings up the whole schema.

Two non-obvious rules:

- Username and email lookups are **case-insensitive**. This matches the
  FreezerManager pattern and means "Splaisan" and "splaisan" log in to
  the same row.
- ``create()`` is the only path that inserts. Yaml seeding lives in
  services/users_loader.py.seed_users_table() which calls create() iff the
  username is absent — guaranteed idempotent. Never delete or update a
  USERS row from the yaml file (operator uses admin reset for that).

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import sqlite3
from contextlib import contextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

from .db import utc_now_iso


@dataclass(frozen=True)
class UserRecord:
    user_id: int
    username: str
    email: str
    hashed_password: str
    role: str  # 'admin' | 'user'
    must_change_password: bool
    reset_token: str | None
    reset_token_expires: str | None  # ISO-8601 UTC
    created_at: str
    updated_at: str


_USER_UPDATABLE: frozenset[str] = frozenset({
    "hashed_password", "role", "must_change_password",
    "reset_token", "reset_token_expires", "email",
})


def _row_to_record(row: sqlite3.Row) -> UserRecord:
    return UserRecord(
        user_id=row["user_id"],
        username=row["username"],
        email=row["email"],
        hashed_password=row["hashed_password"],
        role=row["role"],
        must_change_password=bool(row["must_change_password"]),
        reset_token=row["reset_token"],
        reset_token_expires=row["reset_token_expires"],
        created_at=row["created_at"],
        updated_at=row["updated_at"],
    )


class UsersDAO:
    """Read/write access to the USERS table.

    Constructor only stores the path; the JobsDAO sibling is responsible
    for initialising the shared schema (one DB file, one schema-version
    row). This keeps the boot-time wiring simple: instantiate JobsDAO
    first, then UsersDAO against the same path.
    """

    def __init__(self, path: Path):
        self.path = Path(path)

    @contextmanager
    def _connect(self) -> Iterator[sqlite3.Connection]:
        conn = sqlite3.connect(self.path, isolation_level=None)
        conn.row_factory = sqlite3.Row
        try:
            conn.execute("PRAGMA foreign_keys = ON")
            yield conn
        finally:
            conn.close()

    # ---- lookups ----

    def get_by_id(self, user_id: int) -> UserRecord | None:
        with self._connect() as conn:
            row = conn.execute(
                "SELECT * FROM users WHERE user_id=?", (user_id,)
            ).fetchone()
            return _row_to_record(row) if row else None

    def get_by_username(self, username: str) -> UserRecord | None:
        # Case-insensitive — UNIQUE is on the value as inserted; we
        # accept any-case input by comparing folded.
        with self._connect() as conn:
            row = conn.execute(
                "SELECT * FROM users WHERE LOWER(username)=LOWER(?)",
                (username,),
            ).fetchone()
            return _row_to_record(row) if row else None

    def get_by_email(self, email: str) -> UserRecord | None:
        with self._connect() as conn:
            row = conn.execute(
                "SELECT * FROM users WHERE LOWER(email)=LOWER(?)", (email,)
            ).fetchone()
            return _row_to_record(row) if row else None

    def get_by_reset_token(self, token: str) -> UserRecord | None:
        if not token:
            return None
        with self._connect() as conn:
            row = conn.execute(
                "SELECT * FROM users WHERE reset_token=?", (token,)
            ).fetchone()
            return _row_to_record(row) if row else None

    def list_all(self) -> list[UserRecord]:
        with self._connect() as conn:
            rows = conn.execute(
                "SELECT * FROM users ORDER BY username"
            ).fetchall()
            return [_row_to_record(r) for r in rows]

    # ---- writes ----

    def create(
        self,
        *,
        username: str,
        email: str,
        hashed_password: str,
        role: str,
        must_change_password: bool = True,
    ) -> int:
        """Insert a new user; returns the new ``user_id``.

        Raises sqlite3.IntegrityError on UNIQUE collision. The caller
        (the yaml seeder) should look up by username first and skip on
        conflict, rather than catching the integrity error.
        """
        if role not in ("admin", "user"):
            raise ValueError(f"invalid role {role!r}")
        now = utc_now_iso()
        with self._connect() as conn:
            cur = conn.execute(
                """
                INSERT INTO users (
                    username, email, hashed_password, role,
                    must_change_password, created_at, updated_at
                ) VALUES (?, ?, ?, ?, ?, ?, ?)
                """,
                (username, email, hashed_password, role,
                 int(must_change_password), now, now),
            )
            return cur.lastrowid  # type: ignore[return-value]

    def update(self, user_id: int, **fields: object) -> None:
        if not fields:
            return
        bad = [k for k in fields if k not in _USER_UPDATABLE]
        if bad:
            raise ValueError(f"unknown / non-updatable columns: {bad}")
        # Coerce booleans for the integer column.
        if "must_change_password" in fields:
            fields["must_change_password"] = int(bool(
                fields["must_change_password"]))
        fields["updated_at"] = utc_now_iso()
        set_clause = ",".join(f"{k}=:{k}" for k in fields)
        params = dict(fields)
        params["user_id"] = user_id
        with self._connect() as conn:
            conn.execute(
                f"UPDATE users SET {set_clause} WHERE user_id=:user_id",
                params,
            )

    def set_password(
        self,
        user_id: int,
        *,
        hashed_password: str,
        must_change_password: bool = False,
    ) -> None:
        """Sugar for the common 'password rotated' write.

        Also clears any outstanding reset token so a used reset link
        can't be replayed after a manual change-password.
        """
        self.update(
            user_id,
            hashed_password=hashed_password,
            must_change_password=must_change_password,
            reset_token=None,
            reset_token_expires=None,
        )

    def set_reset_token(
        self,
        user_id: int,
        *,
        token: str,
        expires_iso: str,
    ) -> None:
        self.update(
            user_id,
            reset_token=token,
            reset_token_expires=expires_iso,
        )

    def clear_reset_token(self, user_id: int) -> None:
        self.update(user_id, reset_token=None, reset_token_expires=None)
