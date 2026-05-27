"""Load the lab-members allowlist from users.yaml + seed the USERS table.

``users.yaml`` is the single source of truth for *who exists*. Each
entry adds (idempotently) one row to the USERS table on app boot. After
first boot, edits to the file do **not** delete or rewrite existing
rows — the seeder only ever inserts when the username is absent. To
rename, demote, reset, or remove a user, operators use the (future)
admin UI or delete the row directly. This is deliberate: it prevents
the file from becoming a wipe vector (HANDOFF §Auth).

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import logging
from dataclasses import dataclass
from pathlib import Path

import yaml

from .auth import hash_password

log = logging.getLogger(__name__)


@dataclass(frozen=True)
class User:
    id: str
    display_name: str
    email: str | None
    admin: bool
    initial_password: str | None = None
    must_change_password: bool = True


def load_users(path: Path) -> list[User]:
    """Parse users.yaml. No DB access; safe to call from anywhere."""
    if not path.exists():
        raise FileNotFoundError(f"users.yaml not found at {path}")
    with path.open() as fh:
        data = yaml.safe_load(fh) or {}
    raw = data.get("users", [])
    if not isinstance(raw, list) or not raw:
        raise ValueError(f"users.yaml has no users defined: {path}")
    seen = set()
    users: list[User] = []
    for entry in raw:
        uid = str(entry["id"]).strip()
        if not uid or uid in seen:
            raise ValueError(f"users.yaml: invalid or duplicate id {uid!r}")
        seen.add(uid)
        users.append(User(
            id=uid,
            display_name=str(entry.get("display_name", uid)),
            email=entry.get("email"),
            admin=bool(entry.get("admin", False)),
            initial_password=entry.get("initial_password"),
            must_change_password=bool(entry.get("must_change_password", True)),
        ))
    return users


def seed_users_table(yaml_users, users_dao) -> int:
    """Insert any yaml users not yet present in the USERS table.

    Returns the count of new rows inserted. Skips existing usernames
    silently. Each new row uses the yaml's ``initial_password``
    (bcrypt-hashed) and ``must_change_password`` flag.

    Raises ValueError when a brand-new user is missing
    ``initial_password`` — we'd otherwise insert an unloginable row
    that would block the operator from spotting the misconfiguration
    until first login fails.
    """
    inserted = 0
    for u in yaml_users:
        existing = users_dao.get_by_username(u.id)
        if existing is not None:
            continue
        if not u.email:
            raise ValueError(
                f"users.yaml: user {u.id!r} has no email — required for seed")
        if not u.initial_password:
            raise ValueError(
                f"users.yaml: user {u.id!r} has no initial_password — required "
                f"for seed (set must_change_password: true and the operator "
                f"will rotate on first login)"
            )
        users_dao.create(
            username=u.id,
            email=u.email,
            hashed_password=hash_password(u.initial_password),
            role="admin" if u.admin else "user",
            must_change_password=u.must_change_password,
        )
        log.info("seeded user %s (role=%s)", u.id,
                 "admin" if u.admin else "user")
        inserted += 1
    return inserted
