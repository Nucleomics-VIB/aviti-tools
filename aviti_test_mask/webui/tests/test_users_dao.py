"""Unit tests for services/users_dao.py.

Exercises every read and write path against a throwaway tmp_path DB.
JobsDAO is instantiated first to bring up the shared schema (USERS lives
in db.py's SCHEMA_SQL alongside JOBS).
"""
from __future__ import annotations

import pytest

from services.db import JobsDAO
from services.users_dao import UsersDAO


def _make_daos(tmp_path):
    db_path = tmp_path / "test.db"
    jobs_dao = JobsDAO(db_path)  # creates schema (incl. users table)
    users_dao = UsersDAO(db_path)
    return jobs_dao, users_dao


def test_create_and_get_by_id(tmp_path):
    _, dao = _make_daos(tmp_path)
    uid = dao.create(
        username="splaisan", email="s@vib.be",
        hashed_password="hashed", role="admin",
    )
    rec = dao.get_by_id(uid)
    assert rec is not None
    assert rec.username == "splaisan"
    assert rec.email == "s@vib.be"
    assert rec.role == "admin"
    assert rec.must_change_password is True  # default
    assert rec.reset_token is None
    assert rec.created_at == rec.updated_at  # fresh row


def test_get_by_username_case_insensitive(tmp_path):
    _, dao = _make_daos(tmp_path)
    dao.create(username="Splaisan", email="s@vib.be",
               hashed_password="h", role="admin")
    assert dao.get_by_username("splaisan") is not None
    assert dao.get_by_username("SPLAISAN") is not None
    assert dao.get_by_username("nope") is None


def test_get_by_email_case_insensitive(tmp_path):
    _, dao = _make_daos(tmp_path)
    dao.create(username="u", email="Stephane@VIB.be",
               hashed_password="h", role="user")
    assert dao.get_by_email("stephane@vib.be") is not None
    assert dao.get_by_email("STEPHANE@vib.be") is not None
    assert dao.get_by_email("other@vib.be") is None


def test_invalid_role_rejected(tmp_path):
    _, dao = _make_daos(tmp_path)
    with pytest.raises(ValueError, match="invalid role"):
        dao.create(username="u", email="u@x.org",
                   hashed_password="h", role="superadmin")


def test_unique_username_collision_raises(tmp_path):
    _, dao = _make_daos(tmp_path)
    dao.create(username="u", email="a@x.org", hashed_password="h",
               role="user")
    import sqlite3
    with pytest.raises(sqlite3.IntegrityError):
        dao.create(username="u", email="b@x.org", hashed_password="h",
                   role="user")


def test_unique_email_collision_raises(tmp_path):
    _, dao = _make_daos(tmp_path)
    dao.create(username="a", email="shared@x.org", hashed_password="h",
               role="user")
    import sqlite3
    with pytest.raises(sqlite3.IntegrityError):
        dao.create(username="b", email="shared@x.org",
                   hashed_password="h", role="user")


def test_update_unknown_column_rejected(tmp_path):
    _, dao = _make_daos(tmp_path)
    uid = dao.create(username="u", email="u@x.org",
                     hashed_password="h", role="user")
    with pytest.raises(ValueError, match="unknown / non-updatable"):
        dao.update(uid, username="hijack")  # username not updatable


def test_set_password_rotates_and_clears_token(tmp_path):
    _, dao = _make_daos(tmp_path)
    uid = dao.create(username="u", email="u@x.org",
                     hashed_password="old", role="user")
    dao.set_reset_token(uid, token="abc", expires_iso="2030-01-01T00:00:00Z")
    rec_before = dao.get_by_id(uid)
    assert rec_before.reset_token == "abc"

    dao.set_password(uid, hashed_password="new", must_change_password=False)
    rec_after = dao.get_by_id(uid)
    assert rec_after.hashed_password == "new"
    assert rec_after.must_change_password is False
    # Outstanding reset link is invalidated by a manual password change.
    assert rec_after.reset_token is None
    assert rec_after.reset_token_expires is None


def test_get_by_reset_token(tmp_path):
    _, dao = _make_daos(tmp_path)
    uid = dao.create(username="u", email="u@x.org",
                     hashed_password="h", role="user")
    dao.set_reset_token(uid, token="tok-123",
                        expires_iso="2030-01-01T00:00:00Z")
    rec = dao.get_by_reset_token("tok-123")
    assert rec is not None and rec.user_id == uid
    assert dao.get_by_reset_token("nope") is None
    assert dao.get_by_reset_token("") is None  # short-circuits


def test_clear_reset_token(tmp_path):
    _, dao = _make_daos(tmp_path)
    uid = dao.create(username="u", email="u@x.org",
                     hashed_password="h", role="user")
    dao.set_reset_token(uid, token="tok",
                        expires_iso="2030-01-01T00:00:00Z")
    dao.clear_reset_token(uid)
    rec = dao.get_by_id(uid)
    assert rec.reset_token is None
    assert rec.reset_token_expires is None


def test_list_all_ordered_by_username(tmp_path):
    _, dao = _make_daos(tmp_path)
    dao.create(username="zeta", email="z@x.org",
               hashed_password="h", role="user")
    dao.create(username="alpha", email="a@x.org",
               hashed_password="h", role="user")
    dao.create(username="mu", email="m@x.org",
               hashed_password="h", role="admin")
    names = [u.username for u in dao.list_all()]
    assert names == ["alpha", "mu", "zeta"]


def test_must_change_password_coerced_on_update(tmp_path):
    _, dao = _make_daos(tmp_path)
    uid = dao.create(username="u", email="u@x.org",
                     hashed_password="h", role="user",
                     must_change_password=True)
    dao.update(uid, must_change_password=False)
    rec = dao.get_by_id(uid)
    assert rec.must_change_password is False


def test_v1_to_v2_migration(tmp_path):
    """A v1 DB (no users table) must transparently bump to v2."""
    import sqlite3
    db_path = tmp_path / "legacy.db"
    # Build a minimal v1 DB by hand — only schema_version row matters
    # for the transition test.
    conn = sqlite3.connect(db_path, isolation_level=None)
    conn.execute("CREATE TABLE schema_version (version INTEGER PRIMARY KEY)")
    conn.execute("INSERT INTO schema_version VALUES (1)")
    conn.close()

    # Opening with current code should: create the users table (CREATE
    # IF NOT EXISTS) and bump schema_version 1 → 2.
    JobsDAO(db_path)
    conn = sqlite3.connect(db_path)
    version = conn.execute(
        "SELECT version FROM schema_version").fetchone()[0]
    conn.close()
    assert version == 2

    # The users DAO should be usable on the migrated DB.
    udao = UsersDAO(db_path)
    udao.create(username="x", email="x@x.org",
                hashed_password="h", role="user")
    assert udao.get_by_username("x") is not None
