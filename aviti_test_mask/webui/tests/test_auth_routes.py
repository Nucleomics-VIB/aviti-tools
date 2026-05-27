"""Integration tests for routes/auth.py.

Spins up a tiny Flask app with the auth blueprint, a real
``UsersDAO`` against a tmp_path SQLite file, and the email module
configured to record-only. Covers:

- /login success / wrong password / unknown user — all return the same
  401 message (no enumeration via wording)
- /login plants session keys correctly
- /forgot-password is anti-enumeration: unknown email → same 'submitted'
  template as a known email; a reset email is captured for the known one
  and not for the unknown one
- /reset-password/<token>: invalid token rejected; valid token accepts new
  password; expired token rejected even if matched
- /change-password requires login (decorator integration), current password
  check, and length check
"""
from __future__ import annotations

from datetime import datetime, timedelta, timezone
from pathlib import Path
from types import SimpleNamespace

import pytest
from flask import Flask

from services.auth import hash_password
from services.db import JobsDAO
from services.email import init_mail, mail
from services.users_dao import UsersDAO
from routes.auth import auth_bp


def _make_app(tmp_path: Path) -> Flask:
    db = tmp_path / "test.db"
    JobsDAO(db)                          # init shared schema
    users_dao = UsersDAO(db)
    # Pre-seed three users mirroring the live users.yaml (so the login
    # flow can exercise admin + non-admin paths against the real bcrypt
    # round-trip, not stubs).
    users_dao.create(
        username="splaisan", email="s@vib.be",
        hashed_password=hash_password("changeme"),
        role="admin", must_change_password=True)
    users_dao.create(
        username="alice", email="alice@example.com",
        hashed_password=hash_password("alicepass"),
        role="user", must_change_password=False)

    app = Flask(
        __name__,
        template_folder=str(Path(__file__).parent.parent / "templates"),
        static_folder=str(Path(__file__).parent.parent / "static"),
    )
    app.secret_key = "test-secret"
    app.config["USERS_DAO"] = users_dao
    app.config["WEBUI_CONFIG"] = SimpleNamespace(
        app_name="aviti_test_mask",
        org_name="VIB Nucleomics Core",
        support_email="nucleomics@vib.be",
        password_min_length=8,
        cleanup_age_hours=72,
    )
    app.config["SESSION_FILE_DIR"] = tmp_path / "flask_session"
    app.config["MAIL_SERVER"] = "smtp.gmail.com"
    app.config["MAIL_PORT"] = 587
    app.config["MAIL_USE_TLS"] = True
    app.config["MAIL_USERNAME"] = "bot@gmail.com"
    app.config["MAIL_PASSWORD"] = "x"
    app.config["MAIL_DEFAULT_SENDER"] = "bot@gmail.com"
    app.config["MAIL_SUPPRESS_SEND"] = True
    init_mail(app)

    app.register_blueprint(auth_bp)
    return app


# ---- /login ----

def test_login_success_plants_session(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    resp = client.post("/login", json={
        "username": "alice", "password": "alicepass"})
    assert resp.status_code == 200
    body = resp.get_json()
    assert body["success"] is True
    assert body["role"] == "user"
    assert body["must_change_password"] is False
    with client.session_transaction() as s:
        assert s["user_id"]  # row exists
        assert s["username"] == "alice"
        assert s["role"] == "user"
        assert s["must_change_password"] is False


def test_login_must_change_password_signaled(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    resp = client.post("/login", json={
        "username": "splaisan", "password": "changeme"})
    assert resp.status_code == 200
    assert resp.get_json()["must_change_password"] is True


def test_login_wrong_password_returns_same_error_as_unknown_user(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    r_wrong = client.post("/login", json={
        "username": "alice", "password": "wrong"})
    r_unknown = client.post("/login", json={
        "username": "ghost", "password": "anything"})
    assert r_wrong.status_code == 401
    assert r_unknown.status_code == 401
    # Identical wording → no enumeration via response.
    assert r_wrong.get_json() == r_unknown.get_json()


def test_login_missing_fields_400(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    resp = client.post("/login", json={"username": "", "password": ""})
    assert resp.status_code == 400


# ---- /logout ----

def test_logout_clears_session(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    client.post("/login", json={"username": "alice", "password": "alicepass"})
    resp = client.post("/logout")
    assert resp.status_code == 200
    with client.session_transaction() as s:
        assert "user_id" not in s


# ---- /forgot-password ----

def test_forgot_password_unknown_email_same_template_no_email(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    with app.app_context():
        with mail.record_messages() as outbox:
            resp = client.post("/forgot-password",
                               data={"email": "nope@example.com"})
            assert resp.status_code == 200
            assert b"reset link has been sent" in resp.data
            # Crucially: no email actually sent for an unknown address.
            assert outbox == []


def test_forgot_password_known_email_sends_reset_link(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    with app.app_context():
        with mail.record_messages() as outbox:
            resp = client.post("/forgot-password",
                               data={"email": "alice@example.com"})
            assert resp.status_code == 200
            assert b"reset link has been sent" in resp.data
            assert len(outbox) == 1
            assert outbox[0].recipients == ["alice@example.com"]
            # The body contains the reset URL with the generated token.
            assert "/reset-password/" in outbox[0].html


# ---- /reset-password ----

def test_reset_password_invalid_token_get_returns_error(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    resp = client.get("/reset-password/nope-not-a-token")
    assert resp.status_code == 200
    assert b"invalid or has expired" in resp.data


def test_reset_password_valid_token_changes_password(tmp_path):
    app = _make_app(tmp_path)
    dao: UsersDAO = app.config["USERS_DAO"]
    user = dao.get_by_username("alice")
    expires = (datetime.now(timezone.utc) + timedelta(hours=1)).strftime(
        "%Y-%m-%dT%H:%M:%SZ")
    dao.set_reset_token(user.user_id, token="GOODTOKEN",
                        expires_iso=expires)

    client = app.test_client()
    resp = client.post("/reset-password/GOODTOKEN", json={
        "new_password": "brand-new-pw-9",
        "confirm_password": "brand-new-pw-9",
    })
    assert resp.status_code == 200
    # New password works, old one doesn't, reset token cleared.
    resp = client.post("/login", json={
        "username": "alice", "password": "brand-new-pw-9"})
    assert resp.status_code == 200
    resp = client.post("/login", json={
        "username": "alice", "password": "alicepass"})
    assert resp.status_code == 401
    refreshed = dao.get_by_username("alice")
    assert refreshed.reset_token is None


def test_reset_password_expired_token_rejected(tmp_path):
    app = _make_app(tmp_path)
    dao: UsersDAO = app.config["USERS_DAO"]
    user = dao.get_by_username("alice")
    expires = (datetime.now(timezone.utc) - timedelta(seconds=1)).strftime(
        "%Y-%m-%dT%H:%M:%SZ")
    dao.set_reset_token(user.user_id, token="STALETOKEN",
                        expires_iso=expires)
    client = app.test_client()
    resp = client.post("/reset-password/STALETOKEN", json={
        "new_password": "brand-new-pw-9",
        "confirm_password": "brand-new-pw-9",
    })
    assert resp.status_code == 400


def test_reset_password_mismatch_rejected(tmp_path):
    app = _make_app(tmp_path)
    dao = app.config["USERS_DAO"]
    user = dao.get_by_username("alice")
    expires = (datetime.now(timezone.utc) + timedelta(hours=1)).strftime(
        "%Y-%m-%dT%H:%M:%SZ")
    dao.set_reset_token(user.user_id, token="TOK", expires_iso=expires)
    client = app.test_client()
    resp = client.post("/reset-password/TOK", json={
        "new_password": "newpw-9chars",
        "confirm_password": "different",
    })
    assert resp.status_code == 400


# ---- /change-password ----

def test_change_password_requires_login(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    resp = client.post("/change-password",
                       json={"current_password": "x", "new_password": "y",
                             "confirm_password": "y"},
                       headers={"X-Requested-With": "XMLHttpRequest"})
    assert resp.status_code == 401


def test_change_password_current_password_must_match(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    client.post("/login", json={
        "username": "alice", "password": "alicepass"})
    resp = client.post("/change-password", json={
        "current_password": "wrong",
        "new_password": "brand-new-pw-9",
        "confirm_password": "brand-new-pw-9",
    })
    assert resp.status_code == 400


def test_change_password_success_rotates(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    client.post("/login", json={
        "username": "alice", "password": "alicepass"})
    resp = client.post("/change-password", json={
        "current_password": "alicepass",
        "new_password": "fresh-pw-1234",
        "confirm_password": "fresh-pw-1234",
    })
    assert resp.status_code == 200
    # Old password no longer works.
    client.post("/logout")
    bad = client.post("/login", json={
        "username": "alice", "password": "alicepass"})
    assert bad.status_code == 401
    good = client.post("/login", json={
        "username": "alice", "password": "fresh-pw-1234"})
    assert good.status_code == 200


def test_change_password_short_new_rejected(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    client.post("/login", json={
        "username": "alice", "password": "alicepass"})
    resp = client.post("/change-password", json={
        "current_password": "alicepass",
        "new_password": "short",
        "confirm_password": "short",
    })
    assert resp.status_code == 400
