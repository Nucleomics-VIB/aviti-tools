"""Unit tests for services/auth.py.

Covers:
- bcrypt hash + verify round-trip
- decorator behaviour (anonymous, logged-in, admin-required, AJAX vs browser)
- session-dir cleanup bounding (only Flask-Session-pattern files in
  the configured dir; respects mtime cutoff; safe when dir missing)
"""
from __future__ import annotations

import time
from pathlib import Path

import pytest
from flask import Flask, Blueprint, jsonify, session

from services.auth import (
    admin_required,
    cleanup_old_sessions,
    hash_password,
    login_required,
    verify_password,
)


# ---- bcrypt helpers ----

def test_hash_verify_roundtrip():
    h = hash_password("hunter2")
    assert h.startswith("$2")
    assert verify_password("hunter2", h) is True
    assert verify_password("wrong", h) is False


def test_verify_rejects_empty_inputs():
    h = hash_password("abc")
    assert verify_password("", h) is False
    assert verify_password("abc", "") is False
    assert verify_password("", "") is False


def test_verify_rejects_malformed_hash():
    # Garbage 'hash' should yield False, not raise.
    assert verify_password("abc", "not-a-bcrypt-string") is False


def test_hash_rejects_empty_password():
    with pytest.raises(ValueError):
        hash_password("")


# ---- decorator app fixtures ----

def _make_app() -> Flask:
    """Tiny Flask app with one login_required and one admin_required route,
    plus a stub auth.login endpoint for the redirect target."""
    app = Flask(__name__)
    app.secret_key = "test-secret"

    bp = Blueprint("test_pages", __name__)
    auth_bp = Blueprint("auth", __name__)

    @auth_bp.route("/login")
    def login():
        return "login-page"

    @bp.route("/protected")
    @login_required
    def protected():
        return jsonify({"ok": True})

    @bp.route("/admin-only")
    @admin_required
    def admin_only():
        return jsonify({"ok": True, "where": "admin"})

    app.register_blueprint(bp)
    app.register_blueprint(auth_bp)
    return app


def _login_as(client, *, user_id: int = 1, username: str = "u",
              role: str = "user") -> None:
    with client.session_transaction() as s:
        s["user_id"] = user_id
        s["username"] = username
        s["role"] = role


# ---- decorator behaviour ----

def test_login_required_anonymous_browser_redirects_to_login():
    app = _make_app()
    client = app.test_client()
    resp = client.get("/protected", follow_redirects=False)
    assert resp.status_code == 302
    assert "/login" in resp.location
    # ``next`` param preserves where the user wanted to go.
    assert "next=/protected" in resp.location or "next=%2Fprotected" in resp.location


def test_login_required_anonymous_ajax_returns_401_json():
    app = _make_app()
    client = app.test_client()
    resp = client.get("/protected",
                      headers={"X-Requested-With": "XMLHttpRequest"})
    assert resp.status_code == 401
    assert resp.is_json
    assert resp.get_json()["error"]


def test_login_required_logged_in_passes():
    app = _make_app()
    client = app.test_client()
    _login_as(client)
    resp = client.get("/protected")
    assert resp.status_code == 200
    assert resp.get_json() == {"ok": True}


def test_admin_required_user_role_returns_403():
    app = _make_app()
    client = app.test_client()
    _login_as(client, role="user")
    resp = client.get("/admin-only",
                      headers={"X-Requested-With": "XMLHttpRequest"})
    assert resp.status_code == 403


def test_admin_required_admin_role_passes():
    app = _make_app()
    client = app.test_client()
    _login_as(client, role="admin")
    resp = client.get("/admin-only")
    assert resp.status_code == 200
    assert resp.get_json()["where"] == "admin"


def test_admin_required_anonymous_redirects():
    app = _make_app()
    client = app.test_client()
    resp = client.get("/admin-only", follow_redirects=False)
    assert resp.status_code == 302
    assert "/login" in resp.location


# ---- session cleanup (bounded) ----

def _make_session_file(dir_: Path, name: str, age_hours: float) -> Path:
    dir_.mkdir(parents=True, exist_ok=True)
    f = dir_ / name
    f.write_text("x")
    when = time.time() - (age_hours * 3600)
    import os
    os.utime(f, (when, when))
    return f


def test_cleanup_old_sessions_only_removes_old_pattern_files(tmp_path):
    sd = tmp_path / "flask_session"
    fresh = _make_session_file(sd, "a" * 32, age_hours=1)         # young
    stale = _make_session_file(sd, "b" * 32, age_hours=200)       # old
    not_pattern = _make_session_file(sd, "README.txt", age_hours=200)
    # Deeply stale but in a subdir — we don't recurse.
    sub = sd / "subdir"
    sub.mkdir()
    nested = _make_session_file(sub, "c" * 32, age_hours=999)

    removed = cleanup_old_sessions(sd, age_hours=72)
    assert removed == 1
    assert fresh.exists()
    assert not stale.exists()
    assert not_pattern.exists()      # non-pattern: untouched
    assert nested.exists()           # nested: untouched


def test_cleanup_old_sessions_missing_dir_is_noop(tmp_path):
    nope = tmp_path / "does-not-exist"
    assert cleanup_old_sessions(nope) == 0


def test_cleanup_old_sessions_will_not_delete_parent_or_dirs(tmp_path):
    """Smoke-check the safety property: the function must never remove the
    session_dir itself, regardless of how old it claims to be."""
    sd = tmp_path / "flask_session"
    sd.mkdir()
    # No files in it; just call.
    assert cleanup_old_sessions(sd, age_hours=0) == 0
    assert sd.exists()
    assert sd.is_dir()
