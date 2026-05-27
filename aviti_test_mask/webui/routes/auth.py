"""Auth blueprint: login, logout, change/forgot/reset password.

Five routes, no URL prefix (mounted at app root). All public except
``/change-password`` which requires a session. ``/forgot-password`` is
anti-enumeration: returns the same success template whether the email
exists or not.

State carried in the Flask session:

- ``user_id``: int — primary FK, source of truth for ``@login_required``
- ``username``: str — shown in the navbar, stamped on jobs as the
  ``submitter`` field by the submit route
- ``role``: ``"admin"`` | ``"user"`` — checked by ``@admin_required`` and
  by the submit_job concurrency-cap bypass
- ``must_change_password``: bool — when true, the login response routes
  the browser to ``/change-password`` and stays there until cleared

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import logging
import secrets
from datetime import datetime, timedelta, timezone

from flask import (
    Blueprint, current_app, jsonify, redirect, render_template,
    request, session, url_for,
)

from services.auth import (
    cleanup_old_sessions, hash_password, login_required, verify_password,
)
from services.email import send_password_change_confirmation_email, send_password_reset_email
from services.users_dao import UsersDAO

log = logging.getLogger(__name__)

auth_bp = Blueprint("auth", __name__)


# ---- helpers ----

def _users_dao() -> UsersDAO:
    return current_app.config["USERS_DAO"]


def _cfg():
    return current_app.config["WEBUI_CONFIG"]


def _min_pw_length() -> int:
    return int(getattr(_cfg(), "password_min_length", 8))


def _email_configured() -> bool:
    return bool(current_app.config.get("MAIL_USERNAME"))


def _utc_now() -> datetime:
    return datetime.now(timezone.utc)


def _iso(dt: datetime) -> str:
    return dt.strftime("%Y-%m-%dT%H:%M:%SZ")


def _parse_iso(s: str) -> datetime | None:
    try:
        return datetime.strptime(s, "%Y-%m-%dT%H:%M:%SZ").replace(
            tzinfo=timezone.utc)
    except (ValueError, TypeError):
        return None


def _payload() -> dict:
    """Accept JSON body (AJAX) or form-encoded (plain POST)."""
    if request.is_json:
        return request.get_json(silent=True) or {}
    return request.form.to_dict()


# ---- /login ----

@auth_bp.route("/login", methods=["GET", "POST"])
def login():
    if request.method == "GET":
        if session.get("user_id"):
            return redirect(url_for("pages.home"))
        return render_template("login.html")

    data = _payload()
    username = (data.get("username") or "").strip()
    password = data.get("password") or ""
    if not username or not password:
        return jsonify({"error": "username and password are required"}), 400

    dao = _users_dao()
    user = dao.get_by_username(username)
    if user is None or not verify_password(password, user.hashed_password):
        # Same error for both branches — never reveal which one failed.
        return jsonify({"error": "invalid username or password"}), 401

    # Successful login. Plant the session keys and clean stale session
    # files on the way through.
    session.clear()
    session["user_id"] = user.user_id
    session["username"] = user.username
    session["role"] = user.role
    session["must_change_password"] = user.must_change_password
    session.permanent = True

    try:
        sd = current_app.config.get("SESSION_FILE_DIR")
        if sd:
            cleanup_old_sessions(
                sd, age_hours=getattr(_cfg(), "cleanup_age_hours", 72))
    except Exception as exc:  # noqa: BLE001 — never block login
        log.warning("session cleanup skipped: %s", exc)

    return jsonify({
        "success": True,
        "username": user.username,
        "role": user.role,
        "must_change_password": user.must_change_password,
    })


# ---- /logout ----

@auth_bp.route("/logout", methods=["POST", "GET"])
def logout():
    session.clear()
    if request.method == "GET":
        return redirect(url_for("auth.login"))
    return jsonify({"success": True})


# ---- /change-password ----

@auth_bp.route("/change-password", methods=["GET", "POST"])
@login_required
def change_password():
    if request.method == "GET":
        return render_template(
            "change_password.html",
            must_change=bool(session.get("must_change_password")),
            password_min_length=_min_pw_length(),
        )

    data = _payload()
    current = data.get("current_password") or ""
    new = data.get("new_password") or ""
    confirm = data.get("confirm_password") or ""

    if new != confirm:
        return jsonify({"error": "new passwords do not match"}), 400
    if len(new) < _min_pw_length():
        return jsonify({
            "error": f"new password must be at least {_min_pw_length()} characters",
        }), 400

    dao = _users_dao()
    user = dao.get_by_id(session["user_id"])
    if user is None:
        # The session points at a row that no longer exists. Force re-login.
        session.clear()
        return jsonify({"error": "session expired"}), 401
    if not verify_password(current, user.hashed_password):
        return jsonify({"error": "current password is incorrect"}), 400

    dao.set_password(
        user.user_id,
        hashed_password=hash_password(new),
        must_change_password=False,
    )
    session["must_change_password"] = False
    # Best-effort confirmation email — never block on it.
    try:
        send_password_change_confirmation_email(user)
    except Exception as exc:  # noqa: BLE001
        log.warning("change-pw confirmation email failed: %s", exc)
    return jsonify({"success": True})


# ---- /forgot-password ----

@auth_bp.route("/forgot-password", methods=["GET", "POST"])
def forgot_password():
    # Anti-enumeration: GET shows the form; POST always renders the
    # "submitted" template whether the email is registered or not.
    if not _email_configured() and request.method == "GET":
        return render_template(
            "forgot_password.html",
            submitted=False,
            email_configured=False,
        )
    if request.method == "GET":
        return render_template(
            "forgot_password.html",
            submitted=False,
            email_configured=True,
        )

    email = ((_payload().get("email") or "").strip()).lower()
    if email and _email_configured():
        dao = _users_dao()
        user = dao.get_by_email(email)
        if user is not None:
            token = secrets.token_urlsafe(32)
            expires = _iso(_utc_now() + timedelta(hours=1))
            dao.set_reset_token(user.user_id, token=token,
                                expires_iso=expires)
            reset_url = url_for(
                "auth.reset_password", token=token, _external=True)
            try:
                send_password_reset_email(user, reset_url)
            except Exception as exc:  # noqa: BLE001
                log.warning("forgot-pw email send failed: %s", exc)

    # Always success page — never leak whether the email matched.
    return render_template(
        "forgot_password.html",
        submitted=True,
        email_configured=_email_configured(),
    )


# ---- /reset-password/<token> ----

@auth_bp.route("/reset-password/<token>", methods=["GET", "POST"])
def reset_password(token: str):
    dao = _users_dao()
    user = dao.get_by_reset_token(token)
    expired = True
    if user is not None and user.reset_token_expires:
        when = _parse_iso(user.reset_token_expires)
        expired = when is None or when < _utc_now()

    if request.method == "GET":
        if user is None or expired:
            return render_template(
                "reset_password.html",
                error="This reset link is invalid or has expired.",
                token=token,
                password_min_length=_min_pw_length(),
            )
        return render_template(
            "reset_password.html",
            token=token,
            password_min_length=_min_pw_length(),
        )

    # POST
    if user is None or expired:
        return jsonify({"error": "invalid or expired reset link"}), 400
    data = _payload()
    new = data.get("new_password") or ""
    confirm = data.get("confirm_password") or ""
    if new != confirm:
        return jsonify({"error": "new passwords do not match"}), 400
    if len(new) < _min_pw_length():
        return jsonify({
            "error": f"new password must be at least {_min_pw_length()} characters",
        }), 400

    dao.set_password(
        user.user_id,
        hashed_password=hash_password(new),
        must_change_password=False,
    )
    try:
        send_password_change_confirmation_email(user)
    except Exception as exc:  # noqa: BLE001
        log.warning("reset confirmation email failed: %s", exc)
    return jsonify({"success": True})
