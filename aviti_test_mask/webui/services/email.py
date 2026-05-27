"""Email helper — Flask-Mail wrapper plus named senders.

Factored out of services/auth.py so the auth routes know nothing about
SMTP details. Adding a new email type (welcome, backup confirmation,
alert, …) is a new function here, not a new dependency in auth code.
Swapping the provider (Gmail → SendGrid → university gateway) is a
config change, not a code change.

The module is safe to import even when SMTP is unconfigured:
``MAIL_USERNAME`` unset → every send returns ``False`` and logs a
warning, so the auth flow still works locally (reset emails just don't
go out; ``/forgot-password`` renders a 'contact administrator' notice
instead of leaking the unconfigured state).

Provider context: the VIB / KU Leuven SMTP gateway does not accept
arbitrary application authentication — we masquerade through Gmail
using an app-password (``MAIL_USERNAME=…@gmail.com``,
``MAIL_PASSWORD=<16-char-app-password>``). See
``~/.claude/skills/webapp_template/AUTH_MODULE.md`` §3 for the rationale
and how to obtain the app password.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import logging
from typing import Any, Tuple

from flask import current_app
from flask_mail import Mail, Message

log = logging.getLogger(__name__)

# Singleton Mail instance. ``init_mail(app)`` wires it; helpers grab the
# extension lazily via ``current_app`` so they don't need a config arg.
mail = Mail()


def init_mail(app: Any) -> None:
    """Bind the Flask-Mail extension to ``app``.

    Called once from ``app.py:create_app()``. App.config must already
    have the MAIL_* keys set before this is invoked.
    """
    mail.init_app(app)


def _is_configured() -> bool:
    return bool(current_app.config.get("MAIL_USERNAME"))


def _app_name() -> str:
    cfg = current_app.config.get("WEBUI_CONFIG")
    return getattr(cfg, "app_name", "aviti_test_mask")


def _org_name() -> str:
    cfg = current_app.config.get("WEBUI_CONFIG")
    return getattr(cfg, "org_name", "VIB Nucleomics Core")


def _support_email() -> str:
    cfg = current_app.config.get("WEBUI_CONFIG")
    return getattr(cfg, "support_email", "")


def _wrap_html(title: str, body_html: str, accent: str = "#0d6efd") -> str:
    """Bootstrap-free inline-CSS wrapper. Inline because email clients
    strip <style>. Keep colour swaps narrow — only the accent border."""
    return f"""<!doctype html>
<html><body style="font-family:Arial,sans-serif;color:#212529;
max-width:600px;margin:1em auto;padding:1em;border-top:4px solid {accent}">
<h2 style="color:{accent};margin-top:0">{title}</h2>
{body_html}
<hr style="border:none;border-top:1px solid #dee2e6;margin-top:2em">
<p style="color:#6c757d;font-size:0.85em">
{_app_name()} — {_org_name()}
{f"<br>Questions? {_support_email()}" if _support_email() else ""}
</p>
</body></html>"""


def send_email(to: str, subject: str, html_body: str) -> bool:
    """Send a single HTML email. Returns True on success.

    No-op (returns False, logs warning) when ``MAIL_USERNAME`` is unset.
    Logs and returns False on any SMTP/connection failure — callers should
    not let email send failure block the surrounding flow.
    """
    if not _is_configured():
        log.warning("send_email: MAIL_USERNAME unset; skipping send to %s", to)
        return False
    try:
        msg = Message(subject=subject, recipients=[to], html=html_body)
        mail.send(msg)
        log.info("sent email to %s: %s", to, subject)
        return True
    except Exception as exc:  # noqa: BLE001 — log + degrade
        log.error("send_email failed to %s: %s", to, exc)
        return False


# ---- named senders ----

def send_password_reset_email(user: Any, reset_url: str) -> bool:
    """Reset-link email. ``user`` is anything with ``.email`` and
    ``.username`` (a UserRecord, in practice)."""
    body = f"""
<p>Hi {user.username},</p>
<p>You (or someone using your email address) requested a password reset for
your {_app_name()} account.</p>
<p style="margin:1.5em 0">
  <a href="{reset_url}" style="background:#dc3545;color:white;
  padding:0.7em 1.5em;text-decoration:none;border-radius:4px">
  Reset Password</a>
</p>
<p>If the button doesn't work, copy this link into your browser:<br>
<code style="background:#f8f9fa;padding:0.2em 0.4em">{reset_url}</code></p>
<p>This link expires in 1 hour. If you didn't request a reset, ignore
this email — your current password is unchanged.</p>
"""
    return send_email(user.email,
                      f"{_app_name()}: password reset",
                      _wrap_html("Password reset request", body, "#dc3545"))


def send_password_change_confirmation_email(user: Any) -> bool:
    body = f"""
<p>Hi {user.username},</p>
<p>Your password on {_app_name()} was just changed.</p>
<p>If this wasn't you, contact the administrator immediately
{f"at {_support_email()}" if _support_email() else ""}.</p>
"""
    return send_email(user.email,
                      f"{_app_name()}: password changed",
                      _wrap_html("Password changed", body, "#198754"))


def send_welcome_email(user: Any, temporary_password: str) -> bool:
    body = f"""
<p>Hi {user.username},</p>
<p>Your account on {_app_name()} has been created.</p>
<p><strong>Temporary password:</strong>
<code style="background:#f8f9fa;padding:0.2em 0.4em">{temporary_password}</code></p>
<p>You will be prompted to change it on first login.</p>
"""
    return send_email(user.email,
                      f"Welcome to {_app_name()}",
                      _wrap_html(f"Welcome to {_app_name()}", body, "#0d6efd"))


def email_config_status() -> Tuple[bool, str]:
    """Used by a future admin diagnostic page. Returns (ok, message).

    Named ``email_config_status`` (not ``test_email_configuration``) so
    pytest doesn't try to auto-collect it as a test when imported.
    """
    if not _is_configured():
        return False, "MAIL_USERNAME is not set; email delivery disabled"
    sender = current_app.config.get("MAIL_USERNAME")
    server = current_app.config.get("MAIL_SERVER")
    port = current_app.config.get("MAIL_PORT")
    return True, f"configured: {sender} via {server}:{port}"
