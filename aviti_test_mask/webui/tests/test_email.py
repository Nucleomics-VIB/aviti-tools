"""Tests for services/email.py.

We don't actually open an SMTP connection: tests use Flask-Mail's
``mail.record_messages()`` context manager which captures Message objects
in-memory instead of sending them.
"""
from __future__ import annotations

from types import SimpleNamespace

import pytest
from flask import Flask

from services import email as email_mod
from services.email import (
    init_mail,
    mail,
    send_email,
    send_password_change_confirmation_email,
    send_password_reset_email,
    send_welcome_email,
    email_config_status,
)


def _make_app(*, mail_username: str | None = "bot@gmail.com") -> Flask:
    app = Flask(__name__)
    app.config["WEBUI_CONFIG"] = SimpleNamespace(
        app_name="aviti_test_mask",
        org_name="VIB Nucleomics Core",
        support_email="nucleomics@vib.be",
    )
    app.config["MAIL_SERVER"] = "smtp.gmail.com"
    app.config["MAIL_PORT"] = 587
    app.config["MAIL_USE_TLS"] = True
    app.config["MAIL_USERNAME"] = mail_username
    app.config["MAIL_PASSWORD"] = "secret" if mail_username else None
    app.config["MAIL_DEFAULT_SENDER"] = mail_username
    app.config["MAIL_SUPPRESS_SEND"] = True   # do not actually contact SMTP
    init_mail(app)
    return app


def test_send_email_returns_true_when_configured():
    app = _make_app()
    with app.app_context():
        with mail.record_messages() as outbox:
            ok = send_email("user@example.com", "Hi", "<p>body</p>")
            assert ok is True
            assert len(outbox) == 1
            msg = outbox[0]
            assert msg.recipients == ["user@example.com"]
            assert msg.subject == "Hi"
            assert "<p>body</p>" in msg.html


def test_send_email_noop_when_username_unset():
    app = _make_app(mail_username=None)
    with app.app_context():
        with mail.record_messages() as outbox:
            ok = send_email("user@example.com", "Hi", "<p>body</p>")
            assert ok is False
            assert outbox == []


def test_send_email_returns_false_on_smtp_error(monkeypatch):
    app = _make_app()

    def boom(self, msg):
        raise RuntimeError("smtp boom")

    with app.app_context():
        monkeypatch.setattr(mail.__class__, "send", boom)
        assert send_email("user@example.com", "Hi", "<p>x</p>") is False


def test_reset_email_includes_url_and_username():
    app = _make_app()
    user = SimpleNamespace(username="splaisan", email="s@vib.be")
    with app.app_context():
        with mail.record_messages() as outbox:
            ok = send_password_reset_email(
                user, "https://example.org/reset/tok-abc")
            assert ok is True
            html = outbox[0].html
            assert "splaisan" in html
            assert "https://example.org/reset/tok-abc" in html
            # Org branding makes it into the footer.
            assert "VIB Nucleomics Core" in html
            assert outbox[0].recipients == ["s@vib.be"]
            assert "password reset" in outbox[0].subject.lower()


def test_password_change_confirmation_sent():
    app = _make_app()
    user = SimpleNamespace(username="alice", email="alice@example.com")
    with app.app_context():
        with mail.record_messages() as outbox:
            ok = send_password_change_confirmation_email(user)
            assert ok is True
            assert outbox[0].recipients == ["alice@example.com"]
            assert "password changed" in outbox[0].subject.lower()
            assert "alice" in outbox[0].html


def test_welcome_email_includes_temporary_password():
    app = _make_app()
    user = SimpleNamespace(username="newbie", email="n@example.com")
    with app.app_context():
        with mail.record_messages() as outbox:
            send_welcome_email(user, temporary_password="t3mp-p4ss")
            assert "t3mp-p4ss" in outbox[0].html


def test_email_config_status_reports_status():
    app = _make_app(mail_username=None)
    with app.app_context():
        ok, msg = email_config_status()
        assert ok is False
        assert "MAIL_USERNAME" in msg

    app = _make_app(mail_username="bot@gmail.com")
    with app.app_context():
        ok, msg = email_config_status()
        assert ok is True
        assert "bot@gmail.com" in msg
        assert "smtp.gmail.com:587" in msg
