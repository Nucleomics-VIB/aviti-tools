"""Flask entry point for the aviti_test_mask web UI.

This module is intentionally thin: it loads the WebUIConfig, instantiates
the DAOs, starts the background JobWorker, wires session-auth + email,
registers the blueprints, and exposes the `app` object Gunicorn /
`flask run` / `python app.py` use.

All HTTP handlers live in ``routes/``; all domain logic lives in
``services/``.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import logging
import os
import secrets
from datetime import timedelta
from pathlib import Path

from flask import Flask, jsonify
from flask_session import Session

from services.config_loader import env_config_path, load
from services.db import JobsDAO, RunsMetadataDAO
from services.discovery import check_nas_mount
from services.email import init_mail
from services.job_lifecycle import IllegalTransition
from services.job_worker import JobWorker
from services.users_dao import UsersDAO
from services.users_loader import load_users, seed_users_table
from routes import register_all
from routes.auth import auth_bp

log = logging.getLogger(__name__)


def _resolve_secret_key(cfg) -> str:
    """env SECRET_KEY → webui_config.yaml:secret_key →
    <results_root>/.secret_key file → freshly generated (and persisted).

    The persistence step at the end means the dev-server's auto-reload
    doesn't log everyone out — without it, every reload gets a new
    process-local key and every existing session cookie becomes invalid.
    """
    if key := os.environ.get("SECRET_KEY"):
        return key
    if key := cfg.raw.get("secret_key"):
        return str(key)
    key_file = Path(cfg.results_root) / ".secret_key"
    if key_file.exists():
        return key_file.read_text().strip()
    key = secrets.token_hex(32)
    try:
        key_file.parent.mkdir(parents=True, exist_ok=True)
        key_file.write_text(key)
        key_file.chmod(0o600)
    except OSError as exc:
        log.warning("could not persist secret key to %s: %s — sessions "
                    "will be invalidated on next restart", key_file, exc)
    return key


def create_app() -> Flask:
    cfg = load(env_config_path())
    app = Flask(__name__, static_folder="static", template_folder="templates")

    # ---- Session + secret key (must precede any session usage) ----
    app.config["SECRET_KEY"] = _resolve_secret_key(cfg)
    app.config["PERMANENT_SESSION_LIFETIME"] = timedelta(
        hours=cfg.session_lifetime_hours)
    app.config["SESSION_COOKIE_HTTPONLY"] = True
    app.config["SESSION_COOKIE_SAMESITE"] = "Lax"
    # Honour env override for HTTPS deployments; default from YAML.
    app.config["SESSION_COOKIE_SECURE"] = (
        os.environ.get("SESSION_COOKIE_SECURE", "").lower() == "true"
        or bool(cfg.cookie_secure)
    )
    app.config["SESSION_TYPE"] = "filesystem"
    # /tmp on the container is tmpfs (declared in docker-compose.yml) →
    # sessions are ephemeral on restart, which is fine for an internal
    # tool and avoids a bind mount. On Mac dev /tmp is also fine.
    app.config["SESSION_FILE_DIR"] = os.environ.get(
        "AVITI_SESSION_DIR", "/tmp/aviti_sessions")
    Session(app)

    # ---- Email ----
    app.config["MAIL_SERVER"] = cfg.smtp_server
    app.config["MAIL_PORT"] = cfg.smtp_port
    app.config["MAIL_USE_TLS"] = cfg.use_tls
    app.config["MAIL_USERNAME"] = os.environ.get("MAIL_USERNAME")
    app.config["MAIL_PASSWORD"] = os.environ.get("MAIL_PASSWORD")
    app.config["MAIL_DEFAULT_SENDER"] = os.environ.get("MAIL_USERNAME")
    init_mail(app)

    # ---- Config + DAOs ----
    app.config["WEBUI_CONFIG"] = cfg
    app.config["DAO"] = JobsDAO(cfg.db_path)         # also runs schema migration
    app.config["RUNS_DAO"] = RunsMetadataDAO(cfg.db_path)
    app.config["USERS_DAO"] = UsersDAO(cfg.db_path)

    # ---- Seed users from users.yaml (idempotent) ----
    try:
        yaml_users = load_users(cfg.users_file)
        inserted = seed_users_table(yaml_users, app.config["USERS_DAO"])
        if inserted:
            log.info("seeded %d user(s) from %s", inserted, cfg.users_file)
    except Exception as exc:  # noqa: BLE001 — fail loudly but stay up
        log.error("user seed failed (continuing without seed): %s", exc)

    # ---- Background worker (single-instance, no auto-reloader duplicate) ----
    if os.environ.get("AVITI_DISABLE_WORKER") != "1":
        if not app.debug or os.environ.get("WERKZEUG_RUN_MAIN") == "true":
            worker = JobWorker(cfg, app.config["DAO"])
            worker.start()
            app.config["WORKER"] = worker

    # ---- Error handlers ----
    @app.errorhandler(IllegalTransition)
    def _illegal_transition(exc: IllegalTransition):
        return jsonify({
            "error": str(exc),
            "current": exc.current,
            "target": exc.target,
        }), 409

    # ---- Template globals ----
    @app.context_processor
    def inject_globals() -> dict:
        return {
            "app_name": cfg.app_name,
            "app_version": cfg.app_version,
            "release_date": cfg.release_date,
            "org_name": cfg.org_name,
            "support_email": cfg.support_email,
            "nas_check": check_nas_mount(cfg),
        }

    # ---- Blueprints (auth first so /login is reachable when others 401) ----
    app.register_blueprint(auth_bp)
    register_all(app)
    return app


app = create_app()


if __name__ == "__main__":
    cfg = app.config["WEBUI_CONFIG"]
    debug = os.environ.get("AVITI_WEBUI_DEBUG") == "1"
    app.run(host=cfg.host, port=cfg.port, debug=debug)
