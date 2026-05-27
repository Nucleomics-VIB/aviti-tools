"""Flask entry point for the aviti_test_mask web UI.

This module is intentionally thin: it loads the WebUIConfig, instantiates
the DAOs, starts the background JobWorker, registers the four
blueprints, and exposes the `app` object Gunicorn / `flask run` /
`python app.py` use.

All HTTP handlers live in ``routes/``; all domain logic lives in
``services/``.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import os
import secrets

from flask import Flask, jsonify

from services.config_loader import env_config_path, load
from services.db import JobsDAO, RunsMetadataDAO
from services.discovery import check_nas_mount
from services.job_lifecycle import IllegalTransition
from services.job_worker import JobWorker
from routes import register_all


def create_app() -> Flask:
    cfg = load(env_config_path())
    app = Flask(__name__, static_folder="static", template_folder="templates")
    # Flash needs a secret. Dev-only: ephemeral key tied to the process.
    # Production deployments set SECRET_KEY in the environment.
    app.secret_key = os.environ.get("SECRET_KEY") or secrets.token_hex(16)
    app.config["WEBUI_CONFIG"] = cfg
    app.config["DAO"] = JobsDAO(cfg.db_path)
    app.config["RUNS_DAO"] = RunsMetadataDAO(cfg.db_path)

    # The background worker is single-instance per process; Flask's
    # debug auto-reloader would spawn a duplicate, hence the env-flag
    # guard (WERKZEUG_RUN_MAIN is set in the reloader child only).
    if os.environ.get("AVITI_DISABLE_WORKER") != "1":
        if not app.debug or os.environ.get("WERKZEUG_RUN_MAIN") == "true":
            worker = JobWorker(cfg, app.config["DAO"])
            worker.start()
            app.config["WORKER"] = worker

    @app.errorhandler(IllegalTransition)
    def _illegal_transition(exc: IllegalTransition):
        # Return 409 Conflict instead of 500 when a route attempts a
        # state mutation the FSM forbids. Routes have their own
        # pre-checks; this is the safety net behind them.
        return jsonify({
            "error": str(exc),
            "current": exc.current,
            "target": exc.target,
        }), 409

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

    register_all(app)
    return app


app = create_app()


if __name__ == "__main__":
    cfg = app.config["WEBUI_CONFIG"]
    debug = os.environ.get("AVITI_WEBUI_DEBUG") == "1"
    app.run(host=cfg.host, port=cfg.port, debug=debug)
