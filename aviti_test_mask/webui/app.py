"""Flask application skeleton for the aviti_test_mask web UI.

v1 backend foundation: read-only GET endpoints driving discovery,
configuration, and lookups. Job submission, queue management, and the
HTML form are added in subsequent iterations.
"""
from __future__ import annotations

from dataclasses import asdict
from pathlib import Path

from flask import Flask, jsonify

from config_loader import env_config_path, load
from db import JobsDAO
from discovery import iter_validated, scan_nas_for_runs
from masks_loader import load_builtin_masks
from users_loader import load_users


def create_app() -> Flask:
    cfg = load(env_config_path())
    app = Flask(__name__, static_folder="static", template_folder="templates")
    app.config["WEBUI_CONFIG"] = cfg
    app.config["DAO"] = JobsDAO(cfg.db_path)

    @app.get("/api/v1/health")
    def health():
        nas_ok = cfg.nas_root.exists() and cfg.nas_root.is_dir()
        return jsonify({
            "status": "ok",
            "nas_root": str(cfg.nas_root),
            "nas_mounted": nas_ok,
            "db_path": str(cfg.db_path),
        })

    @app.get("/api/v1/config")
    def get_config():
        public = {
            k: (str(v) if isinstance(v, Path) else v)
            for k, v in cfg.raw.items()
        }
        return jsonify(public)

    @app.get("/api/v1/users")
    def get_users():
        users = load_users(cfg.users_file)
        return jsonify({"users": [asdict(u) for u in users]})

    @app.get("/api/v1/masks")
    def get_masks():
        masks = load_builtin_masks(cfg.masks_file)
        return jsonify({"masks": [
            {"text": m.text, "safe_name": m.safe_name, "source": m.source}
            for m in masks
        ]})

    @app.get("/api/v1/runs")
    def get_runs():
        candidates, warnings = scan_nas_for_runs(cfg)
        return jsonify({
            "runs": [
                {
                    "run_id": c.run_id,
                    "sequencer": c.sequencer,
                    "path": str(c.path),
                    "mtime": c.mtime,
                    "validated": False,
                }
                for c in candidates
            ],
            "count": len(candidates),
            "warnings": warnings,
        })

    @app.get("/api/v1/runs/validated")
    def get_runs_validated():
        valid, invalid = [], []
        for cand in iter_validated(cfg):
            entry = {
                "run_id": cand.run_id,
                "sequencer": cand.sequencer,
                "path": str(cand.path),
                "mtime": cand.mtime,
                "meta": cand.meta,
                "first_failure": cand.first_failure,
            }
            (valid if cand.is_valid else invalid).append(entry)
        return jsonify({
            "valid": valid,
            "invalid": invalid,
            "count_valid": len(valid),
            "count_invalid": len(invalid),
        })

    return app


app = create_app()


if __name__ == "__main__":
    cfg = app.config["WEBUI_CONFIG"]
    app.run(host=cfg.host, port=cfg.port, debug=True)
