"""Flask application skeleton for the aviti_test_mask web UI.

v1 backend foundation: read-only GET endpoints driving discovery,
configuration, and lookups, plus the home + about pages. Job
submission, queue management, and the full form land in subsequent
iterations.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

from dataclasses import asdict
from pathlib import Path

from flask import Flask, jsonify, render_template, request

from config_loader import env_config_path, load
from db import JobsDAO
from discovery import iter_validated, read_run_start, scan_nas_for_runs
from masks_loader import load_builtin_masks
from users_loader import load_users


def create_app() -> Flask:
    cfg = load(env_config_path())
    app = Flask(__name__, static_folder="static", template_folder="templates")
    app.config["WEBUI_CONFIG"] = cfg
    app.config["DAO"] = JobsDAO(cfg.db_path)

    @app.context_processor
    def inject_globals() -> dict:
        return {
            "app_name": cfg.app_name,
            "app_version": cfg.app_version,
            "release_date": cfg.release_date,
            "org_name": cfg.org_name,
            "support_email": cfg.support_email,
        }

    @app.get("/")
    def home():
        return render_template("index.html")

    @app.get("/about")
    def about():
        return render_template("about.html")

    @app.get("/api/v1/health")
    def health():
        nas_ok = cfg.nas_root.exists() and cfg.nas_root.is_dir()
        return jsonify({
            "status": "ok",
            "app_name": cfg.app_name,
            "app_version": cfg.app_version,
            "release_date": cfg.release_date,
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

    def _paginate(items: list, page: int, per_page: int) -> dict:
        total = len(items)
        per_page = max(1, min(per_page, 100))
        last_page = max(1, (total + per_page - 1) // per_page)
        page = max(1, min(page, last_page))
        start = (page - 1) * per_page
        end = start + per_page
        return {
            "page": page,
            "per_page": per_page,
            "total": total,
            "last_page": last_page,
            "has_prev": page > 1,
            "has_next": page < last_page,
            "items": items[start:end],
        }

    def _page_args() -> tuple[int, int]:
        try:
            page = int(request.args.get("page", "1"))
            per_page = int(request.args.get("per_page", "10"))
        except ValueError:
            page, per_page = 1, 10
        return page, per_page

    @app.get("/api/v1/runs")
    def get_runs():
        candidates, warnings = scan_nas_for_runs(cfg)
        page, per_page = _page_args()
        rows = [
            {
                "run_id": c.run_id,
                "sequencer": c.sequencer,
                "path": str(c.path),
                "mtime": c.mtime,
                "run_start": None,   # filled lazily for the page slice only
                "validated": False,
            }
            for c in candidates
        ]
        pag = _paginate(rows, page, per_page)
        # Enrich only the slice the client will see with the precise
        # instrument-reported start timestamp (RunParameters.Date).
        for row in pag["items"]:
            row["run_start"] = read_run_start(Path(row["path"]))
        return jsonify({
            "runs": pag["items"],
            "pagination": {k: v for k, v in pag.items() if k != "items"},
            "warnings": warnings,
        })

    @app.get("/api/v1/runs/validated")
    def get_runs_validated():
        # Validation is expensive — paginate over the discovery list first,
        # validate only the page we're returning, so /runs/validated stays
        # responsive even when 95 candidates are present.
        candidates, warnings = scan_nas_for_runs(cfg)
        page, per_page = _page_args()
        rows = [
            {
                "run_id": c.run_id,
                "sequencer": c.sequencer,
                "path": str(c.path),
                "mtime": c.mtime,
                "run_start": None,
                "_candidate": c,
            }
            for c in candidates
        ]
        pag = _paginate(rows, page, per_page)

        from discovery import validate_run as _validate
        valid, invalid = [], []
        for entry in pag["items"]:
            cand = entry.pop("_candidate")
            result = _validate(cand.path, cfg)
            entry["run_start"] = read_run_start(cand.path)
            entry["meta"] = result["meta"]
            entry["first_failure"] = (
                result["first_failure"]["name"]
                if result["first_failure"] else None
            )
            (valid if result["valid"] else invalid).append(entry)
        return jsonify({
            "valid": valid,
            "invalid": invalid,
            "count_valid": len(valid),
            "count_invalid": len(invalid),
            "pagination": {k: v for k, v in pag.items() if k != "items"},
            "warnings": warnings,
        })

    return app


app = create_app()


if __name__ == "__main__":
    import os
    cfg = app.config["WEBUI_CONFIG"]
    debug = os.environ.get("AVITI_WEBUI_DEBUG") == "1"
    app.run(host=cfg.host, port=cfg.port, debug=debug)
