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

from flask import Flask, abort, flash, jsonify, redirect, render_template, request, url_for

from config_loader import env_config_path, load
from db import JobsDAO, RunsMetadataDAO
from discovery import (
    is_test_run, iter_validated, read_run_metadata, read_run_start,
    scan_nas_for_runs, validate_run,
)
from masks_loader import load_builtin_masks
from users_loader import load_users


def create_app() -> Flask:
    cfg = load(env_config_path())
    app = Flask(__name__, static_folder="static", template_folder="templates")
    # Flash needs a secret. Dev-only: ephemeral key tied to the process.
    import secrets as _secrets
    app.secret_key = _secrets.token_hex(16)
    app.config["WEBUI_CONFIG"] = cfg
    app.config["DAO"] = JobsDAO(cfg.db_path)
    app.config["RUNS_DAO"] = RunsMetadataDAO(cfg.db_path)

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

    @app.get("/submit/<run_internal_id>")
    def submit_form(run_internal_id: str):
        runs_dao: RunsMetadataDAO = app.config["RUNS_DAO"]
        run = runs_dao.get(run_internal_id)
        if run is None:
            abort(404)
        users = load_users(cfg.users_file)
        masks = load_builtin_masks(cfg.masks_file)
        import json as _json
        lane_projects = _json.loads(run.get("lane_projects_json") or "{}")
        return render_template(
            "submit.html",
            run=run,
            users=users,
            masks=masks,
            lane_projects=lane_projects,
            defaults={
                "threads": cfg.threads,
                "max_inner_jobs": cfg.max_inner_jobs,
                "docker_image": cfg.raw.get("docker_image", "elembio/bases2fastq:latest"),
            },
        )

    @app.post("/submit/<run_internal_id>")
    def submit_post(run_internal_id: str):
        runs_dao: RunsMetadataDAO = app.config["RUNS_DAO"]
        dao: JobsDAO = app.config["DAO"]
        run = runs_dao.get(run_internal_id)
        if run is None:
            abort(404)
        from db import JobRecord, utc_now_iso
        import json as _json
        import uuid

        form = request.form
        submitter = (form.get("submitter") or "").strip()
        if not submitter:
            flash("Submitter is required.", "danger")
            return redirect(url_for("submit_form", run_internal_id=run_internal_id))

        masks_source = form.get("masks_source", "builtin")
        masks_list: list[str] = []
        if masks_source == "builtin":
            masks_list = form.getlist("builtin_masks")
        elif masks_source == "typed":
            t = (form.get("typed_mask") or "").strip()
            if t:
                masks_list = [t]
        # uploaded mode would parse the uploaded file — deferred.

        if not masks_list:
            flash("Pick at least one mask.", "danger")
            return redirect(url_for("submit_form", run_internal_id=run_internal_id))

        tiles_mode = form.get("tiles_mode", "default")
        tiles_spec = ""
        if tiles_mode == "all":
            tiles_spec = "all"
        elif tiles_mode == "lane":
            tiles_spec = f"lane:{form.get('tiles_lane') or '1'}"
        elif tiles_mode == "spread":
            tiles_spec = f"spread:{form.get('tiles_n') or '3'}"
        elif tiles_mode == "random":
            tiles_spec = f"random:{form.get('tiles_n') or '3'}"
        elif tiles_mode == "raw":
            tiles_spec = (form.get("tiles_raw") or "").strip()

        lanes = form.get("lanes", "all")
        lane_projects = _json.loads(run.get("lane_projects_json") or "{}")
        # Future: respect user overrides; for v1 we just persist what's in DB.

        job_id = f"{run['run_id']}__{utc_now_iso().replace(':','-')}__{uuid.uuid4().hex[:6]}"
        record = JobRecord(
            job_id=job_id,
            submitter=submitter,
            run_id=run["run_id"],
            run_path=run["run_path"],
            params_json=_json.dumps({
                "lanes": lanes,
                "tiles_mode": tiles_mode,
                "tiles_spec": tiles_spec,
            }),
            masks_source=masks_source,
            masks_json=_json.dumps(masks_list),
            state="queued",
            cache_input=1 if form.get("cache_input") else 0,
            threads=int(form.get("threads") or cfg.threads),
            max_jobs=int(form.get("max_jobs") or cfg.max_inner_jobs),
            docker_image=form.get("docker_image") or cfg.raw.get("docker_image", ""),
            submitted_at=utc_now_iso(),
            mask_count=len(masks_list),
            tiles_spec=tiles_spec,
            mem_limit_per_job=form.get("mem_limit") or None,
            run_internal_id=run_internal_id,
        )
        dao.insert(record)
        flash(f"Job {job_id} queued. (Worker not yet implemented — row created in DB.)", "success")
        return redirect(url_for("submit_form", run_internal_id=run_internal_id))

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
        # Enrich + upsert metadata only for the slice the client will see.
        # Cheap when the row already exists (one indexed SELECT + UPDATE).
        runs_dao: RunsMetadataDAO = app.config["RUNS_DAO"]
        for row in pag["items"]:
            p = Path(row["path"])
            meta = read_run_metadata(p)
            row["is_test"] = is_test_run(row["run_id"])
            if meta:
                runs_dao.upsert(meta["run_internal_id"], meta["fields"])
                f = meta["fields"]
                row["run_internal_id"] = meta["run_internal_id"]
                row["run_start"] = f.get("run_start")
                row["outcome"] = f.get("outcome")
                row["percent_pf"] = f.get("percent_pf")
                row["run_description"] = f.get("run_description")
                row["operator_name"] = f.get("operator_name")
                row["throughput"] = f.get("throughput")
                row["kit_config"] = f.get("kit_config")
                row["chemistry_version"] = f.get("chemistry_version")
                row["analysis_lanes"] = f.get("analysis_lanes")
                row["total_yield"] = f.get("total_yield")
            else:
                row["run_internal_id"] = None
                row["run_start"] = read_run_start(p)
                for k in ("outcome", "percent_pf", "run_description",
                         "operator_name", "throughput", "kit_config",
                         "chemistry_version", "analysis_lanes", "total_yield"):
                    row[k] = None
            # Validation is the heaviest check per row (BaseCalls listdir +
            # stat per zip); cap it here so we only walk once per page.
            v = validate_run(p, cfg)
            row["valid"] = v["valid"]
            row["first_failure"] = (
                v["first_failure"]["name"] if v["first_failure"] else None
            )
        return jsonify({
            "runs": pag["items"],
            "pagination": {k: v for k, v in pag.items() if k != "items"},
            "warnings": warnings,
        })

    @app.get("/api/v1/runs/<run_internal_id>")
    def get_run_detail(run_internal_id: str):
        runs_dao: RunsMetadataDAO = app.config["RUNS_DAO"]
        row = runs_dao.get(run_internal_id)
        if row is None:
            return jsonify({"error": "unknown run"}), 404
        # Re-read from disk so an in-progress run that has since
        # finished doesn't serve a stale "incomplete" snapshot. Cheap
        # when the row already exists; on disk error we fall back to
        # the cached row.
        try:
            disk_path = Path(row["run_path"])
            if disk_path.exists():
                meta = read_run_metadata(disk_path)
                if meta and meta["run_internal_id"] == run_internal_id:
                    runs_dao.upsert(run_internal_id, meta["fields"])
                    row = runs_dao.get(run_internal_id) or row
        except OSError:
            pass
        return jsonify(row)

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
