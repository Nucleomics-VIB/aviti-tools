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

        # Optional pre-fill from an existing job (Re-submit / clone).
        clone_id = request.args.get("clone")
        clone: dict | None = None
        if clone_id:
            jobs_dao: JobsDAO = app.config["DAO"]
            src = jobs_dao.get(clone_id)
            if src:
                try:
                    params = _json.loads(src.get("params_json") or "{}")
                except _json.JSONDecodeError:
                    params = {}
                try:
                    masks_list = _json.loads(src.get("masks_json") or "[]")
                except _json.JSONDecodeError:
                    masks_list = []
                clone = {
                    "submitter": src.get("submitter"),
                    "masks_source": src.get("masks_source"),
                    "masks": masks_list,
                    "tiles_mode": params.get("tiles_mode", "default"),
                    "tiles_spec": src.get("tiles_spec") or "",
                    "lanes": params.get("lanes", "all"),
                    "threads": src.get("threads"),
                    "max_jobs": src.get("max_jobs"),
                    "cache_input": src.get("cache_input"),
                    "mem_limit_per_job": src.get("mem_limit_per_job"),
                    "docker_image": src.get("docker_image"),
                }

        return render_template(
            "submit.html",
            run=run,
            users=users,
            masks=masks,
            lane_projects=lane_projects,
            clone=clone,
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

    @app.get("/queue")
    def queue_page():
        return render_template("queue.html")

    def _sorted_queue_jobs() -> list[dict]:
        """Active jobs in display order — running first, then queue order."""
        dao: JobsDAO = app.config["DAO"]
        rows, _ = dao.list(
            states=["queued", "paused", "running", "stopping", "integrating"],
            limit=500,
            order_by="submitted_at ASC",
        )
        # Active jobs (running/integrating/stopping) go on top, with no
        # position number. The remaining queued+paused rows keep their
        # submission order — Move up/down + Start now rewrite submitted_at
        # to reorder, so this stays as the canonical ordering.
        active_states = {"running", "integrating", "stopping"}
        active = [r for r in rows if r["state"] in active_states]
        waiting = [r for r in rows if r["state"] not in active_states]
        ordered: list[dict] = []
        for r in active:
            r["queue_position"] = None
            ordered.append(r)
        for i, r in enumerate(waiting, start=1):
            r["queue_position"] = i
            ordered.append(r)
        return ordered

    @app.get("/api/v1/queue")
    def get_queue():
        jobs = _sorted_queue_jobs()
        return jsonify({"jobs": jobs, "total": len(jobs)})

    @app.delete("/api/v1/jobs/<job_id>")
    def delete_job(job_id: str):
        dao: JobsDAO = app.config["DAO"]
        row = dao.get(job_id)
        if row is None:
            return jsonify({"error": "unknown job"}), 404
        if row["state"] in ("done", "failed", "cancelled", "deleted"):
            return jsonify({"error": f"cannot delete {row['state']!r} job"}), 409
        if row["state"] == "queued":
            from db import utc_now_iso
            dao.update(job_id, state="cancelled",
                       cancelled_by=row["submitter"],
                       finished_at=utc_now_iso(),
                       error_message="cancelled before start")
        else:
            # Running / stopping / integrating — graceful drain (no worker yet,
            # so we just mark state for now).
            dao.update(job_id, state="stopping", cancelled_by=row["submitter"])
        return jsonify({"ok": True})

    @app.post("/api/v1/jobs/<job_id>/pause")
    def pause_job(job_id: str):
        dao: JobsDAO = app.config["DAO"]
        row = dao.get(job_id)
        if row is None:
            return jsonify({"error": "unknown job"}), 404
        if row["state"] != "queued":
            return jsonify({"error": f"can only pause queued jobs (state={row['state']})"}), 409
        dao.update(job_id, state="paused")
        return jsonify({"ok": True})

    @app.post("/api/v1/jobs/<job_id>/resume")
    def resume_job(job_id: str):
        dao: JobsDAO = app.config["DAO"]
        row = dao.get(job_id)
        if row is None:
            return jsonify({"error": "unknown job"}), 404
        if row["state"] != "paused":
            return jsonify({"error": f"can only resume paused jobs (state={row['state']})"}), 409
        dao.update(job_id, state="queued")
        return jsonify({"ok": True})

    def _adjust_submitted_at(job_id: str, ref_time: str | None) -> None:
        """Rewrite a job's submitted_at to control queue ordering.

        Move semantics use submitted_at as the canonical ordering key, so
        promoting a job to the front means setting its submitted_at to
        just before the current first entry. We bump to one microsecond
        earlier to keep the ISO-8601 string monotonic vs the neighbour.
        """
        dao: JobsDAO = app.config["DAO"]
        if ref_time is None:
            return
        dao.update(job_id, started_at=None)  # no-op, but keeps the noqa happy
        # Direct field assignment via update():
        from db import utc_now_iso  # noqa: F401  (kept for readability)
        # Actually do the update:
        with dao._connect() as conn:  # type: ignore[attr-defined]
            conn.execute("UPDATE jobs SET submitted_at=? WHERE job_id=?",
                         (ref_time, job_id))

    @app.post("/api/v1/jobs/<job_id>/start_now")
    def start_now(job_id: str):
        dao: JobsDAO = app.config["DAO"]
        row = dao.get(job_id)
        if row is None:
            return jsonify({"error": "unknown job"}), 404
        if row["state"] not in ("queued", "paused"):
            return jsonify({"error": f"cannot promote state={row['state']}"}), 409
        # Find the earliest submitted_at among queued/paused and set
        # this job to one second before it. If it's already first, no-op.
        jobs = _sorted_queue_jobs()
        waiting = [j for j in jobs if j["state"] in ("queued", "paused")
                   and j["job_id"] != job_id]
        if not waiting:
            return jsonify({"ok": True, "note": "already alone in queue"})
        from datetime import datetime, timedelta
        earliest = waiting[0]["submitted_at"]
        new_ts = (datetime.strptime(earliest, "%Y-%m-%dT%H:%M:%SZ")
                  - timedelta(seconds=1)).strftime("%Y-%m-%dT%H:%M:%SZ")
        with dao._connect() as conn:  # type: ignore[attr-defined]
            conn.execute("UPDATE jobs SET submitted_at=? WHERE job_id=?",
                         (new_ts, job_id))
        # Resume if it was paused so it actually leaves the queue when a
        # worker picks up the front.
        if row["state"] == "paused":
            dao.update(job_id, state="queued")
        return jsonify({"ok": True, "new_submitted_at": new_ts})

    @app.post("/api/v1/jobs/<job_id>/move")
    def move_job(job_id: str):
        try:
            delta = int(request.args.get("delta", "0"))
        except ValueError:
            return jsonify({"error": "delta must be ±1"}), 400
        if delta not in (-1, 1):
            return jsonify({"error": "delta must be ±1"}), 400
        dao: JobsDAO = app.config["DAO"]
        row = dao.get(job_id)
        if row is None:
            return jsonify({"error": "unknown job"}), 404
        if row["state"] not in ("queued", "paused"):
            return jsonify({"error": f"cannot move state={row['state']}"}), 409
        jobs = _sorted_queue_jobs()
        waiting = [j for j in jobs if j["state"] in ("queued", "paused")]
        idx = next((i for i, j in enumerate(waiting) if j["job_id"] == job_id), -1)
        if idx < 0:
            return jsonify({"error": "job not in waiting set"}), 409
        target = idx + delta
        if target < 0 or target >= len(waiting):
            return jsonify({"error": "out of range"}), 409
        # Swap submitted_at with the neighbour.
        neighbour = waiting[target]
        a_ts, b_ts = row["submitted_at"], neighbour["submitted_at"]
        # If neighbour's submitted_at equals row's (rare second-precision
        # collision), shift one by a second to break the tie.
        if a_ts == b_ts:
            from datetime import datetime, timedelta
            shifted = (datetime.strptime(a_ts, "%Y-%m-%dT%H:%M:%SZ")
                       + timedelta(seconds=delta)).strftime("%Y-%m-%dT%H:%M:%SZ")
            a_ts = shifted
        with dao._connect() as conn:  # type: ignore[attr-defined]
            conn.execute("UPDATE jobs SET submitted_at=? WHERE job_id=?",
                         (b_ts, job_id))
            conn.execute("UPDATE jobs SET submitted_at=? WHERE job_id=?",
                         (a_ts, neighbour["job_id"]))
        return jsonify({"ok": True})

    @app.get("/resubmit/<job_id>")
    def resubmit(job_id: str):
        dao: JobsDAO = app.config["DAO"]
        row = dao.get(job_id)
        if row is None:
            abort(404)
        if not row.get("run_internal_id"):
            flash("Original job has no linked run — cannot re-submit.", "danger")
            return redirect(url_for("queue_page"))
        return redirect(url_for("submit_form",
                                run_internal_id=row["run_internal_id"],
                                clone=job_id))

    @app.post("/api/v1/queue/clear")
    def clear_queue():
        if request.args.get("confirm") != "CLEAR":
            return jsonify({"error": "missing confirm=CLEAR"}), 400
        dao: JobsDAO = app.config["DAO"]
        # Snapshot first so we can report a count.
        rows, _ = dao.list(states=["queued"], limit=10000, order_by="submitted_at ASC")
        from db import utc_now_iso
        now = utc_now_iso()
        for r in rows:
            dao.update(r["job_id"], state="cancelled",
                       cancelled_by=r["submitter"],
                       finished_at=now,
                       error_message="cleared via /queue")
        return jsonify({"deleted": len(rows)})

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
