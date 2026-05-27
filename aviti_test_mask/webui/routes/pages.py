"""HTML page routes — home, about, queue, submit (GET + POST), resubmit.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json as _json

from flask import (
    Blueprint, abort, current_app, flash, redirect, render_template,
    request, send_from_directory, url_for,
)

from services.db import JobsDAO, RunsMetadataDAO
from services.job_submission import submit_job
from services.masks_loader import load_builtin_masks
from services.users_loader import load_users

bp = Blueprint("pages", __name__)


def _cfg():
    return current_app.config["WEBUI_CONFIG"]


@bp.get("/")
def home():
    return render_template("index.html")


@bp.get("/about")
def about():
    return render_template("about.html")


@bp.get("/queue")
def queue_page():
    return render_template("queue.html")


@bp.get("/history")
def history_page():
    return render_template("history.html")


@bp.get("/results/<job_id>")
def results_page(job_id: str):
    return render_template("results.html", job_id=job_id)


@bp.get("/results/<job_id>/files/<safe_mask>/<path:filename>")
def mask_file(job_id: str, safe_mask: str, filename: str):
    """Serve a file under results/<job_id>/qc_runs/<safe_mask>/.

    Path safety: rejects traversal in ``safe_mask`` and ``filename``;
    confirms the resolved file resides inside the mask folder.
    """
    if not safe_mask or "/" in safe_mask or "\\" in safe_mask \
            or safe_mask in ("..", "."):
        abort(404)
    cfg = _cfg()
    qc_root = (cfg.results_root / job_id / "qc_runs").resolve()
    if not qc_root.is_dir():
        abort(404)
    mask_root = (qc_root / safe_mask).resolve()
    try:
        mask_root.relative_to(qc_root)
    except ValueError:
        abort(404)
    if not mask_root.is_dir():
        abort(404)
    target = (mask_root / filename).resolve()
    try:
        target.relative_to(mask_root)
    except ValueError:
        abort(404)
    if not target.is_file():
        abort(404)
    return send_from_directory(mask_root, filename)


@bp.get("/settings")
def settings_page():
    return render_template("settings.html")


@bp.get("/monitor")
def monitor_page():
    return render_template("monitor.html")


@bp.get("/submit/<run_internal_id>")
def submit_form(run_internal_id: str):
    cfg = _cfg()
    runs_dao: RunsMetadataDAO = current_app.config["RUNS_DAO"]
    run = runs_dao.get(run_internal_id)
    if run is None:
        abort(404)
    users = load_users(cfg.users_file)
    masks = load_builtin_masks(cfg.masks_file)
    lane_projects = _json.loads(run.get("lane_projects_json") or "{}")

    clone_id = request.args.get("clone")
    clone: dict | None = None
    if clone_id:
        jobs_dao: JobsDAO = current_app.config["DAO"]
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
            "docker_image": cfg.raw.get(
                "docker_image", "elembio/bases2fastq:latest"),
        },
    )


def _pick_masks(form) -> list[str]:
    """Extract the mask list from the submit form's multi-input shape."""
    masks_source = form.get("masks_source", "builtin")
    if masks_source == "builtin":
        return form.getlist("builtin_masks")
    if masks_source == "typed":
        t = (form.get("typed_mask") or "").strip()
        return [t] if t else []
    # uploaded mode would parse the uploaded file — deferred.
    return []


def _form_int(form, key: str, default: int) -> int:
    raw = form.get(key)
    if not raw:
        return default
    try:
        return int(raw)
    except ValueError:
        return default


@bp.post("/submit/<run_internal_id>")
def submit_post(run_internal_id: str):
    runs_dao: RunsMetadataDAO = current_app.config["RUNS_DAO"]
    dao: JobsDAO = current_app.config["DAO"]
    run = runs_dao.get(run_internal_id)
    if run is None:
        abort(404)
    run = {**run, "run_internal_id": run_internal_id}

    form = request.form
    result = submit_job(
        _cfg(), dao, run,
        submitter=form.get("submitter", ""),
        masks_source=form.get("masks_source", "builtin"),
        masks_list=_pick_masks(form),
        tiles_mode=form.get("tiles_mode", "default"),
        tiles_n=_form_int(form, "tiles_n", 3),
        tiles_lane=_form_int(form, "tiles_lane", 1),
        tiles_raw=form.get("tiles_raw"),
        lanes=form.get("lanes", "all"),
        threads=_form_int(form, "threads", _cfg().threads),
        max_jobs=_form_int(form, "max_jobs", _cfg().max_inner_jobs),
        docker_image=form.get("docker_image") or None,
        mem_limit=form.get("mem_limit") or None,
        cache_input=bool(form.get("cache_input")),
    )
    if result.ok:
        flash(f"Job {result.job_id} queued.", "success")
    else:
        flash(result.error, "danger")
    return redirect(url_for("pages.submit_form",
                            run_internal_id=run_internal_id))


@bp.get("/resubmit/<job_id>")
def resubmit(job_id: str):
    dao: JobsDAO = current_app.config["DAO"]
    row = dao.get(job_id)
    if row is None:
        abort(404)
    if not row.get("run_internal_id"):
        flash("Original job has no linked run — cannot re-submit.", "danger")
        return redirect(url_for("pages.queue_page"))
    return redirect(url_for("pages.submit_form",
                            run_internal_id=row["run_internal_id"],
                            clone=job_id))
