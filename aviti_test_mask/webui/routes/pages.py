"""HTML page routes — home, about, queue, submit (GET + POST), resubmit.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json as _json
import uuid
from pathlib import Path

from flask import (
    Blueprint, abort, current_app, flash, redirect, render_template,
    request, url_for,
)

from services.db import JobRecord, JobsDAO, RunsMetadataDAO, utc_now_iso
from services.discovery import resolve_tile_spec
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


@bp.post("/submit/<run_internal_id>")
def submit_post(run_internal_id: str):
    cfg = _cfg()
    runs_dao: RunsMetadataDAO = current_app.config["RUNS_DAO"]
    dao: JobsDAO = current_app.config["DAO"]
    run = runs_dao.get(run_internal_id)
    if run is None:
        abort(404)

    form = request.form
    submitter = (form.get("submitter") or "").strip()
    if not submitter:
        flash("Submitter is required.", "danger")
        return redirect(url_for("pages.submit_form",
                                run_internal_id=run_internal_id))

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
        return redirect(url_for("pages.submit_form",
                                run_internal_id=run_internal_id))

    tiles_mode = form.get("tiles_mode", "default")
    lanes = form.get("lanes", "all")
    try:
        tile_resolution = resolve_tile_spec(
            Path(run["run_path"]),
            tiles_mode,
            tiles_n=int(form.get("tiles_n") or 3),
            tiles_lane=int(form.get("tiles_lane") or 1),
            tiles_raw=form.get("tiles_raw"),
            lanes=lanes,
        )
    except ValueError as exc:
        flash(f"Tile selection error: {exc}", "danger")
        return redirect(url_for("pages.submit_form",
                                run_internal_id=run_internal_id))
    tiles_spec = tile_resolution["spec"]
    tiles_pattern = tile_resolution["pattern"]
    tiles_picked = tile_resolution["tiles"]

    job_id = (f"{run['run_id']}__{utc_now_iso().replace(':', '-')}"
              f"__{uuid.uuid4().hex[:6]}")
    record = JobRecord(
        job_id=job_id,
        submitter=submitter,
        run_id=run["run_id"],
        run_path=run["run_path"],
        params_json=_json.dumps({
            "lanes": lanes,
            "tiles_mode": tiles_mode,
            "tiles_spec": tiles_spec,
            "tiles_pattern": tiles_pattern,
            "tiles_picked": tiles_picked,
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
    flash(f"Job {job_id} queued.", "success")
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
