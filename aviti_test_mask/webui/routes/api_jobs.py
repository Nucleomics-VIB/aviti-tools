"""Jobs API — /api/v1/queue, /api/v1/jobs/<id>, queue actions.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json as _json
from datetime import datetime, timedelta
from pathlib import Path

from flask import Blueprint, current_app, jsonify, request

from services.db import JobsDAO, utc_now_iso

bp = Blueprint("api_jobs", __name__, url_prefix="/api/v1")


def _cfg():
    return current_app.config["WEBUI_CONFIG"]


def _dao() -> JobsDAO:
    return current_app.config["DAO"]


# ── Queue listing ────────────────────────────────────────────────────


def _sorted_queue_jobs() -> list[dict]:
    """Active jobs in display order — running first, then queue order."""
    dao = _dao()
    rows, _ = dao.list(
        states=["queued", "paused", "running", "stopping", "integrating"],
        limit=500,
        order_by="submitted_at ASC",
    )
    for r in rows:
        try:
            p = _json.loads(r.get("params_json") or "{}")
        except _json.JSONDecodeError:
            p = {}
        r["lanes"] = p.get("lanes")
        r["tiles_mode"] = p.get("tiles_mode")
        r["tiles_pattern"] = p.get("tiles_pattern")
        r["tiles_picked"] = p.get("tiles_picked")
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


@bp.get("/queue")
def get_queue():
    jobs = _sorted_queue_jobs()
    return jsonify({"jobs": jobs, "total": len(jobs)})


@bp.get("/jobs")
def list_jobs():
    """Paginated jobs listing for the History page.

    Query params: page, per_page, state (repeatable), submitter,
    since (ISO8601).
    """
    try:
        page = max(1, int(request.args.get("page", "1")))
        per_page = max(1, min(int(request.args.get("per_page", "20")), 100))
    except ValueError:
        page, per_page = 1, 20
    states = request.args.getlist("state") or None
    submitter = request.args.get("submitter") or None
    since = request.args.get("since") or None
    rows, total = _dao().list(
        states=states, submitter=submitter, since=since,
        limit=per_page, offset=(page - 1) * per_page,
        order_by="submitted_at DESC",
    )
    last_page = max(1, (total + per_page - 1) // per_page)
    return jsonify({
        "jobs": rows,
        "pagination": {
            "page": page, "per_page": per_page, "total": total,
            "last_page": last_page,
            "has_prev": page > 1, "has_next": page < last_page,
        },
    })


@bp.get("/jobs/recent_failures")
def get_recent_failures():
    dao = _dao()
    since = (datetime.utcnow() - timedelta(hours=24)).strftime(
        "%Y-%m-%dT%H:%M:%SZ")
    rows, _ = dao.list(states=["failed", "cancelled"],
                        since=since, limit=20,
                        order_by="submitted_at DESC")
    return jsonify({"jobs": rows, "total": len(rows)})


@bp.get("/jobs/<job_id>/results")
def get_job_results(job_id: str):
    dao = _dao()
    row = dao.get(job_id)
    if row is None:
        return jsonify({"error": "unknown job"}), 404
    with dao._connect() as conn:  # type: ignore[attr-defined]
        cur = conn.execute(
            "SELECT mask, lane, project, status, q30_pct, assigned_pct, "
            "score, source, error_msg "
            "FROM mask_results WHERE job_id=:j "
            "ORDER BY (score IS NULL), score DESC, mask ASC",
            {"j": job_id})
        results = [dict(r) for r in cur.fetchall()]
    return jsonify({
        "job": {
            "job_id": row["job_id"], "run_id": row["run_id"],
            "submitter": row["submitter"], "state": row["state"],
            "submitted_at": row["submitted_at"],
            "finished_at": row["finished_at"],
            "duration_seconds": row["duration_seconds"],
            "best_mask": row["best_mask"], "best_score": row["best_score"],
            "mask_count": row["mask_count"],
        },
        "results": results,
        "total": len(results),
    })


@bp.get("/jobs/<job_id>/log")
def get_job_log(job_id: str):
    cfg = _cfg()
    dao = _dao()
    row = dao.get(job_id)
    if row is None:
        return jsonify({"error": "unknown job"}), 404
    which = (request.args.get("file") or "run").lower()
    if which not in ("run", "integrate"):
        return jsonify({"error": "file must be 'run' or 'integrate'"}), 400
    fname = "integrate.log" if which == "integrate" else "run.log"
    log_path = cfg.results_root / job_id / fname
    if not log_path.exists():
        return jsonify({"error": f"no {fname} yet", "log": ""}), 200
    try:
        size = log_path.stat().st_size
        with log_path.open("rb") as fh:
            if size > 64 * 1024:
                fh.seek(-64 * 1024, 2)
            content = fh.read().decode("utf-8", errors="replace")
    except OSError as exc:
        return jsonify({"error": str(exc)}), 500
    return jsonify({"log": content, "size_bytes": size,
                    "state": row["state"]})


# ── Actions ──────────────────────────────────────────────────────────


@bp.delete("/jobs/<job_id>")
def delete_job(job_id: str):
    dao = _dao()
    row = dao.get(job_id)
    if row is None:
        return jsonify({"error": "unknown job"}), 404
    if row["state"] in ("done", "failed", "cancelled", "deleted"):
        return jsonify(
            {"error": f"cannot delete {row['state']!r} job"}), 409
    if row["state"] == "queued":
        dao.update(job_id, state="cancelled",
                   cancelled_by=row["submitter"],
                   finished_at=utc_now_iso(),
                   error_message="cancelled before start")
    else:
        dao.update(job_id, state="stopping",
                   cancelled_by=row["submitter"])
    return jsonify({"ok": True})


@bp.post("/jobs/<job_id>/pause")
def pause_job(job_id: str):
    dao = _dao()
    row = dao.get(job_id)
    if row is None:
        return jsonify({"error": "unknown job"}), 404
    if row["state"] != "queued":
        return jsonify(
            {"error": f"can only pause queued jobs (state={row['state']})"}), 409
    dao.update(job_id, state="paused")
    return jsonify({"ok": True})


@bp.post("/jobs/<job_id>/resume")
def resume_job(job_id: str):
    dao = _dao()
    row = dao.get(job_id)
    if row is None:
        return jsonify({"error": "unknown job"}), 404
    if row["state"] != "paused":
        return jsonify(
            {"error": f"can only resume paused jobs (state={row['state']})"}), 409
    dao.update(job_id, state="queued")
    return jsonify({"ok": True})


@bp.post("/jobs/<job_id>/start_now")
def start_now(job_id: str):
    dao = _dao()
    row = dao.get(job_id)
    if row is None:
        return jsonify({"error": "unknown job"}), 404
    if row["state"] not in ("queued", "paused"):
        return jsonify({"error": f"cannot promote state={row['state']}"}), 409
    jobs = _sorted_queue_jobs()
    waiting = [j for j in jobs
               if j["state"] in ("queued", "paused")
               and j["job_id"] != job_id]
    if not waiting:
        return jsonify({"ok": True, "note": "already alone in queue"})
    earliest = waiting[0]["submitted_at"]
    new_ts = (datetime.strptime(earliest, "%Y-%m-%dT%H:%M:%SZ")
              - timedelta(seconds=1)).strftime("%Y-%m-%dT%H:%M:%SZ")
    with dao._connect() as conn:  # type: ignore[attr-defined]
        conn.execute("UPDATE jobs SET submitted_at=? WHERE job_id=?",
                     (new_ts, job_id))
    if row["state"] == "paused":
        dao.update(job_id, state="queued")
    return jsonify({"ok": True, "new_submitted_at": new_ts})


@bp.post("/jobs/<job_id>/move")
def move_job(job_id: str):
    try:
        delta = int(request.args.get("delta", "0"))
    except ValueError:
        return jsonify({"error": "delta must be ±1"}), 400
    if delta not in (-1, 1):
        return jsonify({"error": "delta must be ±1"}), 400
    dao = _dao()
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
    neighbour = waiting[target]
    a_ts, b_ts = row["submitted_at"], neighbour["submitted_at"]
    if a_ts == b_ts:
        shifted = (datetime.strptime(a_ts, "%Y-%m-%dT%H:%M:%SZ")
                   + timedelta(seconds=delta)).strftime("%Y-%m-%dT%H:%M:%SZ")
        a_ts = shifted
    with dao._connect() as conn:  # type: ignore[attr-defined]
        conn.execute("UPDATE jobs SET submitted_at=? WHERE job_id=?",
                     (b_ts, job_id))
        conn.execute("UPDATE jobs SET submitted_at=? WHERE job_id=?",
                     (a_ts, neighbour["job_id"]))
    return jsonify({"ok": True})


@bp.post("/queue/clear")
def clear_queue():
    if request.args.get("confirm") != "CLEAR":
        return jsonify({"error": "missing confirm=CLEAR"}), 400
    dao = _dao()
    rows, _ = dao.list(states=["queued"], limit=10000,
                        order_by="submitted_at ASC")
    now = utc_now_iso()
    for r in rows:
        dao.update(r["job_id"], state="cancelled",
                   cancelled_by=r["submitter"],
                   finished_at=now,
                   error_message="cleared via /queue")
    return jsonify({"deleted": len(rows)})


@bp.post("/jobs/<job_id>/dismiss")
def dismiss_job(job_id: str):
    dao = _dao()
    row = dao.get(job_id)
    if row is None:
        return jsonify({"error": "unknown job"}), 404
    if row["state"] not in ("done", "failed", "cancelled"):
        return jsonify(
            {"error": f"can only dismiss terminal jobs (state={row['state']})"}), 409
    dao.soft_delete(job_id)
    return jsonify({"ok": True})


@bp.post("/failures/clear")
def clear_failures():
    dao = _dao()
    since = (datetime.utcnow() - timedelta(hours=24)).strftime(
        "%Y-%m-%dT%H:%M:%SZ")
    rows, _ = dao.list(states=["failed", "cancelled"],
                        since=since, limit=10000)
    for r in rows:
        dao.soft_delete(r["job_id"])
    return jsonify({"deleted": len(rows)})
