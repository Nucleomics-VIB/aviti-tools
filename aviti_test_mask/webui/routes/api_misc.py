"""Misc API endpoints — /health, /config, /users, /masks, /monitor.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import subprocess as _sp
from dataclasses import asdict
from datetime import datetime, timedelta
from pathlib import Path

from flask import Blueprint, current_app, jsonify, request, session

from services.db import JobsDAO
from services.discovery import check_nas_mount
from services.masks_loader import load_builtin_masks
from services.users_loader import load_users

bp = Blueprint("api_misc", __name__, url_prefix="/api/v1")


# /health stays public — the Docker healthcheck calls it without a
# session. Everything else under this blueprint requires login.
_PUBLIC_PATHS = {"/api/v1/health"}


@bp.before_request
def _require_login_except_health():
    if request.path in _PUBLIC_PATHS:
        return None
    if not session.get("user_id"):
        return jsonify({"error": "authentication required"}), 401
    return None


def _cfg():
    return current_app.config["WEBUI_CONFIG"]


@bp.get("/health")
def health():
    cfg = _cfg()
    check = check_nas_mount(cfg)
    worker = current_app.config.get("WORKER")
    worker_alive = worker.is_alive() if worker else False
    # Cheap docker daemon probe — bounded at 5 s so /health never hangs.
    try:
        r = _sp.run(
            ["docker", "info", "--format", "{{.ServerVersion}}"],
            capture_output=True, text=True, timeout=5,
        )
        docker_ok = r.returncode == 0
        docker_version = r.stdout.strip() if docker_ok else None
        docker_error = r.stderr.strip() if not docker_ok else None
    except (FileNotFoundError, _sp.TimeoutExpired) as exc:
        docker_ok = False
        docker_version = None
        docker_error = str(exc)
    return jsonify({
        "status": "ok" if (check["ok"] and worker_alive and docker_ok)
                  else "degraded",
        "app_name": cfg.app_name,
        "app_version": cfg.app_version,
        "release_date": cfg.release_date,
        "nas_root": str(cfg.nas_root),
        "nas_check": check,
        "worker_alive": worker_alive,
        "docker": {"ok": docker_ok, "version": docker_version,
                   "error": docker_error},
        "db_path": str(cfg.db_path),
    })


@bp.get("/config")
def get_config():
    cfg = _cfg()
    public = {
        k: (str(v) if isinstance(v, Path) else v)
        for k, v in cfg.raw.items()
    }
    return jsonify(public)


@bp.get("/users")
def get_users():
    cfg = _cfg()
    users = load_users(cfg.users_file)
    return jsonify({"users": [asdict(u) for u in users]})


@bp.get("/monitor")
def get_monitor():
    """Aggregate stats for the Monitor page.

    Computed live off the SQLite jobs table — no separate stats table.
    Cheap because the DB stays small (jobs accumulate at a few per
    sequencing run, not at request scale).
    """
    dao: JobsDAO = current_app.config["DAO"]
    now = datetime.utcnow()
    iso_24h = (now - timedelta(hours=24)).strftime("%Y-%m-%dT%H:%M:%SZ")
    iso_7d = (now - timedelta(days=7)).strftime("%Y-%m-%dT%H:%M:%SZ")
    overall = dao.stats()
    last_24h = dao.stats(since=iso_24h)
    last_7d = dao.stats(since=iso_7d)
    with dao._connect() as conn:  # type: ignore[attr-defined]
        avg_done = conn.execute(
            "SELECT AVG(duration_seconds) FROM jobs WHERE state='done'"
        ).fetchone()[0]
        best_masks = [
            {"mask": r["best_mask"], "count": r["c"]}
            for r in conn.execute(
                "SELECT best_mask, COUNT(*) AS c FROM jobs "
                "WHERE best_mask IS NOT NULL "
                "GROUP BY best_mask ORDER BY c DESC LIMIT 10"
            )
        ]
        recent_done = [
            dict(r) for r in conn.execute(
                "SELECT job_id, run_id, submitter, submitted_at, "
                "finished_at, best_mask, best_score, duration_seconds "
                "FROM jobs WHERE state='done' "
                "ORDER BY finished_at DESC LIMIT 5"
            )
        ]
    return jsonify({
        "overall": overall,
        "last_24h": last_24h,
        "last_7d": last_7d,
        "avg_done_seconds": int(avg_done) if avg_done is not None else None,
        "top_best_masks": best_masks,
        "recent_done": recent_done,
    })


@bp.get("/masks")
def get_masks():
    cfg = _cfg()
    masks = load_builtin_masks(cfg.masks_file)
    return jsonify({"masks": [
        {"text": m.text, "safe_name": m.safe_name, "source": m.source}
        for m in masks
    ]})
