"""Misc API endpoints — /health, /config, /users, /masks.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import subprocess as _sp
from dataclasses import asdict
from pathlib import Path

from flask import Blueprint, current_app, jsonify

from services.discovery import check_nas_mount
from services.masks_loader import load_builtin_masks
from services.users_loader import load_users

bp = Blueprint("api_misc", __name__, url_prefix="/api/v1")


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


@bp.get("/masks")
def get_masks():
    cfg = _cfg()
    masks = load_builtin_masks(cfg.masks_file)
    return jsonify({"masks": [
        {"text": m.text, "safe_name": m.safe_name, "source": m.source}
        for m in masks
    ]})
