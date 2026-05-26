"""Run-discovery API — /api/v1/runs, /api/v1/runs/<id>, /api/v1/runs/validated.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

from pathlib import Path

from flask import Blueprint, current_app, jsonify, request

from services.db import RunsMetadataDAO
from services.discovery import (
    is_test_run, read_run_metadata, read_run_start, scan_nas_for_runs,
    validate_run,
)

bp = Blueprint("api_runs", __name__, url_prefix="/api/v1/runs")

_VALIDATION_CACHE: dict[tuple[str, float], dict] = {}
_METADATA_CACHE: dict[tuple[str, float], dict | None] = {}


def _cfg():
    return current_app.config["WEBUI_CONFIG"]


def _cached_validate(path: Path, mtime: float, cfg) -> dict:
    key = (str(path), mtime)
    cached = _VALIDATION_CACHE.get(key)
    if cached is not None:
        return cached
    out = validate_run(path, cfg)
    _VALIDATION_CACHE[key] = out
    return out


def _cached_metadata(path: Path, mtime: float) -> dict | None:
    key = (str(path), mtime)
    if key in _METADATA_CACHE:
        return _METADATA_CACHE[key]
    out = read_run_metadata(path)
    _METADATA_CACHE[key] = out
    return out


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


@bp.get("")
def get_runs():
    """Paginated candidate list, each item enriched with run metadata."""
    cfg = _cfg()
    candidates, warnings = scan_nas_for_runs(cfg)
    page, per_page = _page_args()
    rows = [
        {
            "run_id": c.run_id,
            "sequencer": c.sequencer,
            "path": str(c.path),
            "mtime": c.mtime,
            "run_start": None,
            "validated": False,
        }
        for c in candidates
    ]
    pag = _paginate(rows, page, per_page)
    runs_dao: RunsMetadataDAO = current_app.config["RUNS_DAO"]
    for row in pag["items"]:
        p = Path(row["path"])
        mtime = row["mtime"]
        meta = _cached_metadata(p, mtime)
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
        v = _cached_validate(p, mtime, cfg)
        row["valid"] = v["valid"]
        row["first_failure"] = (
            v["first_failure"]["name"] if v["first_failure"] else None
        )
    return jsonify({
        "runs": pag["items"],
        "pagination": {k: v for k, v in pag.items() if k != "items"},
        "warnings": warnings,
    })


@bp.get("/validated")
def get_runs_validated():
    """Same listing but validated server-side and split valid/invalid."""
    cfg = _cfg()
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

    valid, invalid = [], []
    for entry in pag["items"]:
        cand = entry.pop("_candidate")
        result = _cached_validate(cand.path, cand.mtime, cfg)
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


@bp.get("/<run_internal_id>")
def get_run_detail(run_internal_id: str):
    cfg = _cfg()
    runs_dao: RunsMetadataDAO = current_app.config["RUNS_DAO"]
    row = runs_dao.get(run_internal_id)
    if row is None:
        return jsonify({"error": "unknown run"}), 404
    # Re-read from disk so an in-progress run that has since finished
    # doesn't serve a stale snapshot.
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
