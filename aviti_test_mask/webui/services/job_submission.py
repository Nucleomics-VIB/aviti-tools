"""Submit a new job — validation, tile resolution, JobRecord assembly.

The submit POST route used to do all of this inline, mixing form parsing
with domain decisions. This module is the deep seam: route shrinks to
"parse the form, call ``submit_job``, flash the result, redirect".
Unit tests exercise the whole submit flow without a Flask test client.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json
import uuid
from dataclasses import dataclass
from pathlib import Path

from .config_loader import WebUIConfig
from .db import JobRecord, JobsDAO, utc_now_iso
from .discovery import resolve_tile_spec


@dataclass(frozen=True)
class SubmissionResult:
    """Outcome of a submit attempt.

    Exactly one of ``job_id`` / ``error`` is set. ``ok`` mirrors that
    so callers can branch on a single attribute.
    """
    ok: bool
    job_id: str | None = None
    error: str | None = None

    @classmethod
    def success(cls, job_id: str) -> "SubmissionResult":
        return cls(ok=True, job_id=job_id)

    @classmethod
    def failure(cls, error: str) -> "SubmissionResult":
        return cls(ok=False, error=error)


def _build_job_id(run_id: str) -> str:
    return (f"{run_id}__{utc_now_iso().replace(':', '-')}"
            f"__{uuid.uuid4().hex[:6]}")


def submit_job(
    cfg: WebUIConfig,
    dao: JobsDAO,
    run: dict,
    *,
    submitter: str,
    masks_source: str,
    masks_list: list[str],
    tiles_mode: str = "default",
    tiles_n: int = 3,
    tiles_lane: int = 1,
    tiles_raw: str | None = None,
    lanes: str = "all",
    threads: int | None = None,
    max_jobs: int | None = None,
    docker_image: str | None = None,
    mem_limit: str | None = None,
    cache_input: bool = False,
) -> SubmissionResult:
    """Validate inputs, resolve the tile spec, insert a queued job.

    Returns ``SubmissionResult.failure(msg)`` for any user-correctable
    problem (missing submitter, no masks, bad tile spec). Returns
    ``SubmissionResult.success(job_id)`` on insert.
    """
    submitter = (submitter or "").strip()
    if not submitter:
        return SubmissionResult.failure("Submitter is required.")
    if not masks_list:
        return SubmissionResult.failure("Pick at least one mask.")

    try:
        tile_resolution = resolve_tile_spec(
            Path(run["run_path"]),
            tiles_mode,
            tiles_n=tiles_n,
            tiles_lane=tiles_lane,
            tiles_raw=tiles_raw,
            lanes=lanes,
        )
    except ValueError as exc:
        return SubmissionResult.failure(f"Tile selection error: {exc}")

    job_id = _build_job_id(run["run_id"])
    record = JobRecord(
        job_id=job_id,
        submitter=submitter,
        run_id=run["run_id"],
        run_path=run["run_path"],
        params_json=json.dumps({
            "lanes": lanes,
            "tiles_mode": tiles_mode,
            "tiles_spec": tile_resolution["spec"],
            "tiles_pattern": tile_resolution["pattern"],
            "tiles_picked": tile_resolution["tiles"],
        }),
        masks_source=masks_source,
        masks_json=json.dumps(masks_list),
        state="queued",
        cache_input=1 if cache_input else 0,
        threads=int(threads if threads is not None else cfg.threads),
        max_jobs=int(max_jobs if max_jobs is not None
                     else cfg.max_inner_jobs),
        docker_image=docker_image or cfg.raw.get("docker_image", ""),
        submitted_at=utc_now_iso(),
        mask_count=len(masks_list),
        tiles_spec=tile_resolution["spec"],
        mem_limit_per_job=mem_limit or None,
        run_internal_id=run.get("run_internal_id"),
    )
    dao.insert(record)
    return SubmissionResult.success(job_id)
