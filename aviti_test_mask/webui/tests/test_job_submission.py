"""Tests for the job submission service.

Exercises validation, tile resolution, and DB insert without booting
Flask — the route is now a thin shell that just hands form values to
``submit_job``.
"""
from __future__ import annotations

import json
from pathlib import Path
from types import SimpleNamespace

import pytest

from services.db import JobsDAO, RunsMetadataDAO
from services.job_submission import submit_job, SubmissionResult


def _make_cfg(tmp_path: Path) -> SimpleNamespace:
    return SimpleNamespace(
        threads=4,
        max_inner_jobs=2,
        raw={"docker_image": "elembio/bases2fastq:latest"},
    )


def _make_run(tmp_path: Path, *, with_tiles: bool = True) -> dict:
    run_dir = tmp_path / "run"
    run_dir.mkdir()
    rp = {"Cycles": {"R1": 1}}
    if with_tiles:
        rp["Tiles"] = ["L1R01C01S1", "L1R02C01S1"]
    (run_dir / "RunParameters.json").write_text(json.dumps(rp))
    return {
        "run_id": "20260522_AV224503_5279_1",
        "run_path": str(run_dir),
        "run_internal_id": "seq_abc123",
    }


def _dao(tmp_path: Path, run: dict | None = None) -> JobsDAO:
    """Build a JobsDAO and, if a run dict is given, seed the FK row
    in runs_metadata so JobsDAO.insert's FK constraint is satisfied."""
    db_path = tmp_path / "jobs.db"
    dao = JobsDAO(db_path)
    if run is not None:
        meta = RunsMetadataDAO(db_path)
        meta.upsert(run["run_internal_id"], {
            "run_id": run["run_id"],
            "run_path": run["run_path"],
        })
    return dao


# ── Happy path ───────────────────────────────────────────────────────


def test_submit_success_inserts_queued_job(tmp_path):
    cfg = _make_cfg(tmp_path)
    run = _make_run(tmp_path)
    dao = _dao(tmp_path, run)
    result = submit_job(
        cfg, dao, run,
        submitter="splaisan",
        masks_source="builtin",
        masks_list=["R1:Y18N*-R2:Y18N*"],
    )
    assert result.ok is True
    assert result.error is None
    assert result.job_id is not None
    row = dao.get(result.job_id)
    assert row["state"] == "queued"
    assert row["submitter"] == "splaisan"
    assert row["run_internal_id"] == "seq_abc123"
    assert row["mask_count"] == 1


def test_submit_persists_tile_resolution_in_params(tmp_path):
    cfg = _make_cfg(tmp_path)
    run = _make_run(tmp_path)
    dao = _dao(tmp_path, run)
    result = submit_job(
        cfg, dao, run,
        submitter="splaisan",
        masks_source="builtin",
        masks_list=["m1"],
        tiles_mode="default",
    )
    row = dao.get(result.job_id)
    params = json.loads(row["params_json"])
    assert params["tiles_mode"] == "default"
    # Default mode resolves to one tile per lane — see test_discovery.
    assert params["tiles_pattern"] == "L1R01C01S1"
    assert params["tiles_picked"] == ["L1R01C01S1"]


def test_submit_applies_config_defaults(tmp_path):
    cfg = _make_cfg(tmp_path)  # threads=4, max_inner_jobs=2
    run = _make_run(tmp_path)
    dao = _dao(tmp_path, run)
    result = submit_job(
        cfg, dao, run,
        submitter="splaisan",
        masks_source="builtin",
        masks_list=["m1"],
    )
    row = dao.get(result.job_id)
    assert row["threads"] == 4
    assert row["max_jobs"] == 2
    assert row["docker_image"] == "elembio/bases2fastq:latest"


def test_submit_caller_overrides_config_defaults(tmp_path):
    cfg = _make_cfg(tmp_path)
    run = _make_run(tmp_path)
    dao = _dao(tmp_path, run)
    result = submit_job(
        cfg, dao, run,
        submitter="splaisan",
        masks_source="builtin",
        masks_list=["m1"],
        threads=16, max_jobs=8,
        docker_image="custom:1.2", mem_limit="64g",
        cache_input=True,
    )
    row = dao.get(result.job_id)
    assert row["threads"] == 16
    assert row["max_jobs"] == 8
    assert row["docker_image"] == "custom:1.2"
    assert row["mem_limit_per_job"] == "64g"
    assert row["cache_input"] == 1


# ── Validation failures ──────────────────────────────────────────────


def test_submit_rejects_empty_submitter(tmp_path):
    cfg = _make_cfg(tmp_path)
    dao = _dao(tmp_path)
    result = submit_job(
        cfg, dao, _make_run(tmp_path),
        submitter="   ",
        masks_source="builtin",
        masks_list=["m1"],
    )
    assert result.ok is False
    assert "Submitter" in result.error
    # No row inserted.
    assert dao.list()[1] == 0


def test_submit_rejects_empty_masks_list(tmp_path):
    cfg = _make_cfg(tmp_path)
    dao = _dao(tmp_path)
    result = submit_job(
        cfg, dao, _make_run(tmp_path),
        submitter="splaisan",
        masks_source="builtin",
        masks_list=[],
    )
    assert result.ok is False
    assert "mask" in result.error.lower()
    assert dao.list()[1] == 0


def test_submit_rejects_bad_tile_spec(tmp_path):
    cfg = _make_cfg(tmp_path)
    dao = _dao(tmp_path)
    result = submit_job(
        cfg, dao, _make_run(tmp_path),
        submitter="splaisan",
        masks_source="builtin",
        masks_list=["m1"],
        tiles_mode="raw",
        tiles_raw="",  # raw mode rejects empty
    )
    assert result.ok is False
    assert "Tile selection error" in result.error
    assert dao.list()[1] == 0


def test_submit_rejects_unknown_tile_mode(tmp_path):
    cfg = _make_cfg(tmp_path)
    dao = _dao(tmp_path)
    result = submit_job(
        cfg, dao, _make_run(tmp_path),
        submitter="splaisan",
        masks_source="builtin",
        masks_list=["m1"],
        tiles_mode="invented",
    )
    assert result.ok is False
    assert dao.list()[1] == 0


# ── SubmissionResult ─────────────────────────────────────────────────


def test_submission_result_constructors():
    s = SubmissionResult.success("JID")
    assert s.ok is True
    assert s.job_id == "JID"
    assert s.error is None
    f = SubmissionResult.failure("nope")
    assert f.ok is False
    assert f.job_id is None
    assert f.error == "nope"
