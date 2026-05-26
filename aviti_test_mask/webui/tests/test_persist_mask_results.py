"""Tests for the integrator-CSV → mask_results persistence step.

These run synchronously without spawning the worker thread by bypassing
``__init__`` and exercising ``_persist_mask_results`` directly.
"""
from __future__ import annotations

import sqlite3
from types import SimpleNamespace

from services.db import JobsDAO, JobRecord, utc_now_iso
from services.job_worker import JobWorker


def _make_record(job_id: str = "job-1") -> JobRecord:
    return JobRecord(
        job_id=job_id, submitter="alice",
        run_id="20260522_AV224503_5279_1",
        run_path="/tmp/run", params_json="{}",
        masks_source="builtin", masks_json="[]",
        state="integrating", cache_input=0, threads=4, max_jobs=1,
        docker_image="elembio/bases2fastq:latest",
        submitted_at=utc_now_iso(), mask_count=3,
    )


def _bare_worker(dao: JobsDAO) -> JobWorker:
    """Instantiate a JobWorker without running __init__ — we only need
    .dao for the CSV-persist path."""
    worker = JobWorker.__new__(JobWorker)
    worker.dao = dao
    return worker


def test_persist_writes_rows_and_bubbles_best(tmp_path):
    session = tmp_path / "session"
    session.mkdir()
    (session / "mask_integration_summary.csv").write_text(
        "Folder,Mask,%Assigned,Q30%,Score,Source,RunPF,RunQ30\n"
        "mask_0_Y12,R1:Y12N*-R2:Y12N*,98.4,93.2,91.71,metrics.csv,99.1,93.4\n"
        "mask_1_Y10,R1:Y10N*-R2:Y10N*,98.2,89.0,87.40,html,,\n"
        "mask_2_N,R1:N*-R2:N*,50.0,80.0,40.0,log,,\n"
    )
    db_path = tmp_path / "jobs.db"
    dao = JobsDAO(db_path)
    dao.insert(_make_record())

    worker = _bare_worker(dao)
    worker._persist_mask_results(
        SimpleNamespace(job_id="job-1", session_dir=session))

    job = dao.get("job-1")
    assert job["best_mask"] == "R1:Y12N*-R2:Y12N*"
    assert job["best_score"] == 91.71

    with sqlite3.connect(db_path) as conn:
        conn.row_factory = sqlite3.Row
        rows = [dict(r) for r in conn.execute(
            "SELECT mask, lane, q30_pct, assigned_pct, score, source "
            "FROM mask_results WHERE job_id='job-1' ORDER BY score DESC")]
    assert len(rows) == 3
    assert [r["mask"] for r in rows] == [
        "R1:Y12N*-R2:Y12N*", "R1:Y10N*-R2:Y10N*", "R1:N*-R2:N*"]
    assert all(r["lane"] == "all" for r in rows)
    assert rows[0]["q30_pct"] == 93.2
    assert rows[0]["assigned_pct"] == 98.4
    assert rows[0]["source"] == "metrics.csv"


def test_persist_handles_missing_csv(tmp_path):
    session = tmp_path / "session"
    session.mkdir()  # no CSV produced
    db_path = tmp_path / "jobs.db"
    dao = JobsDAO(db_path)
    dao.insert(_make_record())

    worker = _bare_worker(dao)
    worker._persist_mask_results(
        SimpleNamespace(job_id="job-1", session_dir=session))

    # No rows; best_mask untouched.
    assert dao.get("job-1")["best_mask"] is None
    with sqlite3.connect(db_path) as conn:
        n = conn.execute(
            "SELECT COUNT(*) FROM mask_results WHERE job_id='job-1'"
        ).fetchone()[0]
    assert n == 0


def test_persist_skips_blank_score_and_invalid_rows(tmp_path):
    session = tmp_path / "session"
    session.mkdir()
    (session / "mask_integration_summary.csv").write_text(
        "Folder,Mask,%Assigned,Q30%,Score,Source,RunPF,RunQ30\n"
        ",,,,,,,\n"  # blank row → skipped (no mask)
        "mask_a,R1:Y12N*-R2:Y12N*,,,oops,html,,\n"  # bad score → row still inserts, score=None
        "mask_b,R1:N*-R2:N*,80.0,90.0,72.0,log,,\n"
    )
    db_path = tmp_path / "jobs.db"
    dao = JobsDAO(db_path)
    dao.insert(_make_record())

    worker = _bare_worker(dao)
    worker._persist_mask_results(
        SimpleNamespace(job_id="job-1", session_dir=session))

    # Best score comes from the only row with a numeric score.
    job = dao.get("job-1")
    assert job["best_mask"] == "R1:N*-R2:N*"
    assert job["best_score"] == 72.0
