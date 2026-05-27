"""End-to-end JobWorker lifecycle test.

Boots a real ``JobsDAO`` and a real ``JobWorker`` against stub bash
scripts that mimic the log markers and CSV format the production
``aviti_test_mask.sh`` / ``integrate_mask_results.sh`` emit.

The DAO-level FSM test (``test_job_lifecycle.py``) only proves that
each *single* transition is allowed or rejected. This test proves the
worker drives a queued row all the way to ``done`` *in the right
order* — the class of bug (slot-leak, missed post-exit reconciliation)
that motivated the FSM in the first place.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json
import stat
import time
from pathlib import Path
from types import SimpleNamespace

from services.db import JobsDAO, JobRecord, utc_now_iso
from services.docker_client import DaemonInfo, DockerClient
from services.job_worker import JobWorker


# ── Stub scripts ─────────────────────────────────────────────────────

STUB_PIPELINE_OK = """#!/usr/bin/env bash
out=""
job_id=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o) out="$2"; shift 2 ;;
    --job-id) job_id="$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "$out"
{
  echo "[stub] launching job ${job_id}"
  echo '✅ [mask_a] completed in 1s'
  echo '✅ [mask_b] completed in 1s'
  echo '\U0001F4CA 2/2 succeeded  |  0 failed'
} >> "$out/run.log"
exit 0
"""

STUB_PIPELINE_FAIL = """#!/usr/bin/env bash
out=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o) out="$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "$out"
{
  echo "[stub] crashing"
  echo "Error: simulated failure"
} >> "$out/run.log"
exit 7
"""

STUB_INTEGRATOR_OK = """#!/usr/bin/env bash
out=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o) out="$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "$out"
cat > "$out/mask_integration_summary.csv" <<EOF
Mask,Q30%,%Assigned,Score,Source
mask_a,95.1,99.0,0.95,run
mask_b,93.4,98.2,0.91,run
EOF
exit 0
"""


# ── Fakes ────────────────────────────────────────────────────────────


class FakeDocker(DockerClient):
    """Docker stand-in: daemon always ok, no containers, no-op stop."""

    def __init__(self):  # noqa: D401  -- intentional: skip super init
        pass

    def find_containers_for_job(self, job_id):
        return []

    def inspect_status(self, container_id):
        return "unknown"

    def daemon_info(self):
        return DaemonInfo(ok=True, version="fake")

    def stop_containers(self, container_ids):
        return None


# ── Fixtures helpers ─────────────────────────────────────────────────


def _install_stubs(scripts_dir: Path, *, pipeline: str,
                   integrator: str = STUB_INTEGRATOR_OK) -> None:
    scripts_dir.mkdir(parents=True, exist_ok=True)
    for name, body in (("aviti_test_mask.sh", pipeline),
                       ("integrate_mask_results.sh", integrator)):
        p = scripts_dir / name
        p.write_text(body)
        p.chmod(p.stat().st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH)


def _make_cfg(tmp_path: Path) -> SimpleNamespace:
    scripts_dir = tmp_path / "scripts"
    results_root = tmp_path / "results"
    results_root.mkdir(parents=True, exist_ok=True)
    return SimpleNamespace(
        scripts_dir=scripts_dir,
        results_root=results_root,
        max_global_containers=2,
        max_jobs_per_user=2,
        conda_env_name="test_env",
    )


def _enqueue(dao: JobsDAO, *, job_id: str, run_path: Path,
             submitter: str = "splaisan") -> None:
    run_path.mkdir(parents=True, exist_ok=True)
    (run_path / "RunParameters.json").write_text("{}")
    dao.insert(JobRecord(
        job_id=job_id,
        submitter=submitter,
        run_id="test-run",
        run_path=str(run_path),
        params_json="{}",
        masks_source="builtin",
        masks_json=json.dumps(["mask_a", "mask_b"]),
        state="queued",
        cache_input=0,
        threads=4,
        max_jobs=2,
        docker_image="stub:latest",
        submitted_at=utc_now_iso(),
        mask_count=2,
    ))


def _wait_until(predicate, *, timeout: float = 10.0,
                interval: float = 0.05) -> bool:
    deadline = time.time() + timeout
    while time.time() < deadline:
        if predicate():
            return True
        time.sleep(interval)
    return False


def _fast_poll(monkeypatch):
    """Shrink the worker's idle interval so the test wraps in <2 s."""
    from services import job_worker as jw_mod
    monkeypatch.setattr(jw_mod, "POLL_INTERVAL_SECONDS", 0.05)


# ── Happy path ───────────────────────────────────────────────────────


def test_lifecycle_happy_path_runs_to_done(tmp_path, monkeypatch):
    _fast_poll(monkeypatch)
    cfg = _make_cfg(tmp_path)
    _install_stubs(cfg.scripts_dir, pipeline=STUB_PIPELINE_OK)
    dao = JobsDAO(tmp_path / "jobs.db")
    _enqueue(dao, job_id="job-happy", run_path=tmp_path / "run")

    worker = JobWorker(cfg, dao, docker=FakeDocker())
    worker.start()
    try:
        reached = _wait_until(
            lambda: (dao.get("job-happy") or {}).get("state") == "done",
            timeout=10.0,
        )
    finally:
        worker.stop()

    row = dao.get("job-happy")
    assert reached, f"job did not reach 'done'; last row={row}"
    assert row["state"] == "done"
    assert row["exit_code"] == 0
    assert row["error_message"] is None
    assert row["finished_at"] is not None
    # Best mask bubbled up from the integrator CSV (mask_a score 0.95).
    assert row["best_mask"] == "mask_a"
    assert row["best_score"] == 0.95
    # Slot drained — no orphan in the worker's in-memory active table.
    assert worker._active == {}


# ── Script-failure path ──────────────────────────────────────────────


def test_lifecycle_script_failure_lands_in_failed(tmp_path, monkeypatch):
    _fast_poll(monkeypatch)
    cfg = _make_cfg(tmp_path)
    _install_stubs(cfg.scripts_dir, pipeline=STUB_PIPELINE_FAIL)
    dao = JobsDAO(tmp_path / "jobs.db")
    _enqueue(dao, job_id="job-fail", run_path=tmp_path / "run")

    worker = JobWorker(cfg, dao, docker=FakeDocker())
    worker.start()
    try:
        reached = _wait_until(
            lambda: (dao.get("job-fail") or {}).get("state") == "failed",
            timeout=10.0,
        )
    finally:
        worker.stop()

    row = dao.get("job-fail")
    assert reached, f"job did not reach 'failed'; last row={row}"
    assert row["state"] == "failed"
    assert row["exit_code"] == 7
    assert "simulated failure" in (row["error_message"] or "")
    assert worker._active == {}


# ── Preflight-failure path ───────────────────────────────────────────


def test_lifecycle_preflight_failure_skips_running(tmp_path, monkeypatch):
    """Preflight rejection transitions queued → failed directly.

    The FSM allows ``(queued, failed)`` precisely so this path doesn't
    have to fake a running state first.
    """
    _fast_poll(monkeypatch)
    cfg = _make_cfg(tmp_path)
    _install_stubs(cfg.scripts_dir, pipeline=STUB_PIPELINE_OK)
    dao = JobsDAO(tmp_path / "jobs.db")
    # run_path points at a directory that does not exist → preflight fails.
    bogus = tmp_path / "does_not_exist"
    dao.insert(JobRecord(
        job_id="job-pre",
        submitter="splaisan",
        run_id="test-run",
        run_path=str(bogus),
        params_json="{}",
        masks_source="builtin",
        masks_json=json.dumps(["m"]),
        state="queued",
        cache_input=0, threads=4, max_jobs=2,
        docker_image="stub:latest",
        submitted_at=utc_now_iso(),
        mask_count=1,
    ))

    worker = JobWorker(cfg, dao, docker=FakeDocker())
    worker.start()
    try:
        reached = _wait_until(
            lambda: (dao.get("job-pre") or {}).get("state") == "failed",
            timeout=5.0,
        )
    finally:
        worker.stop()

    row = dao.get("job-pre")
    assert reached, f"preflight failure didn't reach 'failed'; row={row}"
    assert row["state"] == "failed"
    assert "preflight" in (row["error_message"] or "")
    # started_at IS set (the worker stamps it on the preflight-fail row
    # so the History page can sort consistently), but no script ever ran.
    assert row["exit_code"] is None
    assert worker._active == {}
