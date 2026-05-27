"""Tests for the job state machine.

Pins the FSM in three layers:
1. The module-level ``ALLOWED`` set and the helper functions
   (``validate_transition``, ``can_transition``, ``is_terminal``).
2. ``JobsDAO.update`` consults the FSM and rejects illegal moves.
3. ``soft_delete`` only works from terminal states.
"""
from __future__ import annotations

import pytest

from services import job_lifecycle as fsm
from services.db import JobsDAO, JobRecord, utc_now_iso
from services.job_lifecycle import IllegalTransition, JobState


# ── module-level FSM ─────────────────────────────────────────────────


def test_terminal_set_is_what_we_expect():
    assert fsm.TERMINAL == frozenset({
        JobState.DONE, JobState.FAILED,
        JobState.CANCELLED, JobState.DELETED,
    })


def test_is_terminal():
    assert fsm.is_terminal("done")
    assert fsm.is_terminal("failed")
    assert fsm.is_terminal(JobState.CANCELLED)
    assert not fsm.is_terminal("queued")
    assert not fsm.is_terminal("running")


def test_all_states_includes_every_enum_value():
    assert fsm.ALL_STATES == {s.value for s in JobState}


def test_can_transition_identity_is_always_true():
    # Idempotent re-writes are allowed at every state.
    for s in JobState:
        assert fsm.can_transition(s, s)


def test_can_transition_happy_path():
    assert fsm.can_transition("queued", "running")
    assert fsm.can_transition("running", "integrating")
    assert fsm.can_transition("integrating", "done")
    assert fsm.can_transition("done", "deleted")


def test_can_transition_rejects_skips():
    # Cannot jump from queued to done.
    assert not fsm.can_transition("queued", "done")
    # Cannot resurrect a terminal job.
    assert not fsm.can_transition("done", "running")
    assert not fsm.can_transition("failed", "queued")
    assert not fsm.can_transition("cancelled", "queued")
    # Cannot go straight from running to done without integrating.
    assert not fsm.can_transition("running", "done")


def test_validate_transition_raises_illegal():
    with pytest.raises(IllegalTransition) as exc:
        fsm.validate_transition("done", "running")
    assert exc.value.current == "done"
    assert exc.value.target == "running"


def test_validate_transition_unknown_state_raises_value_error():
    with pytest.raises(ValueError):
        fsm.validate_transition("queued", "bogus")
    with pytest.raises(ValueError):
        fsm.validate_transition("bogus", "queued")


def test_pause_resume_cycle():
    fsm.validate_transition("queued", "paused")
    fsm.validate_transition("paused", "queued")


def test_cancel_paths():
    # User cancels before start.
    fsm.validate_transition("queued", "cancelled")
    fsm.validate_transition("paused", "cancelled")
    # User cancels mid-run → stopping → cancelled.
    fsm.validate_transition("running", "stopping")
    fsm.validate_transition("stopping", "cancelled")


def test_failure_paths_from_every_phase():
    # Operational failure (preflight, stale-reap, integrator crash) can
    # land in 'failed' from any non-terminal state except 'stopping'
    # which has its own reaped→cancelled flow.
    for src in ("queued", "paused", "running", "integrating", "stopping"):
        fsm.validate_transition(src, "failed")


def test_purge_only_from_terminal():
    fsm.validate_transition("done", "deleted")
    fsm.validate_transition("failed", "deleted")
    fsm.validate_transition("cancelled", "deleted")
    with pytest.raises(IllegalTransition):
        fsm.validate_transition("queued", "deleted")
    with pytest.raises(IllegalTransition):
        fsm.validate_transition("running", "deleted")


# ── DAO integration ──────────────────────────────────────────────────


def _record(job_id="job-1", **overrides):
    base = dict(
        job_id=job_id, submitter="splaisan",
        run_id="20260522_AV224503_5279_1",
        run_path="/tmp/run", params_json="{}",
        masks_source="builtin", masks_json="[]",
        state="queued", cache_input=0,
        threads=8, max_jobs=3,
        docker_image="elembio/bases2fastq:latest",
        submitted_at=utc_now_iso(), mask_count=5,
    )
    base.update(overrides)
    return JobRecord(**base)


def test_dao_rejects_skip_from_queued_to_done(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(_record())
    with pytest.raises(IllegalTransition):
        dao.update("job-1", state="done", finished_at=utc_now_iso())
    # Row state must be untouched after a rejected write.
    assert dao.get("job-1")["state"] == "queued"


def test_dao_rejects_resurrecting_terminal(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(_record(state="failed"))
    with pytest.raises(IllegalTransition):
        dao.update("job-1", state="running")
    assert dao.get("job-1")["state"] == "failed"


def test_dao_allows_full_happy_path(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(_record())
    dao.update("job-1", state="running", started_at=utc_now_iso())
    dao.update("job-1", state="integrating")
    dao.update("job-1", state="done", finished_at=utc_now_iso())
    assert dao.get("job-1")["state"] == "done"


def test_dao_allows_pause_resume(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(_record())
    dao.update("job-1", state="paused")
    dao.update("job-1", state="queued")
    assert dao.get("job-1")["state"] == "queued"


def test_dao_idempotent_state_write(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(_record(state="running", started_at=utc_now_iso()))
    # Re-writing the same state shouldn't raise.
    dao.update("job-1", state="running")
    assert dao.get("job-1")["state"] == "running"


def test_dao_update_without_state_skips_check(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(_record())
    # Non-state updates (e.g. progress counts) must not consult the FSM.
    dao.update("job-1", masks_succeeded=3, masks_failed=0)
    row = dao.get("job-1")
    assert row["masks_succeeded"] == 3
    assert row["state"] == "queued"


def test_dao_soft_delete_rejects_non_terminal(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(_record())  # queued
    with pytest.raises(IllegalTransition):
        dao.soft_delete("job-1")
    assert dao.get("job-1")["state"] == "queued"


def test_dao_soft_delete_from_done(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(_record(state="done"))
    dao.soft_delete("job-1")
    assert dao.get("job-1")["state"] == "deleted"
