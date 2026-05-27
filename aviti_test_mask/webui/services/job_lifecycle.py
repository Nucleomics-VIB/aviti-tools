"""Explicit job state machine.

Before this module existed, the nine possible job states lived as free
strings in the ``jobs.state`` column and any code path could write any
string into it. The slot-leak bug (commit 6249685) was a transition the
code didn't know was illegal. Now one module owns the full transition
table; ``JobsDAO.update`` consults it before every state write.

State diagram::

                ┌────────────────────────────────┐
                │                                │
                │             ┌──── pause ───────┘
                │             ▼
                │           paused ──── resume ──┐
                ▼                                │
              queued ◄──────────────────────────┘
                │
                │ schedule
                ▼
              running ───── script_ok ──► integrating ──► done
                │                              │
                │ script_err                   │ integrate_err
                ▼                              ▼
              failed                         failed
                │
                │ cancel (from running)
                ▼
              stopping ───── reaped ──► cancelled

Any of ``done`` / ``failed`` / ``cancelled`` can transition to
``deleted`` (soft-delete / purge).

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

from enum import Enum


class JobState(str, Enum):
    QUEUED = "queued"
    PAUSED = "paused"
    RUNNING = "running"
    INTEGRATING = "integrating"
    STOPPING = "stopping"
    DONE = "done"
    FAILED = "failed"
    CANCELLED = "cancelled"
    DELETED = "deleted"


ALL_STATES: frozenset[str] = frozenset(s.value for s in JobState)

TERMINAL: frozenset[JobState] = frozenset({
    JobState.DONE, JobState.FAILED, JobState.CANCELLED, JobState.DELETED,
})

# Every state mutation the system performs in normal operation. Each
# tuple is (from, to). Any pair NOT in this set is rejected by the DAO.
#
# ``failed`` is reachable from almost every non-terminal state because
# operational failures (preflight, stale-reap on server restart,
# integrator crash) can strike at any phase.
ALLOWED: frozenset[tuple[JobState, JobState]] = frozenset({
    # Queued → onward
    (JobState.QUEUED, JobState.RUNNING),
    (JobState.QUEUED, JobState.PAUSED),
    (JobState.QUEUED, JobState.CANCELLED),     # cancel before start
    (JobState.QUEUED, JobState.FAILED),        # preflight / stale-reap
    # Paused ↔ Queued + cancel
    (JobState.PAUSED, JobState.QUEUED),        # resume
    (JobState.PAUSED, JobState.CANCELLED),
    (JobState.PAUSED, JobState.FAILED),
    # Running → onward
    (JobState.RUNNING, JobState.INTEGRATING),  # script exit 0
    (JobState.RUNNING, JobState.FAILED),       # script exit non-zero / stale-reap
    (JobState.RUNNING, JobState.STOPPING),     # user cancel
    # Integrating → terminal
    (JobState.INTEGRATING, JobState.DONE),
    (JobState.INTEGRATING, JobState.FAILED),
    # Stopping → terminal
    (JobState.STOPPING, JobState.CANCELLED),   # reaped
    (JobState.STOPPING, JobState.FAILED),      # reap failed
    # Purge / soft-delete: any terminal → deleted
    (JobState.DONE, JobState.DELETED),
    (JobState.FAILED, JobState.DELETED),
    (JobState.CANCELLED, JobState.DELETED),
})


class IllegalTransition(ValueError):
    """Raised when code attempts a state transition not in ``ALLOWED``."""

    def __init__(self, current: str, target: str):
        super().__init__(
            f"illegal job state transition: {current!r} → {target!r}"
        )
        self.current = current
        self.target = target


def _coerce(value: str | JobState) -> JobState:
    if isinstance(value, JobState):
        return value
    try:
        return JobState(value)
    except ValueError:
        raise ValueError(f"unknown job state: {value!r}") from None


def is_terminal(state: str | JobState) -> bool:
    return _coerce(state) in TERMINAL


def can_transition(current: str | JobState, target: str | JobState) -> bool:
    cur, tgt = _coerce(current), _coerce(target)
    return cur == tgt or (cur, tgt) in ALLOWED


def validate_transition(current: str | JobState,
                        target: str | JobState) -> None:
    """Raise ``IllegalTransition`` if the move is not permitted.

    Idempotent writes (current == target) are always allowed so callers
    that re-issue the same state on retry don't get punished.
    """
    cur, tgt = _coerce(current), _coerce(target)
    if cur == tgt:
        return
    if (cur, tgt) not in ALLOWED:
        raise IllegalTransition(cur.value, tgt.value)
