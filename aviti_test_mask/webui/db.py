"""SQLite DAO for job records.

Schema v1 — see ``dev_docs/plan_webui.md`` for the design rationale.
WAL mode is enabled at first connection so readers don't block the
writer. No ORM; raw SQL keeps the dependency surface small and the
lock semantics transparent.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json
import sqlite3
from contextlib import contextmanager
from dataclasses import dataclass, asdict, fields as dc_fields
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterator

SCHEMA_VERSION = 1

SCHEMA_SQL = """
CREATE TABLE IF NOT EXISTS jobs (
  job_id              TEXT PRIMARY KEY,
  submitter           TEXT NOT NULL,
  run_id              TEXT NOT NULL,
  run_path            TEXT NOT NULL,
  params_json         TEXT NOT NULL,
  masks_source        TEXT NOT NULL,
  masks_json          TEXT NOT NULL,
  tiles_spec          TEXT,
  state               TEXT NOT NULL,
  queue_position      INTEGER,
  cache_input         INTEGER NOT NULL,
  threads             INTEGER NOT NULL,
  max_jobs            INTEGER NOT NULL,
  docker_image        TEXT NOT NULL,
  mem_limit_per_job   TEXT,
  submitted_at        TEXT NOT NULL,
  started_at          TEXT,
  finished_at         TEXT,
  duration_seconds    INTEGER,
  exit_code           INTEGER,
  mask_count          INTEGER NOT NULL,
  masks_succeeded     INTEGER DEFAULT 0,
  masks_failed        INTEGER DEFAULT 0,
  best_score          REAL,
  best_mask           TEXT,
  lane_projects_json  TEXT NOT NULL DEFAULT '{}',  -- {"1": "P12345", "2": "P67890"}
  error_message       TEXT,
  cancelled_by        TEXT
);

CREATE INDEX IF NOT EXISTS ix_jobs_state ON jobs(state);
CREATE INDEX IF NOT EXISTS ix_jobs_submitter ON jobs(submitter);
CREATE INDEX IF NOT EXISTS ix_jobs_submitted_at ON jobs(submitted_at);

-- One row per (job, mask, lane). Lane is 'all' for the run-level aggregate
-- and '1', '2', ... for per-lane metrics. The web UI lets the user load
-- different projects on different lanes, so per-lane breakdown is the
-- primary view; the 'all' row stays available for backwards-compat reports.
CREATE TABLE IF NOT EXISTS mask_results (
  job_id        TEXT NOT NULL REFERENCES jobs(job_id) ON DELETE CASCADE,
  mask          TEXT NOT NULL,
  lane          TEXT NOT NULL DEFAULT 'all',
  project       TEXT,                              -- copied from jobs.lane_projects_json[lane]
  status        TEXT NOT NULL,
  q30_pct       REAL,
  assigned_pct  REAL,
  score         REAL,
  source        TEXT,
  error_msg     TEXT,
  PRIMARY KEY (job_id, mask, lane)
);
CREATE INDEX IF NOT EXISTS ix_mask_results_lane ON mask_results(lane);
CREATE INDEX IF NOT EXISTS ix_mask_results_project ON mask_results(project);

CREATE TABLE IF NOT EXISTS schema_version (
  version INTEGER PRIMARY KEY
);
"""

VALID_STATES = {
    "queued", "running", "integrating",
    "stopping", "done", "failed", "cancelled", "deleted",
}

# Columns updatable via JobsDAO.update() — anything outside this set is rejected
# to defeat caller-controlled column-name injection in the dynamic SQL builders.
_JOB_UPDATABLE: frozenset[str] = frozenset({
    "state", "queue_position", "tiles_spec", "started_at", "finished_at",
    "duration_seconds", "exit_code", "masks_succeeded", "masks_failed",
    "best_score", "best_mask", "error_message", "cancelled_by",
})
_MASK_RESULT_COLUMNS: frozenset[str] = frozenset({
    "lane", "status", "q30_pct", "assigned_pct", "score", "source", "error_msg",
})


@dataclass
class JobRecord:
    job_id: str
    submitter: str
    run_id: str
    run_path: str
    params_json: str
    masks_source: str
    masks_json: str
    state: str
    cache_input: int
    threads: int
    max_jobs: int
    docker_image: str
    submitted_at: str
    mask_count: int
    tiles_spec: str | None = None
    queue_position: int | None = None
    mem_limit_per_job: str | None = None
    started_at: str | None = None
    finished_at: str | None = None
    duration_seconds: int | None = None
    exit_code: int | None = None
    masks_succeeded: int = 0
    masks_failed: int = 0
    best_score: float | None = None
    best_mask: str | None = None
    error_message: str | None = None
    cancelled_by: str | None = None


_JOB_COLUMNS: tuple[str, ...] = tuple(f.name for f in dc_fields(JobRecord))


def utc_now_iso() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


class JobsDAO:
    def __init__(self, path: Path):
        self.path = Path(path)
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self._init_schema()

    @contextmanager
    def _connect(self) -> Iterator[sqlite3.Connection]:
        conn = sqlite3.connect(self.path, isolation_level=None)
        conn.row_factory = sqlite3.Row
        try:
            conn.execute("PRAGMA foreign_keys = ON")
            yield conn
        finally:
            conn.close()

    def _init_schema(self) -> None:
        with self._connect() as conn:
            conn.execute("PRAGMA journal_mode = WAL")
            conn.executescript(SCHEMA_SQL)
            cur = conn.execute("SELECT version FROM schema_version LIMIT 1")
            row = cur.fetchone()
            if row is None:
                conn.execute("INSERT INTO schema_version(version) VALUES (?)",
                             (SCHEMA_VERSION,))
            elif row[0] != SCHEMA_VERSION:
                raise RuntimeError(
                    f"DB schema v{row[0]} mismatches code v{SCHEMA_VERSION}; "
                    "migration not implemented yet."
                )

    def insert(self, job: JobRecord) -> None:
        if job.state not in VALID_STATES:
            raise ValueError(f"invalid state {job.state!r}")
        # Column names come from JobRecord's declared fields — caller cannot inject.
        cols = _JOB_COLUMNS
        placeholders = ",".join(f":{c}" for c in cols)
        sql = f"INSERT INTO jobs ({','.join(cols)}) VALUES ({placeholders})"  # noqa: S608
        with self._connect() as conn:
            conn.execute(sql, asdict(job))

    def update(self, job_id: str, **fields: Any) -> None:
        if "state" in fields and fields["state"] not in VALID_STATES:
            raise ValueError(f"invalid state {fields['state']!r}")
        if not fields:
            return
        bad = [k for k in fields if k not in _JOB_UPDATABLE]
        if bad:
            raise ValueError(f"unknown / non-updatable columns: {bad}")
        set_clause = ",".join(f"{k}=:{k}" for k in fields)
        params = dict(fields)
        params["job_id"] = job_id
        with self._connect() as conn:
            conn.execute(
                f"UPDATE jobs SET {set_clause} WHERE job_id=:job_id",  # noqa: S608
                params,
            )

    def get(self, job_id: str) -> dict | None:
        with self._connect() as conn:
            cur = conn.execute("SELECT * FROM jobs WHERE job_id=?", (job_id,))
            row = cur.fetchone()
            return dict(row) if row else None

    def list(
        self,
        *,
        states: list[str] | None = None,
        submitter: str | None = None,
        since: str | None = None,
        limit: int = 100,
        offset: int = 0,
        order_by: str = "submitted_at DESC",
    ) -> tuple[list[dict], int]:
        where: list[str] = []
        params: dict[str, Any] = {}
        if states:
            placeholders = ",".join(f":s{i}" for i in range(len(states)))
            where.append(f"state IN ({placeholders})")
            for i, s in enumerate(states):
                params[f"s{i}"] = s
        if submitter:
            where.append("submitter=:submitter")
            params["submitter"] = submitter
        if since:
            where.append("submitted_at >= :since")
            params["since"] = since
        where_clause = f"WHERE {' AND '.join(where)}" if where else ""
        allowed_order = {
            "submitted_at DESC", "submitted_at ASC",
            "started_at DESC", "started_at ASC",
            "duration_seconds DESC", "duration_seconds ASC",
        }
        if order_by not in allowed_order:
            order_by = "submitted_at DESC"

        with self._connect() as conn:
            total = conn.execute(f"SELECT COUNT(*) FROM jobs {where_clause}",
                                 params).fetchone()[0]
            params["limit"] = limit
            params["offset"] = offset
            cur = conn.execute(
                f"SELECT * FROM jobs {where_clause} ORDER BY {order_by} "
                f"LIMIT :limit OFFSET :offset",
                params,
            )
            rows = [dict(r) for r in cur.fetchall()]
        return rows, total

    def add_mask_result(self, job_id: str, mask: str, **fields: Any) -> None:
        fields.setdefault("status", "ok")
        bad = [k for k in fields if k not in _MASK_RESULT_COLUMNS]
        if bad:
            raise ValueError(f"unknown mask_results columns: {bad}")
        cols = ["job_id", "mask"] + list(fields.keys())
        placeholders = ",".join(f":{c}" for c in cols)
        params = {"job_id": job_id, "mask": mask, **fields}
        with self._connect() as conn:
            conn.execute(
                f"INSERT INTO mask_results ({','.join(cols)}) VALUES ({placeholders})",  # noqa: S608
                params,
            )

    def soft_delete(self, job_id: str) -> None:
        """Mark a job as deleted (session files removed elsewhere)."""
        self.update(job_id, state="deleted", error_message="purged")

    def stats(self, *, since: str | None = None) -> dict:
        with self._connect() as conn:
            where = "WHERE submitted_at >= :since" if since else ""
            params = {"since": since} if since else {}
            by_state = {
                row["state"]: row["c"]
                for row in conn.execute(
                    f"SELECT state, COUNT(*) AS c FROM jobs {where} GROUP BY state",
                    params,
                )
            }
            by_user = {
                row["submitter"]: row["c"]
                for row in conn.execute(
                    f"SELECT submitter, COUNT(*) AS c FROM jobs {where} GROUP BY submitter",
                    params,
                )
            }
        return {"by_state": by_state, "by_submitter": by_user}
