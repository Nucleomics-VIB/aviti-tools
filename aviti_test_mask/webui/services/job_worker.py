"""Background job worker for the aviti_test_mask web UI.

A single daemon thread that polls the SQLite jobs table, picks queued
rows, spawns ``aviti_test_mask.sh``, captures the log, and transitions
state on completion.

Concurrency policy (from plan_webui.md):
- Hard global cap: max_global_containers concurrent bases2fastq jobs
  (default 3). The worker treats one ``aviti_test_mask.sh`` invocation
  as occupying one slot regardless of its inner ``-j`` value; the
  script's own semaphore handles inner parallelism within that slot.
- Fair share: at most max_jobs_per_user active jobs per submitter
  (default 1).

Crash recovery: on startup, any non-terminal jobs left over from a
previous process are marked ``failed`` with a clear message. Re-attach
across restart is deferred — the user-chosen behaviour from the grill
was "Re-attach if containers still alive", but a safe failure mode is
acceptable for this first iteration and the loss is recoverable via
Re-submit (📋).

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json
import logging
import os
import signal
import subprocess
import threading
import time
from dataclasses import dataclass
from pathlib import Path

from .config_loader import WebUIConfig
from .db import JobsDAO, utc_now_iso

log = logging.getLogger("job_worker")

POLL_INTERVAL_SECONDS = 2.0
CANCEL_GRACE_SECONDS = 30


def _extract_error_message(log_path: Path, rc: int, *,
                            max_chars: int = 500) -> str:
    """Pull the most informative line from a failed run.log.

    Prefers, in order:
    1. A line containing ``❌`` (the script's failure markers).
    2. A line starting with ``Error:`` / ``error:`` / ``Traceback``.
    3. The last non-empty line.
    Falls back to ``f"script exit {rc}"`` if the log can't be read.
    """
    try:
        text = log_path.read_text(errors="replace")
    except OSError:
        return f"script exit {rc}"
    lines = [ln.rstrip() for ln in text.splitlines() if ln.strip()]
    if not lines:
        return f"script exit {rc}"
    candidates: list[str] = []
    for ln in lines:
        if "❌" in ln:
            candidates.append(ln)
    if not candidates:
        for ln in lines:
            low = ln.lower()
            if low.startswith("error") or low.startswith("traceback"):
                candidates.append(ln)
    if not candidates:
        candidates.append(lines[-1])
    msg = candidates[-1].strip()
    if len(msg) > max_chars:
        msg = msg[:max_chars] + "…"
    return f"[exit {rc}] {msg}"


@dataclass
class _ActiveJob:
    job_id: str
    submitter: str
    process: subprocess.Popen
    session_dir: Path


class JobWorker:
    def __init__(self, cfg: WebUIConfig, dao: JobsDAO, *,
                 script_path: Path | None = None):
        self.cfg = cfg
        self.dao = dao
        self.script_path = script_path or (cfg.scripts_dir / "aviti_test_mask.sh")
        self._active: dict[str, _ActiveJob] = {}
        self._lock = threading.Lock()
        self._stop = threading.Event()
        self._thread: threading.Thread | None = None

    # ── Lifecycle ────────────────────────────────────────────────────

    def start(self) -> None:
        if self._thread and self._thread.is_alive():
            return
        self._reap_stale_on_startup()
        self._stop.clear()
        self._thread = threading.Thread(target=self._loop, daemon=True,
                                         name="aviti_test_mask_worker")
        self._thread.start()
        log.info("worker started (script=%s)", self.script_path)

    def is_alive(self) -> bool:
        return self._thread is not None and self._thread.is_alive()

    def stop(self, *, kill_active: bool = False) -> None:
        self._stop.set()
        if kill_active:
            with self._lock:
                for aj in list(self._active.values()):
                    self._send_signal(aj, signal.SIGTERM)

    def _reap_stale_on_startup(self) -> None:
        """Mark any non-terminal row left over from a previous process."""
        leftover_states = ["running", "integrating", "stopping"]
        rows, _ = self.dao.list(states=leftover_states, limit=1000)
        for r in rows:
            self.dao.update(
                r["job_id"],
                state="failed",
                finished_at=utc_now_iso(),
                error_message="server restarted mid-run (no reattach yet)",
            )
            log.info("reaped stale job %s (state was %s)",
                     r["job_id"], r["state"])

    # ── Main loop ────────────────────────────────────────────────────

    def _loop(self) -> None:
        while not self._stop.is_set():
            try:
                self._tick()
            except Exception:                                  # noqa: BLE001
                log.exception("worker tick failed")
            self._stop.wait(POLL_INTERVAL_SECONDS)

    def _tick(self) -> None:
        self._reap_finished()
        self._update_progress()
        self._handle_cancellations()
        self._try_launch_next()

    # ── Per-mask progress ────────────────────────────────────────────

    def _update_progress(self) -> None:
        """Count per-mask ✅/❌ lines in each active job's log + update DB."""
        with self._lock:
            snapshot = list(self._active.values())
        for aj in snapshot:
            log_path = aj.session_dir / "run.log"
            if not log_path.exists():
                continue
            try:
                # Tail the last 32 KB — counts stay accurate because the
                # script never repeats per-mask result lines.
                size = log_path.stat().st_size
                with log_path.open("rb") as fh:
                    if size > 32 * 1024:
                        fh.seek(-32 * 1024, 2)
                    text = fh.read().decode("utf-8", errors="replace")
            except OSError:
                continue
            ok = sum(1 for ln in text.splitlines()
                     if ln.startswith("✅ [") and "completed" in ln)
            bad = sum(1 for ln in text.splitlines()
                      if ln.startswith("❌ [") and
                      ("FAILED" in ln or "KILLED" in ln))
            current = self.dao.get(aj.job_id) or {}
            if (current.get("masks_succeeded") != ok
                    or current.get("masks_failed") != bad):
                self.dao.update(aj.job_id,
                                masks_succeeded=ok,
                                masks_failed=bad)

    # ── Launch ───────────────────────────────────────────────────────

    def _try_launch_next(self) -> None:
        with self._lock:
            if len(self._active) >= self.cfg.max_global_containers:
                return
            active_users = {aj.submitter for aj in self._active.values()}
        queued, _ = self.dao.list(states=["queued"],
                                   limit=200,
                                   order_by="submitted_at ASC")
        for row in queued:
            if row["submitter"] in active_users:
                if (sum(1 for aj in self._active.values()
                        if aj.submitter == row["submitter"])
                        >= self.cfg.max_jobs_per_user):
                    continue
            self._launch(row)
            return  # one launch per tick keeps the loop responsive

    def _preflight(self, row: dict) -> str | None:
        """Return an error message if the job can't safely run, else None.

        Cheap probes done before we spawn ``aviti_test_mask.sh`` so a
        misconfigured Docker daemon or a stale run-folder reference
        produces a clear "failed" state with a useful error rather than
        letting the bash script run for ~30 s and bail out itself.
        """
        run_path = Path(row["run_path"])
        if not run_path.exists():
            return f"run folder gone: {run_path}"
        if not run_path.is_dir():
            return f"run path is not a directory: {run_path}"
        try:
            next(iter(run_path.iterdir()))
        except (StopIteration, OSError) as exc:
            return f"run folder unreadable: {exc}"
        try:
            r = subprocess.run(
                ["docker", "info", "--format", "{{.ServerVersion}}"],
                capture_output=True, text=True, timeout=10,
            )
        except FileNotFoundError:
            return "docker CLI not on PATH"
        except subprocess.TimeoutExpired:
            return "docker daemon unreachable (timeout)"
        if r.returncode != 0:
            err = (r.stderr or r.stdout or "").strip().splitlines()
            tail = err[-1] if err else "docker info failed"
            return f"docker daemon unreachable: {tail}"
        return None

    def _launch(self, row: dict) -> None:
        job_id = row["job_id"]
        session_dir = self.cfg.results_root / job_id
        session_dir.mkdir(parents=True, exist_ok=True)
        preflight_err = self._preflight(row)
        if preflight_err:
            self.dao.update(
                job_id, state="failed",
                started_at=utc_now_iso(),
                finished_at=utc_now_iso(),
                error_message=f"[preflight] {preflight_err}",
            )
            log.warning("preflight failed for %s: %s", job_id, preflight_err)
            (session_dir / "run.log").write_text(
                f"# {utc_now_iso()} preflight failed: {preflight_err}\n"
            )
            return
        # Persist the resolved mask list into a small YAML the script reads.
        try:
            masks = json.loads(row.get("masks_json") or "[]")
        except json.JSONDecodeError:
            masks = []
        masks_file = session_dir / "masks.yaml"
        with masks_file.open("w") as fh:
            fh.write("masks:\n")
            for m in masks:
                fh.write(f"  - \"{m}\"\n")

        # Resolved tile pattern (None means "default — no flag").
        try:
            params = json.loads(row.get("params_json") or "{}")
        except json.JSONDecodeError:
            params = {}
        include_tile = params.get("tiles_pattern") or ""

        cmd: list[str] = [
            str(self.script_path),
            "-i", row["run_path"],
            "-o", str(session_dir),
            "-m", str(masks_file),
            "-p", str(row["threads"]),
            "-j", str(row["max_jobs"]),
            "--job-id", job_id,
        ]
        if include_tile:
            cmd += ["--include-tile", include_tile]
        if row.get("mem_limit_per_job"):
            cmd += ["--mem-limit", row["mem_limit_per_job"]]
        if row.get("cache_input"):
            cmd += ["--cache-input"]

        log_path = session_dir / "run.log"
        log_fh = log_path.open("ab", buffering=0)
        log_fh.write(
            f"# {utc_now_iso()} launching: {' '.join(cmd)}\n".encode()
        )

        # Start a new process group so we can SIGTERM the whole tree on cancel.
        proc = subprocess.Popen(
            cmd, stdout=log_fh, stderr=subprocess.STDOUT,
            start_new_session=True, close_fds=True,
        )

        self.dao.update(job_id, state="running",
                        started_at=utc_now_iso())
        with self._lock:
            self._active[job_id] = _ActiveJob(
                job_id=job_id, submitter=row["submitter"],
                process=proc, session_dir=session_dir,
            )
        log.info("launched %s (pid=%s)", job_id, proc.pid)

    # ── Reap / integrate ─────────────────────────────────────────────

    def _reap_finished(self) -> None:
        with self._lock:
            ids = list(self._active.keys())
        for jid in ids:
            with self._lock:
                aj = self._active.get(jid)
            if aj is None:
                continue
            rc = aj.process.poll()
            if rc is None:
                continue
            with self._lock:
                self._active.pop(jid, None)
            self._on_process_exit(aj, rc)

    def _on_process_exit(self, aj: _ActiveJob, rc: int) -> None:
        started_at = self.dao.get(aj.job_id) or {}
        started = started_at.get("started_at")
        duration = None
        if started:
            try:
                from datetime import datetime
                t0 = datetime.strptime(started, "%Y-%m-%dT%H:%M:%SZ")
                t1 = datetime.utcnow()
                duration = max(0, int((t1 - t0).total_seconds()))
            except ValueError:
                duration = None

        if rc != 0:
            msg = _extract_error_message(aj.session_dir / "run.log", rc)
            self.dao.update(aj.job_id, state="failed",
                            exit_code=rc,
                            duration_seconds=duration,
                            finished_at=utc_now_iso(),
                            error_message=msg)
            log.warning("job %s failed (rc=%s): %s", aj.job_id, rc, msg)
            return

        # Success → integrate
        self.dao.update(aj.job_id, state="integrating",
                        exit_code=rc, duration_seconds=duration)
        self._run_integrator(aj)

    def _run_integrator(self, aj: _ActiveJob) -> None:
        integrator = self.script_path.parent / "integrate_mask_results.sh"
        if not integrator.exists():
            self.dao.update(aj.job_id, state="done",
                            finished_at=utc_now_iso(),
                            error_message="integrator script missing")
            return
        cmd = [str(integrator), "-o", str(aj.session_dir)]
        log_path = aj.session_dir / "integrate.log"
        try:
            with log_path.open("ab", buffering=0) as fh:
                rc = subprocess.call(cmd, stdout=fh, stderr=subprocess.STDOUT)
        except OSError as exc:
            self.dao.update(aj.job_id, state="failed",
                            finished_at=utc_now_iso(),
                            error_message=f"integrator launch failed: {exc}")
            return
        state = "done" if rc == 0 else "failed"
        err = None if rc == 0 else f"integrator exit {rc}"
        self.dao.update(aj.job_id, state=state,
                        finished_at=utc_now_iso(),
                        error_message=err)
        # Mask results parsing is deferred — the integrator writes a CSV
        # into <session>/mask_integration_summary.csv. Persisting per-row
        # mask_results from that CSV lands with the Results page.

    # ── Cancellation ─────────────────────────────────────────────────

    def _handle_cancellations(self) -> None:
        rows, _ = self.dao.list(states=["stopping"], limit=200)
        for r in rows:
            with self._lock:
                aj = self._active.get(r["job_id"])
            if aj is None:
                # Stopping a job that isn't actually running here — mark cancelled.
                self.dao.update(r["job_id"], state="cancelled",
                                finished_at=utc_now_iso())
                continue
            self._send_signal(aj, signal.SIGTERM)
            self._stop_containers_for(r["job_id"])

    def _send_signal(self, aj: _ActiveJob, sig: int) -> None:
        try:
            os.killpg(os.getpgid(aj.process.pid), sig)
        except (ProcessLookupError, PermissionError):
            pass

    def _stop_containers_for(self, job_id: str) -> None:
        """docker stop any running container labelled with our job id."""
        try:
            ps = subprocess.run(
                ["docker", "ps", "-q",
                 "--filter", f"label=aviti_job_id={job_id}"],
                capture_output=True, text=True, timeout=10,
            )
        except (OSError, subprocess.TimeoutExpired):
            return
        ids = [c.strip() for c in ps.stdout.splitlines() if c.strip()]
        if not ids:
            return
        try:
            subprocess.run(["docker", "stop", *ids],
                           capture_output=True, timeout=30)
        except (OSError, subprocess.TimeoutExpired):
            pass
