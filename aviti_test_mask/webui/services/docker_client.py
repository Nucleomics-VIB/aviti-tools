"""Thin façade over the ``docker`` CLI used by the job worker.

The worker used to issue ``subprocess.run(['docker', ...])`` from four
different methods, each duplicating the timeout / OSError / non-zero-rc
boilerplate. This module owns the contract:

- ``find_containers_for_job(job_id)`` — by aviti_job_id label
- ``inspect_status(container_id)``    — running / exited / missing
- ``daemon_info()``                   — preflight check
- ``stop_containers(container_ids)``  — graceful stop

Every method returns a small typed value and never raises on the
expected failure modes; the caller decides what counts as "ok".

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import subprocess
from dataclasses import dataclass


# Default timeouts in seconds. The 30 s on stop accommodates docker's
# own 10 s graceful-stop window + slack.
INSPECT_TIMEOUT = 10
INFO_TIMEOUT = 10
STOP_TIMEOUT = 30


@dataclass(frozen=True)
class DaemonInfo:
    """Result of ``docker info`` — used by preflight checks."""
    ok: bool
    version: str | None = None
    error: str | None = None


class DockerClient:
    """All docker-CLI calls the worker makes.

    Methods take a ``runner`` only when overridden in tests — the
    default is :func:`subprocess.run`. Keeping the indirection on the
    class (rather than scattered across the worker) means tests need
    one patch site instead of four.
    """

    def __init__(self, runner=subprocess.run):
        self._run = runner

    # ── Container discovery ──────────────────────────────────────────

    def find_containers_for_job(self, job_id: str) -> list[str]:
        """Return container IDs tagged with ``aviti_job_id=<job_id>``.

        Empty list on any failure (daemon down, docker missing, label
        not present). The caller treats empty as "no container".
        """
        try:
            r = self._run(
                ["docker", "ps", "-q",
                 "--filter", f"label=aviti_job_id={job_id}"],
                capture_output=True, text=True, timeout=INSPECT_TIMEOUT,
            )
        except (OSError, subprocess.TimeoutExpired):
            return []
        return [ln.strip() for ln in (r.stdout or "").splitlines()
                if ln.strip()]

    # ── Container state ──────────────────────────────────────────────

    def inspect_status(self, container_id: str) -> str:
        """Return the docker container state string.

        One of ``running``, ``created``, ``restarting``, ``paused``,
        ``exited``, ``dead``, or ``unknown`` (covers
        timeout / OSError / non-zero rc / missing container).
        """
        try:
            r = self._run(
                ["docker", "inspect", "-f", "{{.State.Status}}",
                 container_id],
                capture_output=True, text=True, timeout=INSPECT_TIMEOUT,
            )
        except (OSError, subprocess.TimeoutExpired):
            return "unknown"
        return (r.stdout or "").strip() or "unknown"

    # ── Daemon preflight ─────────────────────────────────────────────

    def daemon_info(self) -> DaemonInfo:
        """Probe the docker daemon. Returns a DaemonInfo with ok=True
        plus the server version, or ok=False plus a one-line error."""
        try:
            r = self._run(
                ["docker", "info", "--format", "{{.ServerVersion}}"],
                capture_output=True, text=True, timeout=INFO_TIMEOUT,
            )
        except FileNotFoundError:
            return DaemonInfo(ok=False, error="docker CLI not on PATH")
        except subprocess.TimeoutExpired:
            return DaemonInfo(ok=False,
                              error="docker daemon unreachable (timeout)")
        except OSError as exc:
            return DaemonInfo(ok=False, error=f"docker invoke failed: {exc}")
        if r.returncode != 0:
            tail = (r.stderr or r.stdout or "").strip().splitlines()
            msg = tail[-1] if tail else "docker info failed"
            return DaemonInfo(ok=False,
                              error=f"docker daemon unreachable: {msg}")
        return DaemonInfo(ok=True, version=(r.stdout or "").strip() or None)

    # ── Stop ─────────────────────────────────────────────────────────

    def stop_containers(self, container_ids: list[str]) -> None:
        """Best-effort ``docker stop``. Silently absorbs failures —
        the caller already issued a SIGTERM to the bash parent."""
        if not container_ids:
            return
        try:
            self._run(["docker", "stop", *container_ids],
                       capture_output=True, timeout=STOP_TIMEOUT)
        except (OSError, subprocess.TimeoutExpired):
            pass
