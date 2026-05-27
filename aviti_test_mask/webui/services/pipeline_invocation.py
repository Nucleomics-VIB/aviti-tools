"""Python ↔ bash contract for the aviti_test_mask pipeline.

Single module owning every fact the worker needs to know about
``scripts/aviti_test_mask.sh`` and ``scripts/integrate_mask_results.sh``:

- How to build the shell command (CLI flags, including the
  ``--include-tile`` + ``--exclude-tile`` pairing required by
  bases2fastq).
- The on-disk handover files (``masks.yaml``, ``run.log``,
  ``mask_integration_summary.csv``) and their formats.
- The log markers the script emits (``✅``, ``❌``, ``📊``) and how to
  parse them.
- The environment variables the script reads (``CONDA_ENV_NAME``).

Before this module existed, the same contract was scattered across
``job_worker.py``, ``routes/pages.py``, ``services/discovery/tiles.py``,
and the two bash scripts — change one bash line, three Python sites
break silently. Now: one place, with fixtures.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import csv
import os
import re
from dataclasses import dataclass
from pathlib import Path


# ── Filenames the script writes / we write for the script ────────────

MASKS_FILE_NAME = "masks.yaml"
LOG_FILE_NAME = "run.log"
INTEGRATOR_LOG_NAME = "integrate.log"
INTEGRATOR_CSV_NAME = "mask_integration_summary.csv"

# ── bases2fastq tile arguments ───────────────────────────────────────

# Per Element Biosciences docs, ``--include-tile`` does NOT restrict on
# its own — you must first ``--exclude-tile`` every tile, then re-include
# the ones you want. This catch-all matches every tile ID.
EXCLUDE_TILE_ALL = "L.R..C..S."

# ── Log markers emitted by aviti_test_mask.sh ────────────────────────

PROGRESS_OK_PREFIX = "✅ ["
PROGRESS_FAIL_PREFIX = "❌ ["
SUMMARY_PREFIX = "📊"
# Match ``📊 9/9 succeeded  |  0 failed`` (any whitespace).
_SUMMARY_RE = re.compile(
    r"^📊\s*(\d+)\s*/\s*(\d+)\s+succeeded\s*\|\s*(\d+)\s+failed"
)

# ── Env vars passed to the script ────────────────────────────────────


def script_env(conda_env_name: str) -> dict[str, str]:
    """Environment overlay for the spawned bash script."""
    return {**os.environ, "CONDA_ENV_NAME": conda_env_name}


# ── Outbound: command + masks file ───────────────────────────────────


def write_masks_file(session_dir: Path, masks: list[str]) -> Path:
    """Persist the resolved mask list as the YAML the script reads."""
    path = session_dir / MASKS_FILE_NAME
    with path.open("w") as fh:
        fh.write("masks:\n")
        for m in masks:
            fh.write(f'  - "{m}"\n')
    return path


def build_script_command(
    script_path: Path,
    *,
    run_path: str,
    session_dir: Path,
    masks_file: Path,
    threads: int | str,
    max_jobs: int | str,
    job_id: str,
    tile_pattern: str | None = None,
    mem_limit: str | None = None,
    cache_input: bool = False,
) -> list[str]:
    """Assemble the argv for ``aviti_test_mask.sh``.

    When ``tile_pattern`` is set, automatically pairs it with
    ``--exclude-tile L.R..C..S.`` — this is the rule that fixes the
    `--include-tile alone does not restrict` bug.
    """
    cmd: list[str] = [
        str(script_path),
        "-i", run_path,
        "-o", str(session_dir),
        "-m", str(masks_file),
        "-p", str(threads),
        "-j", str(max_jobs),
        "--job-id", job_id,
    ]
    if tile_pattern:
        cmd += ["--include-tile", tile_pattern,
                "--exclude-tile", EXCLUDE_TILE_ALL]
    if mem_limit:
        cmd += ["--mem-limit", mem_limit]
    if cache_input:
        cmd += ["--cache-input"]
    return cmd


# ── Inbound: log parsing ─────────────────────────────────────────────


@dataclass(frozen=True)
class ProgressCounts:
    succeeded: int
    failed: int


def count_progress(log_text: str) -> ProgressCounts:
    """Count per-mask ✅ / ❌ markers in a (possibly tailed) log buffer.

    The script never repeats per-mask result lines, so counts stay
    accurate even when called against the last 32 KB of a long log.
    """
    ok = 0
    bad = 0
    for line in log_text.splitlines():
        if line.startswith(PROGRESS_OK_PREFIX) and "completed" in line:
            ok += 1
        elif line.startswith(PROGRESS_FAIL_PREFIX) and (
            "FAILED" in line or "KILLED" in line
        ):
            bad += 1
    return ProgressCounts(succeeded=ok, failed=bad)


def infer_exit_from_summary(log_text: str) -> int | None:
    """Read the trailing ``📊 X/N succeeded | F failed`` line.

    Returns 0 if F == 0, 1 if F > 0, ``None`` if the summary marker is
    absent (the container exited before the script's summary line was
    written). Used by reattach to infer the script's exit code when the
    original Popen handle is gone.
    """
    for line in reversed(log_text.splitlines()):
        m = _SUMMARY_RE.match(line)
        if m:
            return 0 if int(m.group(3)) == 0 else 1
    return None


def extract_error_message(log_text: str, rc: int, *,
                          max_chars: int = 500) -> str:
    """Pull the most informative line from a failed run.log.

    Prefers, in order:
    1. A line containing the ❌ failure marker.
    2. A line starting with ``Error:`` / ``error:`` / ``Traceback``.
    3. The last non-empty line.
    """
    lines = [ln.rstrip() for ln in log_text.splitlines() if ln.strip()]
    if not lines:
        return f"script exit {rc}"
    msg: str | None = None
    for ln in lines:
        if "❌" in ln:
            msg = ln
    if msg is None:
        for ln in lines:
            low = ln.lower()
            if low.startswith("error") or low.startswith("traceback"):
                msg = ln
    if msg is None:
        msg = lines[-1]
    msg = msg.strip()
    if len(msg) > max_chars:
        msg = msg[:max_chars] + "…"
    return f"[exit {rc}] {msg}"


def read_log_tail(log_path: Path, *, tail_bytes: int = 32 * 1024) -> str:
    """Read the last ``tail_bytes`` of a log file; empty string on error."""
    try:
        size = log_path.stat().st_size
        with log_path.open("rb") as fh:
            if size > tail_bytes:
                fh.seek(-tail_bytes, 2)
            return fh.read().decode("utf-8", errors="replace")
    except OSError:
        return ""


# ── Inbound: integrator CSV ──────────────────────────────────────────


@dataclass(frozen=True)
class MaskResultRow:
    mask: str
    q30_pct: float | None
    assigned_pct: float | None
    score: float | None
    source: str | None


def _to_float(v: str | None) -> float | None:
    if v is None or v == "":
        return None
    try:
        return float(v)
    except ValueError:
        return None


def read_integrator_csv(session_dir: Path) -> list[MaskResultRow]:
    """Parse ``<session>/mask_integration_summary.csv``.

    Returns an empty list if the CSV is missing or unreadable. Rows
    with empty ``Mask`` are skipped.
    """
    path = session_dir / INTEGRATOR_CSV_NAME
    if not path.exists():
        return []
    try:
        with path.open("r", encoding="utf-8", newline="") as fh:
            raw_rows = list(csv.DictReader(fh))
    except OSError:
        return []
    out: list[MaskResultRow] = []
    for r in raw_rows:
        mask = (r.get("Mask") or "").strip()
        if not mask:
            continue
        out.append(MaskResultRow(
            mask=mask,
            q30_pct=_to_float(r.get("Q30%")),
            assigned_pct=_to_float(r.get("%Assigned")),
            score=_to_float(r.get("Score")),
            source=(r.get("Source") or None),
        ))
    return out
