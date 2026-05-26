"""NAS scan + run-folder name parsing.

Cheap, stat-only operations: enumerate sequencer directories, list
candidate run folders, parse project numbers out of the folder name,
and probe whether the share looks mounted at all.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import re
import time
from dataclasses import dataclass, field
from pathlib import Path

from ..config_loader import WebUIConfig


@dataclass
class RunCandidate:
    path: Path
    sequencer: str
    run_id: str
    mtime: float
    is_valid: bool = False
    first_failure: str | None = None
    meta: dict = field(default_factory=dict)


def _is_run_folder_name(name: str, regex: re.Pattern) -> bool:
    return bool(regex.match(name))


def check_nas_mount(cfg) -> dict:
    """Quick sanity probe to detect an unmounted / empty NAS share.

    Returns ``{ok, status, message, sequencer_count, sample_run_count}``
    where ``status`` is one of:
    - ``ok``      NAS root exists and contains AV* sequencer dirs with
                  at least one matching run folder beneath them.
    - ``empty``   NAS root exists but no AV* directories or no run
                  folders inside — usually the mount is gone but the
                  mount-point directory still exists (common on macOS
                  when ``/Volumes/<share>`` lingers after eject).
    - ``missing`` NAS root path does not exist at all.

    Designed to run on every UI page load — the operations are stat-only
    (no large listings) so the cost is sub-millisecond on a warm mount
    and bounded by NAS timeout on a cold one.
    """
    nas = cfg.nas_root
    if not nas.exists():
        return {
            "ok": False,
            "status": "missing",
            "message": f"NAS root does not exist: {nas}",
            "sequencer_count": 0,
            "sample_run_count": 0,
        }
    try:
        sequencer_dirs = [p for p in nas.glob(cfg.sequencer_subdirs_glob)
                          if p.is_dir()]
    except OSError as exc:
        return {
            "ok": False,
            "status": "missing",
            "message": f"cannot list NAS root: {exc}",
            "sequencer_count": 0,
            "sample_run_count": 0,
        }
    if not sequencer_dirs:
        return {
            "ok": False,
            "status": "empty",
            "message": (f"NAS root {nas} contains no "
                        f"{cfg.sequencer_subdirs_glob!r} sequencer "
                        "directories — share probably not mounted"),
            "sequencer_count": 0,
            "sample_run_count": 0,
        }
    name_regex = re.compile(cfg.run_folder_regex)
    sample_count = 0
    try:
        for entry in sequencer_dirs[0].iterdir():
            if entry.is_dir() and name_regex.match(entry.name):
                sample_count += 1
                if sample_count >= 1:
                    break
    except OSError:
        pass
    if sample_count == 0:
        return {
            "ok": False,
            "status": "empty",
            "message": (f"{sequencer_dirs[0]} has no run-shaped folders "
                        "— share contents look wrong (unmounted? "
                        "permissions?)"),
            "sequencer_count": len(sequencer_dirs),
            "sample_run_count": 0,
        }
    return {
        "ok": True,
        "status": "ok",
        "message": f"NAS mounted; {len(sequencer_dirs)} sequencer dirs visible",
        "sequencer_count": len(sequencer_dirs),
        "sample_run_count": sample_count,
    }


# Run-folder name shape: YYYYMMDD_AV<digits>_<project tokens joined by _>_<runseq>
# Project numbers are 4-digit by default; non-4-digit tokens indicate test or
# maintenance runs (e.g. "TEST_Mock1", "upgradepv-a").
_PROJECT_REGEX = re.compile(r"^\d{8}_AV\d+_(.+)_[^_]+$")
_PROJECT_NUMBER_RE = re.compile(r"^\d{4}$")


def extract_projects_from_run_id(run_id: str) -> list[str]:
    """Return the 4-digit project numbers found in the run folder name.

    Empty list when no 4-digit token is present (test / maintenance runs).
    Use ``is_test_run`` to distinguish unmatched-shape vs. test runs.
    """
    m = _PROJECT_REGEX.match(run_id)
    if not m:
        return []
    tokens = [tok for tok in m.group(1).split("_") if tok]
    return [tok for tok in tokens if _PROJECT_NUMBER_RE.match(tok)]


def is_test_run(run_id: str) -> bool:
    """True when the folder name has no 4-digit project token."""
    return not extract_projects_from_run_id(run_id)


def scan_nas_for_runs(cfg: WebUIConfig) -> tuple[list[RunCandidate], list[str]]:
    """List candidate runs under the NAS root.

    Returns (candidates, warnings). Candidates are sorted newest first by
    mtime. Validation is *not* performed here — call ``validate_run`` per
    candidate.
    """
    warnings: list[str] = []
    nas = cfg.nas_root

    if not nas.exists():
        return [], [f"nas_root does not exist: {nas}"]
    if not nas.is_dir():
        return [], [f"nas_root is not a directory: {nas}"]

    name_regex = re.compile(cfg.run_folder_regex)
    age_cutoff = time.time() - cfg.run_age_days * 86400

    candidates: list[RunCandidate] = []
    try:
        sequencer_dirs = sorted(nas.glob(cfg.sequencer_subdirs_glob))
    except OSError as exc:
        return [], [f"failed to list nas_root: {exc}"]

    for seq_dir in sequencer_dirs:
        if not seq_dir.is_dir():
            continue
        try:
            for run_dir in seq_dir.iterdir():
                if not run_dir.is_dir():
                    continue
                if not _is_run_folder_name(run_dir.name, name_regex):
                    continue
                try:
                    st = run_dir.stat()
                except OSError as exc:
                    warnings.append(f"cannot stat {run_dir}: {exc}")
                    continue
                if st.st_mtime < age_cutoff:
                    continue
                candidates.append(RunCandidate(
                    path=run_dir,
                    sequencer=seq_dir.name,
                    run_id=run_dir.name,
                    mtime=st.st_mtime,
                ))
        except OSError as exc:
            warnings.append(f"cannot list {seq_dir}: {exc}")
            continue

    candidates.sort(key=lambda r: r.mtime, reverse=True)
    return candidates, warnings
