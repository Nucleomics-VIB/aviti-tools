"""Discover and validate AVITI run folders under the NAS root.

Two pure functions:
- ``scan_nas_for_runs(cfg)``: list candidate run folders that match the
  expected naming pattern, newest first, with cheap filesystem checks.
- ``validate_run(path, cfg)``: deep-ish validation that the run folder
  is complete enough for ``bases2fastq`` to process. Returns a result
  dict matching the schema in ``dev_docs/plan_webui.md``.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json
import re
import struct
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterator

from config_loader import WebUIConfig

ZIP_MAGIC = b"PK\x03\x04"
EXPECTED_TOP_LEVEL = (
    "RunManifest.json",
    "RunParameters.json",
    "RunAnalysisFilesUploaded.json",
    "AvitiRunStats.json",
)
EXPECTED_DIRS = ("BaseCalls", "Filter", "Location", "Alignment")


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


# Run-folder name shape: YYYYMMDD_AV<digits>_<project tokens joined by _>_<runseq>
# Project numbers are 4-digit by default; non-4-digit tokens indicate test or
# maintenance runs (e.g. "TEST_Mock1", "upgradepv-a").
# Examples (observed on the dev NAS):
#   20260322_AV224503_5246_2       → projects: ["5246"]                    (real)
#   20260427_AV224503_5255_5261_1  → projects: ["5255", "5261"]            (multi)
#   20260331_AV224503_TEST_Mock1_A → projects: []  is_test: True
#   20260401_AV224503_upgradepv-a  → projects: []  is_test: True
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
    """True when the folder name has no 4-digit project token.

    Covers both shape-matching folders with non-numeric tokens
    (``TEST_*``) and folders that don't match the standard shape at all
    (``upgradepv-a``, ``ConnectionTest``).
    """
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


def _expected_zip_set(cycles: dict[str, int]) -> set[str]:
    return {
        f"{read}_C{n:03d}.zip"
        for read, count in cycles.items()
        for n in range(1, int(count) + 1)
    }


def _zip_magic_ok(path: Path) -> bool:
    try:
        with path.open("rb") as fh:
            return fh.read(4) == ZIP_MAGIC
    except OSError:
        return False


def validate_run(run_path: Path, cfg: WebUIConfig) -> dict:
    """Deep validation of a single candidate run folder.

    Cheap checks short-circuit on first failure. Returns a structured
    dict; the caller decides whether to surface invalid runs separately.
    """
    started = time.monotonic()
    checks: list[dict] = []

    def add(name: str, ok: bool, **extra) -> bool:
        checks.append({"name": name, "ok": ok, **extra})
        return ok

    for fname in EXPECTED_TOP_LEVEL:
        if not add(fname, (run_path / fname).is_file()):
            return _result(checks, started)
    for dname in EXPECTED_DIRS:
        if not add(f"{dname}/", (run_path / dname).is_dir()):
            return _result(checks, started)

    rp_path = run_path / "RunParameters.json"
    try:
        rp = json.loads(rp_path.read_text())
    except (OSError, json.JSONDecodeError) as exc:
        add("RunParameters.json parses", False, error=str(exc))
        return _result(checks, started)
    add("RunParameters.json parses", True)

    cycles = rp.get("Cycles")
    if not isinstance(cycles, dict) or not cycles:
        add("RunParameters.Cycles", False, error="missing or empty Cycles dict")
        return _result(checks, started)
    expected = _expected_zip_set(cycles)
    add("RunParameters.Cycles", True, cycles=cycles, expected_zip_count=len(expected))

    basecalls = run_path / "BaseCalls"
    try:
        actual_zips = {p.name for p in basecalls.iterdir() if p.suffix == ".zip"}
    except OSError as exc:
        add("BaseCalls listing", False, error=str(exc))
        return _result(checks, started)
    missing = expected - actual_zips
    if missing:
        add("BaseCalls zip set", False,
            expected=len(expected), actual=len(actual_zips),
            missing_count=len(missing),
            missing_sample=sorted(missing)[:5])
        return _result(checks, started)
    add("BaseCalls zip set", True, expected=len(expected), actual=len(actual_zips))

    empty: list[str] = []
    for name in expected:
        try:
            if (basecalls / name).stat().st_size <= 0:
                empty.append(name)
        except OSError:
            empty.append(name)
    if empty:
        add("BaseCalls zip sizes", False, empty_count=len(empty),
            empty_sample=empty[:5])
        return _result(checks, started)
    add("BaseCalls zip sizes", True)

    if cfg.deep_validate:
        bad: list[str] = []
        for name in expected:
            if not _zip_magic_ok(basecalls / name):
                bad.append(name)
        if bad:
            add("BaseCalls zip magic", False, bad_count=len(bad),
                bad_sample=bad[:5])
            return _result(checks, started)
        add("BaseCalls zip magic", True)
    else:
        add("BaseCalls zip magic", True, skipped=True,
            reason="deep_validate=false")

    tiles = rp.get("Tiles")
    if isinstance(tiles, list):
        add("RunParameters.Tiles", True, count=len(tiles),
            sample=tiles[:3] if tiles else [])

    return _result(checks, started, meta={
        "cycles": cycles,
        "tile_count": len(tiles) if isinstance(tiles, list) else None,
        "analysis_lanes": rp.get("AnalysisLanes"),
        "read_order": rp.get("ReadOrder"),
    })


def _result(checks: list[dict], started_at: float, meta: dict | None = None) -> dict:
    first_failure = next((c for c in checks if not c["ok"]), None)
    valid = first_failure is None
    return {
        "valid": valid,
        "checks": checks,
        "first_failure": first_failure,
        "duration_ms": int((time.monotonic() - started_at) * 1000),
        "meta": meta or {},
    }


def iter_validated(cfg: WebUIConfig) -> Iterator[RunCandidate]:
    candidates, _ = scan_nas_for_runs(cfg)
    for cand in candidates:
        result = validate_run(cand.path, cfg)
        cand.is_valid = result["valid"]
        cand.first_failure = (
            result["first_failure"]["name"] if result["first_failure"] else None
        )
        cand.meta = result["meta"]
        yield cand
