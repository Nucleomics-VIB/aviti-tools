"""Per-run validation — the 6-layer chain.

Top-level marker files, RunParameters.Cycles parsing, BaseCalls zip
inventory + size, optional zip-magic deep check. Cheap probes
short-circuit on the first failure.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json
import time
from pathlib import Path
from typing import Iterator

from ..config_loader import WebUIConfig
from .scan import RunCandidate, scan_nas_for_runs

ZIP_MAGIC = b"PK\x03\x04"
EXPECTED_TOP_LEVEL = (
    "RunManifest.json",
    "RunParameters.json",
    "RunAnalysisFilesUploaded.json",
    "AvitiRunStats.json",
)
EXPECTED_DIRS = ("BaseCalls", "Filter", "Location", "Alignment")


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


def _result(checks: list[dict], started_at: float,
            meta: dict | None = None) -> dict:
    first_failure = next((c for c in checks if not c["ok"]), None)
    return {
        "valid": first_failure is None,
        "checks": checks,
        "first_failure": first_failure,
        "duration_ms": int((time.monotonic() - started_at) * 1000),
        "meta": meta or {},
    }


def validate_run(run_path: Path, cfg: WebUIConfig) -> dict:
    """Deep validation of a single candidate run folder."""
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
        add("RunParameters.Cycles", False,
            error="missing or empty Cycles dict")
        return _result(checks, started)
    expected = _expected_zip_set(cycles)
    add("RunParameters.Cycles", True, cycles=cycles,
        expected_zip_count=len(expected))

    basecalls = run_path / "BaseCalls"
    try:
        actual_zips = {p.name for p in basecalls.iterdir()
                       if p.suffix == ".zip"}
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
    add("BaseCalls zip set", True,
        expected=len(expected), actual=len(actual_zips))

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


def iter_validated(cfg: WebUIConfig) -> Iterator[RunCandidate]:
    candidates, _ = scan_nas_for_runs(cfg)
    for cand in candidates:
        result = validate_run(cand.path, cfg)
        cand.is_valid = result["valid"]
        cand.first_failure = (
            result["first_failure"]["name"]
            if result["first_failure"] else None
        )
        cand.meta = result["meta"]
        yield cand
