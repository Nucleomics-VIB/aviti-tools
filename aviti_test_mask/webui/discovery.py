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


def _samples_to_per_lane_projects(samples: list[dict]) -> dict[str, list[str]]:
    """Return ``{lane: [project, ...]}`` aggregated from Manifest.Samples."""
    out: dict[str, set[str]] = {}
    for s in samples:
        project = (s.get("Project") or "").strip()
        if not project:
            continue
        for idx in s.get("Indexes", []):
            lane = idx.get("Lane")
            if lane is None:
                continue
            out.setdefault(str(lane), set()).add(project)
    return {lane: sorted(v) for lane, v in out.items()}


def _summarise_samples(samples: list[dict]) -> list[dict]:
    """Compact, per-sample-per-lane projection of Manifest.Samples."""
    out: list[dict] = []
    for s in samples:
        name = s.get("SampleName")
        project = s.get("Project")
        for idx in s.get("Indexes", []):
            out.append({
                "sample": name,
                "lane": idx.get("Lane"),
                "project": project,
                "index1": idx.get("Index1"),
                "index2": idx.get("Index2"),
            })
    return out


def read_run_metadata(run_path: Path) -> dict | None:
    """Pull the scalar + JSON fields needed to populate ``runs_metadata``.

    Returns ``None`` when the run folder lacks the required JSON files
    or the instrument UUID — never raises. The caller decides whether
    to upsert.
    """
    rp_path = run_path / "RunParameters.json"
    rm_path = run_path / "RunManifest.json"
    stats_path = run_path / "AvitiRunStats.json"
    uploaded_path = run_path / "RunUploaded.json"

    try:
        rp = json.loads(rp_path.read_text())
    except (OSError, json.JSONDecodeError):
        return None
    run_internal_id = rp.get("RunID")
    if not run_internal_id:
        return None

    manifest_text: str | None = None
    samples: list[dict] = []
    try:
        manifest_text = rm_path.read_text()
        manifest = json.loads(manifest_text)
        if isinstance(manifest, dict):
            samples = manifest.get("Samples") or []
    except (OSError, json.JSONDecodeError):
        manifest_text = None

    run_stats: dict = {}
    try:
        stats = json.loads(stats_path.read_text())
        if isinstance(stats, dict):
            run_stats = stats.get("RunStats") or {}
    except (OSError, json.JSONDecodeError):
        pass

    outcome: str | None = None
    try:
        u = json.loads(uploaded_path.read_text())
        outcome = u.get("outcome") if isinstance(u, dict) else None
    except (OSError, json.JSONDecodeError):
        pass

    cycles = rp.get("Cycles") or {}
    fields = {
        "run_id": run_path.name,
        "run_path": str(run_path),
        "run_start": rp.get("Date"),
        "instrument": rp.get("InstrumentName"),
        "side": rp.get("Side"),
        "flowcell_id": rp.get("FlowcellID"),
        "run_name": rp.get("RunName"),
        "run_type": rp.get("RunType"),
        "run_description": rp.get("RunDescription"),
        "operator_name": rp.get("OperatorName"),
        "throughput": rp.get("ThroughputSelection"),
        "kit_config": rp.get("KitConfiguration"),
        "chemistry_version": rp.get("ChemistryVersion"),
        "platform_version": rp.get("PlatformVersion"),
        "library_type": rp.get("LibraryType"),
        "low_diversity": 1 if rp.get("LowDiversity") else 0,
        "analysis_lanes": rp.get("AnalysisLanes"),
        "polony_count": run_stats.get("PolonyCount"),
        "pf_count": run_stats.get("PFCount"),
        "percent_pf": run_stats.get("PercentPF"),
        "total_yield": run_stats.get("TotalYield"),
        "outcome": outcome,
        "cycles_json": json.dumps(cycles),
        "samples_json": json.dumps(_summarise_samples(samples)),
        "lane_projects_json": json.dumps(_samples_to_per_lane_projects(samples)),
        "manifest_json": manifest_text,
        "run_parameters_json": rp_path.read_text() if rp_path.exists() else None,
    }
    return {"run_internal_id": run_internal_id, "fields": fields}


def read_run_start(run_path: Path) -> str | None:
    """Return the ISO-8601 start timestamp from RunParameters.json, or None.

    The AVITI instrument writes a precise ``Date`` field at run start, e.g.
    ``"2026-05-22T14:38:03.570108863Z"``. Cheap to parse — used to enrich
    the run listing without committing to a full ``validate_run`` pass.
    """
    rp = run_path / "RunParameters.json"
    try:
        data = json.loads(rp.read_text())
    except (OSError, json.JSONDecodeError):
        return None
    value = data.get("Date")
    if not isinstance(value, str):
        return None
    return value


def _read_tiles_list(run_path: Path) -> list[str]:
    """Return RunParameters.Tiles as a list of strings (empty on failure)."""
    try:
        rp = json.loads((run_path / "RunParameters.json").read_text())
    except (OSError, json.JSONDecodeError):
        return []
    tiles = rp.get("Tiles")
    return [str(t) for t in tiles] if isinstance(tiles, list) else []


def _spread_pick(items: list[str], n: int) -> list[str]:
    """Evenly-spaced N picks across a sorted list — deterministic."""
    if n <= 0 or not items:
        return []
    if n >= len(items):
        return list(items)
    step = len(items) / n
    return [items[int(i * step)] for i in range(n)]


def resolve_tile_spec(
    run_path: Path,
    tiles_mode: str,
    *,
    tiles_n: int = 3,
    tiles_lane: int | None = None,
    tiles_raw: str | None = None,
    lanes: str = "all",
) -> dict:
    """Translate a form-level tile spec into a concrete bases2fastq pattern.

    Resolution happens at submit time so the queued row carries the exact
    pattern that will run; the worker doesn't re-resolve. For ``random``,
    this means the random pick is locked in at submission — Re-submit
    (📋) draws a fresh pick because it lands in a new submission.

    Returns ``{"spec": <human label>, "pattern": <include-tile arg or None>,
    "tiles": <chosen tile list>, "count": <int>}``.
    """
    tiles_mode = (tiles_mode or "default").strip().lower()
    lane_filter: set[str] | None = None
    if lanes in ("1", "2"):
        lane_filter = {lanes}

    def filter_by_lane(items: list[str]) -> list[str]:
        if lane_filter is None:
            return items
        return [t for t in items if t and t[0] == "L" and t[1] in lane_filter]

    if tiles_mode == "default":
        return {"spec": "default", "pattern": None, "tiles": [], "count": 0}

    if tiles_mode == "all":
        # bases2fastq default regex for "every tile in every analysed lane".
        pat = "L.R..C..S."
        return {"spec": "all", "pattern": pat, "tiles": [], "count": -1}

    if tiles_mode == "lane":
        lane = int(tiles_lane or 1)
        return {"spec": f"lane:{lane}", "pattern": f"L{lane}R..C..S.",
                "tiles": [], "count": -1}

    if tiles_mode == "raw":
        raw = (tiles_raw or "").strip()
        if not raw:
            raise ValueError("raw tile pattern is empty")
        return {"spec": "raw", "pattern": raw, "tiles": [], "count": -1}

    if tiles_mode in ("spread", "random"):
        all_tiles = filter_by_lane(_read_tiles_list(run_path))
        if not all_tiles:
            raise ValueError("RunParameters.Tiles missing; cannot pick tiles")
        n = max(1, int(tiles_n))
        if tiles_mode == "spread":
            picked = _spread_pick(sorted(all_tiles), n)
        else:
            import random as _r
            n = min(n, len(all_tiles))
            picked = _r.sample(all_tiles, n)
        return {
            "spec": f"{tiles_mode}:{len(picked)}",
            "pattern": "|".join(picked),
            "tiles": picked,
            "count": len(picked),
        }

    raise ValueError(f"unknown tiles_mode: {tiles_mode!r}")


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
