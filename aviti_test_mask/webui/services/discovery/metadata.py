"""Run metadata extraction — scalar fields + JSON blobs from a run folder.

Reads RunParameters.json, RunManifest.json, AvitiRunStats.json,
RunUploaded.json. Returns the dict consumed by ``RunsMetadataDAO.upsert``,
or ``None`` when the folder is missing the bits we need.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json
from pathlib import Path


def _samples_to_per_lane_projects(samples: list[dict]) -> dict[str, list[str]]:
    """``{lane: [project, ...]}`` aggregated from Manifest.Samples."""
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
    """Compact per-sample-per-lane projection of Manifest.Samples."""
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
    """Pull every scalar / JSON field we want in ``runs_metadata``.

    Returns ``None`` when the run folder lacks the required JSON files
    or the instrument UUID — never raises.
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
    """Return the ISO-8601 start timestamp from RunParameters.json, or None."""
    rp = run_path / "RunParameters.json"
    try:
        data = json.loads(rp.read_text())
    except (OSError, json.JSONDecodeError):
        return None
    value = data.get("Date")
    if not isinstance(value, str):
        return None
    return value
