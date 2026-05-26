"""Discovery + validation tests against a synthetic NAS layout.

These run without the real NAS mount so they're safe in CI.
"""
from __future__ import annotations

import json
import zipfile
from pathlib import Path
from types import SimpleNamespace

import pytest

from discovery import scan_nas_for_runs, validate_run


def make_config(tmp_path: Path, deep_validate=False) -> SimpleNamespace:
    return SimpleNamespace(
        nas_root=tmp_path,
        sequencer_subdirs_glob="AV*",
        run_folder_marker="RunManifest.json",
        run_folder_regex=r"^\d{8}_AV\d+_\d+_\d+$",
        run_age_days=3650,
        deep_validate=deep_validate,
    )


def make_run(parent: Path, name: str, cycles: dict[str, int], *,
             missing: set[str] | None = None,
             empty: set[str] | None = None,
             skip_dirs: set[str] | None = None) -> Path:
    """Build a synthetic run folder mimicking the AVITI layout."""
    run = parent / name
    run.mkdir(parents=True)
    for f in ("RunManifest.json", "RunAnalysisFilesUploaded.json", "AvitiRunStats.json"):
        (run / f).write_text("{}")
    rp = {"Cycles": cycles, "Tiles": ["L1R06C01S1", "L1R06C01S2"]}
    (run / "RunParameters.json").write_text(json.dumps(rp))
    skip = skip_dirs or set()
    for d in ("BaseCalls", "Filter", "Location", "Alignment"):
        if d in skip:
            continue
        (run / d).mkdir()
    missing = missing or set()
    empty = empty or set()
    for read, count in cycles.items():
        for n in range(1, count + 1):
            name = f"{read}_C{n:03d}.zip"
            if name in missing:
                continue
            path = run / "BaseCalls" / name
            if name in empty:
                path.touch()
                continue
            with zipfile.ZipFile(path, "w") as zf:
                zf.writestr("dummy.bin", b"x" * 8)
    return run


def test_scan_finds_valid_name(tmp_path):
    seq = tmp_path / "AV224503"
    seq.mkdir()
    make_run(seq, "20260522_AV224503_5279_1", {"R1": 1})
    cfg = make_config(tmp_path)
    runs, warns = scan_nas_for_runs(cfg)
    assert warns == []
    assert len(runs) == 1
    assert runs[0].run_id == "20260522_AV224503_5279_1"
    assert runs[0].sequencer == "AV224503"


def test_scan_skips_non_matching_names(tmp_path):
    seq = tmp_path / "AV224503"
    seq.mkdir()
    (seq / "not_a_run").mkdir()
    (seq / "cleaning.log").write_text("x")
    cfg = make_config(tmp_path)
    runs, _ = scan_nas_for_runs(cfg)
    assert runs == []


def test_scan_handles_missing_nas(tmp_path):
    cfg = make_config(tmp_path / "does_not_exist")
    runs, warns = scan_nas_for_runs(cfg)
    assert runs == []
    assert any("does not exist" in w for w in warns)


def test_validate_complete_run(tmp_path):
    seq = tmp_path / "AV224503"
    seq.mkdir()
    run = make_run(seq, "20260522_AV224503_5279_1",
                   {"R1": 2, "R2": 2, "I1": 1, "I2": 1})
    cfg = make_config(tmp_path)
    result = validate_run(run, cfg)
    assert result["valid"] is True
    assert result["first_failure"] is None


def test_validate_missing_zip(tmp_path):
    seq = tmp_path / "AV224503"
    seq.mkdir()
    run = make_run(seq, "20260522_AV224503_5279_1",
                   {"R1": 3}, missing={"R1_C002.zip"})
    cfg = make_config(tmp_path)
    result = validate_run(run, cfg)
    assert result["valid"] is False
    assert result["first_failure"]["name"] == "BaseCalls zip set"
    assert result["first_failure"]["missing_count"] == 1


def test_validate_empty_zip(tmp_path):
    seq = tmp_path / "AV224503"
    seq.mkdir()
    run = make_run(seq, "20260522_AV224503_5279_1",
                   {"R1": 2}, empty={"R1_C001.zip"})
    cfg = make_config(tmp_path)
    result = validate_run(run, cfg)
    assert result["valid"] is False
    assert result["first_failure"]["name"] == "BaseCalls zip sizes"


def test_validate_missing_dir(tmp_path):
    seq = tmp_path / "AV224503"
    seq.mkdir()
    run = make_run(seq, "20260522_AV224503_5279_1",
                   {"R1": 1}, skip_dirs={"Filter"})
    cfg = make_config(tmp_path)
    result = validate_run(run, cfg)
    assert result["valid"] is False
    assert result["first_failure"]["name"] == "Filter/"
