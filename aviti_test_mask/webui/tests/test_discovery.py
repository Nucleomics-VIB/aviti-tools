"""Discovery + validation tests against a synthetic NAS layout.

These run without the real NAS mount so they're safe in CI.
"""
from __future__ import annotations

import json
import zipfile
from pathlib import Path
from types import SimpleNamespace

import pytest

from services.discovery import (
    check_nas_mount, extract_projects_from_run_id, is_test_run,
    resolve_tile_spec, scan_nas_for_runs, validate_run,
)


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


def test_extract_projects_single():
    assert extract_projects_from_run_id("20260322_AV224503_5246_2") == ["5246"]


def test_extract_projects_multi():
    assert extract_projects_from_run_id("20260427_AV224503_5255_5261_1") == ["5255", "5261"]


def test_extract_projects_skips_non_4digit():
    # TEST runs have alpha tokens — should not be treated as projects
    assert extract_projects_from_run_id("20260331_AV224503_TEST_Mock1_A") == []


def test_extract_projects_unmatched_shape():
    assert extract_projects_from_run_id("20260401_AV224503_upgradepv-a") == []
    assert extract_projects_from_run_id("ConnectionTest") == []


def test_is_test_run():
    assert not is_test_run("20260322_AV224503_5246_2")
    assert is_test_run("20260331_AV224503_TEST_Mock1_A")
    assert is_test_run("20260401_AV224503_upgradepv-a")
    assert is_test_run("ConnectionTest")


def _write_tiles(run_path: Path, tiles: list[str]):
    (run_path).mkdir(parents=True, exist_ok=True)
    (run_path / "RunParameters.json").write_text(
        json.dumps({"Tiles": tiles, "Cycles": {"R1": 1}})
    )


def test_check_nas_mount_missing(tmp_path):
    cfg = make_config(tmp_path / "does-not-exist")
    res = check_nas_mount(cfg)
    assert res["ok"] is False
    assert res["status"] == "missing"


def test_check_nas_mount_empty_no_sequencers(tmp_path):
    cfg = make_config(tmp_path)
    # NAS root exists but no AV* dirs
    res = check_nas_mount(cfg)
    assert res["ok"] is False
    assert res["status"] == "empty"


def test_check_nas_mount_empty_sequencer_without_runs(tmp_path):
    cfg = make_config(tmp_path)
    (tmp_path / "AV224503").mkdir()
    res = check_nas_mount(cfg)
    assert res["ok"] is False
    assert res["status"] == "empty"
    assert res["sequencer_count"] == 1


def test_check_nas_mount_ok(tmp_path):
    cfg = make_config(tmp_path)
    seq = tmp_path / "AV224503"
    seq.mkdir()
    make_run(seq, "20260522_AV224503_5279_1", {"R1": 1})
    res = check_nas_mount(cfg)
    assert res["ok"] is True
    assert res["status"] == "ok"
    assert res["sequencer_count"] == 1
    assert res["sample_run_count"] >= 1


def test_resolve_tile_spec_default_single_lane(tmp_path):
    _write_tiles(tmp_path, ["L1R02C01S1", "L1R01C01S1"])
    out = resolve_tile_spec(tmp_path, "default")
    # Pattern still None — script keeps omitting --include-tile.
    assert out["pattern"] is None
    assert out["spec"] == "default"
    # Predicted picks: first tile of each lane (sorted).
    assert out["tiles"] == ["L1R01C01S1"]
    assert out["count"] == 1


def test_resolve_tile_spec_default_multi_lane(tmp_path):
    _write_tiles(tmp_path, ["L1R02C01S1", "L2R01C01S1",
                            "L1R01C01S1", "L2R02C01S1"])
    out = resolve_tile_spec(tmp_path, "default")
    assert out["pattern"] is None
    assert out["tiles"] == ["L1R01C01S1", "L2R01C01S1"]


def test_resolve_tile_spec_default_no_manifest_falls_back(tmp_path):
    _write_tiles(tmp_path, [])
    out = resolve_tile_spec(tmp_path, "default")
    assert out == {"spec": "default", "pattern": None, "tiles": [], "count": 0}


def test_resolve_tile_spec_all(tmp_path):
    _write_tiles(tmp_path, ["L1R01C01S1", "L2R03C04S1"])
    out = resolve_tile_spec(tmp_path, "all")
    assert out["pattern"] == "L.R..C..S."
    assert out["spec"] == "all"
    # Full tile inventory now travels with the row for UI display.
    assert sorted(out["tiles"]) == ["L1R01C01S1", "L2R03C04S1"]
    assert out["count"] == 2


def test_resolve_tile_spec_lane(tmp_path):
    _write_tiles(tmp_path, ["L1R01C01S1"])
    out = resolve_tile_spec(tmp_path, "lane", tiles_lane=2)
    assert out["pattern"] == "L2R..C..S."
    assert out["spec"] == "lane:2"


def test_resolve_tile_spec_spread_deterministic(tmp_path):
    tiles = [f"L1R{r:02d}C01S1" for r in range(1, 11)]  # 10 tiles
    _write_tiles(tmp_path, tiles)
    out1 = resolve_tile_spec(tmp_path, "spread", tiles_n=3)
    out2 = resolve_tile_spec(tmp_path, "spread", tiles_n=3)
    assert out1["tiles"] == out2["tiles"]  # spread is deterministic
    assert len(out1["tiles"]) == 3
    assert "|" in out1["pattern"]
    assert all(t in tiles for t in out1["tiles"])


def test_resolve_tile_spec_random_size_and_pattern(tmp_path):
    tiles = [f"L1R{r:02d}C01S1" for r in range(1, 11)]
    _write_tiles(tmp_path, tiles)
    out = resolve_tile_spec(tmp_path, "random", tiles_n=4)
    assert len(out["tiles"]) == 4
    assert out["pattern"] == "|".join(out["tiles"])
    assert all(t in tiles for t in out["tiles"])


def test_resolve_tile_spec_random_caps_at_total(tmp_path):
    _write_tiles(tmp_path, ["L1R01C01S1", "L1R02C01S1"])
    out = resolve_tile_spec(tmp_path, "random", tiles_n=10)
    assert len(out["tiles"]) == 2  # capped at the actual inventory


def test_resolve_tile_spec_lane_filter(tmp_path):
    tiles = ["L1R01C01S1", "L1R02C01S1", "L2R01C01S1", "L2R02C01S1"]
    _write_tiles(tmp_path, tiles)
    out = resolve_tile_spec(tmp_path, "random", tiles_n=10, lanes="2")
    assert {t[1] for t in out["tiles"]} == {"2"}


def test_resolve_tile_spec_raw(tmp_path):
    _write_tiles(tmp_path, [])
    out = resolve_tile_spec(tmp_path, "raw", tiles_raw="L1R..C..S.")
    assert out["pattern"] == "L1R..C..S."


def test_resolve_tile_spec_raw_rejects_empty(tmp_path):
    _write_tiles(tmp_path, [])
    with pytest.raises(ValueError):
        resolve_tile_spec(tmp_path, "raw", tiles_raw="")


def test_resolve_tile_spec_unknown_mode_raises(tmp_path):
    _write_tiles(tmp_path, [])
    with pytest.raises(ValueError):
        resolve_tile_spec(tmp_path, "bogus")


def test_validate_missing_dir(tmp_path):
    seq = tmp_path / "AV224503"
    seq.mkdir()
    run = make_run(seq, "20260522_AV224503_5279_1",
                   {"R1": 1}, skip_dirs={"Filter"})
    cfg = make_config(tmp_path)
    result = validate_run(run, cfg)
    assert result["valid"] is False
    assert result["first_failure"]["name"] == "Filter/"
