"""Tests for the python↔bash contract module.

These tests pin every fact the worker needs to know about the bash
script's input/output — change one bash line, these tests will tell
you which python sites need updating.
"""
from __future__ import annotations

from pathlib import Path

import pytest

from services import pipeline_invocation as pipeline


# ── build_script_command ─────────────────────────────────────────────


def test_build_command_minimal(tmp_path):
    cmd = pipeline.build_script_command(
        Path("/opt/aviti/aviti_test_mask.sh"),
        run_path="/data/run", session_dir=tmp_path,
        masks_file=tmp_path / "masks.yaml",
        threads=4, max_jobs=2, job_id="JID",
    )
    assert cmd == [
        "/opt/aviti/aviti_test_mask.sh",
        "-i", "/data/run",
        "-o", str(tmp_path),
        "-m", str(tmp_path / "masks.yaml"),
        "-p", "4",
        "-j", "2",
        "--job-id", "JID",
    ]


def test_build_command_tile_pattern_pairs_with_exclude_all(tmp_path):
    """The whole point: --include-tile alone doesn't restrict. The
    worker must always pair it with --exclude-tile L.R..C..S., AND
    exclude must come first per Element's docs (exclude-all wipes the
    default set, then include re-adds the picks)."""
    cmd = pipeline.build_script_command(
        Path("/x"), run_path="/r", session_dir=tmp_path,
        masks_file=tmp_path / "m.yaml",
        threads=4, max_jobs=1, job_id="JID",
        tile_pattern="L1R09C01S1",
    )
    assert "--include-tile" in cmd
    assert "--exclude-tile" in cmd
    exc_idx = cmd.index("--exclude-tile")
    inc_idx = cmd.index("--include-tile")
    assert exc_idx < inc_idx, "exclude must precede include"
    assert cmd[exc_idx + 1] == pipeline.EXCLUDE_TILE_ALL == "L.R..C..S."
    assert cmd[inc_idx + 1] == "L1R09C01S1"


def test_build_command_no_tile_pattern_omits_both(tmp_path):
    cmd = pipeline.build_script_command(
        Path("/x"), run_path="/r", session_dir=tmp_path,
        masks_file=tmp_path / "m.yaml",
        threads=4, max_jobs=1, job_id="JID",
        tile_pattern=None,
    )
    assert "--include-tile" not in cmd
    assert "--exclude-tile" not in cmd


def test_build_command_empty_tile_pattern_treated_as_none(tmp_path):
    cmd = pipeline.build_script_command(
        Path("/x"), run_path="/r", session_dir=tmp_path,
        masks_file=tmp_path / "m.yaml",
        threads=4, max_jobs=1, job_id="JID",
        tile_pattern="",
    )
    assert "--include-tile" not in cmd
    assert "--exclude-tile" not in cmd


def test_build_command_mem_and_cache(tmp_path):
    cmd = pipeline.build_script_command(
        Path("/x"), run_path="/r", session_dir=tmp_path,
        masks_file=tmp_path / "m.yaml",
        threads=4, max_jobs=1, job_id="JID",
        mem_limit="32g", cache_input=True,
    )
    assert cmd[-3:] == ["--mem-limit", "32g", "--cache-input"]


# ── write_masks_file ─────────────────────────────────────────────────


def test_write_masks_file_format(tmp_path):
    path = pipeline.write_masks_file(
        tmp_path, ["R1:Y18N*-R2:Y18N*", "R1:N16Y15N*-R2:Y15N*"]
    )
    assert path == tmp_path / "masks.yaml"
    text = path.read_text()
    assert text == (
        'masks:\n'
        '  - "R1:Y18N*-R2:Y18N*"\n'
        '  - "R1:N16Y15N*-R2:Y15N*"\n'
    )


def test_write_masks_file_empty(tmp_path):
    path = pipeline.write_masks_file(tmp_path, [])
    assert path.read_text() == "masks:\n"


# ── script_env ───────────────────────────────────────────────────────


def test_script_env_includes_conda_env_name(monkeypatch):
    monkeypatch.setenv("UNRELATED", "x")
    env = pipeline.script_env("my_env")
    assert env["CONDA_ENV_NAME"] == "my_env"
    assert env["UNRELATED"] == "x"


# ── count_progress ───────────────────────────────────────────────────


def test_count_progress_counts_ok_and_fail():
    log = (
        "▶ [1/3] mask one\n"
        "✅ [R1:Y18N*-R2:Y18N*] completed\n"
        "❌ [R1:Y12N*-R2:Y12N*] FAILED (exit 1)\n"
        "❌ [R1:Y8N*-R2:Y8N*] KILLED — OOM (exit 137)\n"
        "some unrelated chatter\n"
    )
    out = pipeline.count_progress(log)
    assert out.succeeded == 1
    assert out.failed == 2


def test_count_progress_ignores_non_marker_lines():
    log = "starting...\nnothing happened\ndone\n"
    out = pipeline.count_progress(log)
    assert out.succeeded == 0
    assert out.failed == 0


def test_count_progress_requires_completed_keyword():
    log = "✅ [mask] something else\n"
    assert pipeline.count_progress(log).succeeded == 0


# ── infer_exit_from_summary ──────────────────────────────────────────


def test_infer_exit_summary_all_succeeded():
    log = "stuff\n📊 9/9 succeeded  |  0 failed\n"
    assert pipeline.infer_exit_from_summary(log) == 0


def test_infer_exit_summary_some_failed():
    log = "📊 7/9 succeeded  |  2 failed\n"
    assert pipeline.infer_exit_from_summary(log) == 1


def test_infer_exit_summary_absent_returns_none():
    log = "started\nbut never finished\n"
    assert pipeline.infer_exit_from_summary(log) is None


def test_infer_exit_summary_picks_last_when_multiple():
    log = (
        "📊 3/3 succeeded  |  0 failed\n"
        "📊 1/3 succeeded  |  2 failed\n"
    )
    assert pipeline.infer_exit_from_summary(log) == 1


# ── extract_error_message ────────────────────────────────────────────


def test_extract_error_prefers_failure_marker():
    log = (
        "everything fine until\n"
        "❌ [R1:Y8N*-R2:Y8N*] FAILED (exit 1) — see run.log\n"
        "Trailing line\n"
    )
    msg = pipeline.extract_error_message(log, 1)
    assert msg.startswith("[exit 1]")
    assert "FAILED" in msg


def test_extract_error_falls_back_to_error_keyword():
    log = "Starting\nError: docker daemon refused connection\nbye\n"
    msg = pipeline.extract_error_message(log, 2)
    assert "Error:" in msg


def test_extract_error_falls_back_to_last_line():
    log = "line1\nline2\nfinal line\n"
    msg = pipeline.extract_error_message(log, 3)
    assert msg.endswith("final line")


def test_extract_error_empty_log_uses_rc():
    assert pipeline.extract_error_message("", 5) == "script exit 5"


def test_extract_error_truncates():
    long = "x" * 1000
    log = f"❌ [m] FAILED {long}\n"
    msg = pipeline.extract_error_message(log, 1, max_chars=80)
    assert msg.endswith("…")
    assert len(msg) < 200


# ── read_log_tail ────────────────────────────────────────────────────


def test_read_log_tail_returns_full_when_smaller(tmp_path):
    path = tmp_path / "run.log"
    path.write_text("hello\n")
    assert pipeline.read_log_tail(path) == "hello\n"


def test_read_log_tail_returns_empty_when_missing(tmp_path):
    assert pipeline.read_log_tail(tmp_path / "nope.log") == ""


def test_read_log_tail_seeks_to_tail(tmp_path):
    path = tmp_path / "run.log"
    path.write_text("A" * 50_000 + "BTAIL")
    out = pipeline.read_log_tail(path, tail_bytes=1024)
    assert out.endswith("BTAIL")
    assert len(out) <= 1024


# ── read_integrator_csv ──────────────────────────────────────────────


def _write_csv(session_dir: Path, body: str) -> None:
    (session_dir / pipeline.INTEGRATOR_CSV_NAME).write_text(body)


def test_read_integrator_csv_parses_rows(tmp_path):
    _write_csv(tmp_path,
        "Mask,Q30%,%Assigned,Score,Source\n"
        "R1:Y18N*-R2:Y18N*,92.1,99.4,87.3,bases2fastq\n"
        "R1:Y12N*-R2:Y12N*,89.0,,71.2,bases2fastq\n"
    )
    rows = pipeline.read_integrator_csv(tmp_path)
    assert len(rows) == 2
    assert rows[0].mask == "R1:Y18N*-R2:Y18N*"
    assert rows[0].q30_pct == pytest.approx(92.1)
    assert rows[0].assigned_pct == pytest.approx(99.4)
    assert rows[0].score == pytest.approx(87.3)
    assert rows[0].source == "bases2fastq"
    assert rows[1].assigned_pct is None  # empty cell → None


def test_read_integrator_csv_skips_blank_mask(tmp_path):
    _write_csv(tmp_path,
        "Mask,Q30%,%Assigned,Score,Source\n"
        ",90.0,99.0,80.0,bases2fastq\n"
        "R1:Y8N*-R2:Y8N*,91.0,99.0,82.0,bases2fastq\n"
    )
    rows = pipeline.read_integrator_csv(tmp_path)
    assert [r.mask for r in rows] == ["R1:Y8N*-R2:Y8N*"]


def test_read_integrator_csv_missing_file(tmp_path):
    assert pipeline.read_integrator_csv(tmp_path) == []


def test_read_integrator_csv_bad_float_becomes_none(tmp_path):
    _write_csv(tmp_path,
        "Mask,Q30%,%Assigned,Score,Source\n"
        "R1:Y8N*-R2:Y8N*,n/a,99.0,82.0,bases2fastq\n"
    )
    rows = pipeline.read_integrator_csv(tmp_path)
    assert rows[0].q30_pct is None
    assert rows[0].assigned_pct == pytest.approx(99.0)
