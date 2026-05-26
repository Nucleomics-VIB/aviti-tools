"""Tests for the per-mask results endpoints and static file serving.

These wire a minimal Flask app with only the routes needed (no worker,
no YAML config), pointing at a synthesised on-disk
``results/<job_id>/qc_runs/<safe_mask>/`` tree.
"""
from __future__ import annotations

import json
from pathlib import Path
from types import SimpleNamespace

from flask import Flask

from routes.api_jobs import bp as bp_api_jobs
from routes.pages import bp as bp_pages
from services.db import JobsDAO, JobRecord, utc_now_iso


JOB_ID = "test-job-1"
SAFE_MASK = "mask_0_Y12N"


def _make_app(tmp_path: Path) -> Flask:
    results_root = tmp_path / "results"
    results_root.mkdir()
    # Build the mask folder with a token of each artifact.
    mask_dir = results_root / JOB_ID / "qc_runs" / SAFE_MASK
    (mask_dir / "Reports").mkdir(parents=True)
    (mask_dir / "info").mkdir()
    (mask_dir / "RunStats.json").write_text(json.dumps({
        "RunID": "fake-run",
        "Lanes": [
            {"Lane": 1, "PercentPF": 91.2, "PercentQ30": 92.5,
             "NumPolonies": 123456789},
        ],
    }))
    (mask_dir / "Metrics.csv").write_text(
        "Lane,Tile,Yield\n1,T101,99.0\n1,T102,98.5\n")
    (mask_dir / "Reports" / "report.html").write_text(
        "<html><body><h1>fake report</h1></body></html>")
    (mask_dir / "info" / "Bases2Fastq.log").write_text("ok\n")

    cfg = SimpleNamespace(results_root=results_root)
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(JobRecord(
        job_id=JOB_ID, submitter="alice",
        run_id="20260522_AV_999", run_path="/tmp/run",
        params_json="{}", masks_source="builtin", masks_json="[]",
        state="done", cache_input=0, threads=4, max_jobs=1,
        docker_image="elembio/bases2fastq:latest",
        submitted_at=utc_now_iso(), mask_count=1,
    ))

    app = Flask(__name__)
    app.config["WEBUI_CONFIG"] = cfg
    app.config["DAO"] = dao
    app.register_blueprint(bp_pages)
    app.register_blueprint(bp_api_jobs)
    return app


def test_mask_runstats_endpoint(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()

    r = client.get(f"/api/v1/jobs/{JOB_ID}/masks/{SAFE_MASK}/runstats")
    assert r.status_code == 200
    data = r.get_json()
    assert data["RunID"] == "fake-run"
    assert data["Lanes"][0]["PercentQ30"] == 92.5

    # Missing mask folder → 404.
    r = client.get(f"/api/v1/jobs/{JOB_ID}/masks/nope_no_such/runstats")
    assert r.status_code == 404


def test_mask_metrics_and_files_endpoints(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()

    r = client.get(f"/api/v1/jobs/{JOB_ID}/masks/{SAFE_MASK}/metrics")
    assert r.status_code == 200
    j = r.get_json()
    assert j["columns"] == ["Lane", "Tile", "Yield"]
    assert j["row_count"] == 2

    r = client.get(f"/api/v1/jobs/{JOB_ID}/masks/{SAFE_MASK}/files")
    assert r.status_code == 200
    j = r.get_json()
    paths = sorted(f["path"] for f in j["files"])
    assert "Reports/report.html" in paths
    assert "RunStats.json" in paths
    assert "info/Bases2Fastq.log" in paths


def test_list_mask_folders_endpoint(tmp_path):
    app = _make_app(tmp_path)
    client = app.test_client()
    r = client.get(f"/api/v1/jobs/{JOB_ID}/masks")
    assert r.status_code == 200
    j = r.get_json()
    assert j["total"] == 1
    f = j["folders"][0]
    assert f["folder"] == SAFE_MASK
    assert f["has_runstats"] is True
    assert f["has_metrics"] is True
    assert f["has_log"] is True
    assert f["report"] == "report.html"


def test_mask_files_safe_serving(tmp_path):
    """Static file route serves real files and rejects traversal."""
    app = _make_app(tmp_path)
    client = app.test_client()

    # Legitimate file is served.
    r = client.get(
        f"/results/{JOB_ID}/files/{SAFE_MASK}/Reports/report.html")
    assert r.status_code == 200
    assert b"fake report" in r.data

    # Nested legitimate file.
    r = client.get(
        f"/results/{JOB_ID}/files/{SAFE_MASK}/info/Bases2Fastq.log")
    assert r.status_code == 200
    assert b"ok" in r.data

    # Traversal in safe_mask is rejected at the URL converter level by
    # Flask's default (no slashes in <safe_mask>), but a literal ".."
    # should also fail.
    r = client.get(
        f"/results/{JOB_ID}/files/../Reports/report.html")
    assert r.status_code in (404, 308)

    # Traversal inside filename is rejected.
    r = client.get(
        f"/results/{JOB_ID}/files/{SAFE_MASK}/../../../../etc/passwd")
    assert r.status_code == 404

    # Non-existent mask folder → 404.
    r = client.get(
        f"/results/{JOB_ID}/files/nope_no_such/anything.txt")
    assert r.status_code == 404
