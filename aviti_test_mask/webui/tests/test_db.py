from db import JobsDAO, JobRecord, utc_now_iso

import pytest


def make_record(job_id="job-1", **overrides):
    base = dict(
        job_id=job_id,
        submitter="splaisan",
        run_id="20260522_AV224503_5279_1",
        run_path="/tmp/run",
        params_json="{}",
        masks_source="builtin",
        masks_json="[]",
        state="queued",
        cache_input=0,
        threads=8,
        max_jobs=3,
        docker_image="elembio/bases2fastq:latest",
        submitted_at=utc_now_iso(),
        mask_count=5,
    )
    base.update(overrides)
    return JobRecord(**base)


def test_insert_and_get(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(make_record())
    row = dao.get("job-1")
    assert row["submitter"] == "splaisan"
    assert row["state"] == "queued"
    assert row["mask_count"] == 5


def test_update_state(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(make_record())
    dao.update("job-1", state="running", started_at=utc_now_iso())
    assert dao.get("job-1")["state"] == "running"


def test_invalid_state_rejected(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    with pytest.raises(ValueError):
        dao.insert(make_record(state="bogus"))
    dao.insert(make_record())
    with pytest.raises(ValueError):
        dao.update("job-1", state="bogus")


def test_list_filtering(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(make_record(job_id="a", state="done"))
    dao.insert(make_record(job_id="b", state="failed", submitter="alice"))
    dao.insert(make_record(job_id="c", state="queued"))

    rows, total = dao.list(states=["done", "failed"])
    assert total == 2
    assert {r["job_id"] for r in rows} == {"a", "b"}

    rows, total = dao.list(submitter="alice")
    assert total == 1
    assert rows[0]["job_id"] == "b"


def test_mask_results_cascade(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(make_record())
    dao.add_mask_result("job-1", "R1:Y12N*-R2:Y12N*", q30_pct=92.1, score=88.0)
    stats = dao.stats()
    assert "queued" in stats["by_state"]


def test_soft_delete(tmp_path):
    dao = JobsDAO(tmp_path / "jobs.db")
    dao.insert(make_record(state="done"))
    dao.soft_delete("job-1")
    assert dao.get("job-1")["state"] == "deleted"
