"""Tests for the DockerClient façade.

Each test injects a fake ``runner`` that mimics subprocess.run, so no
real docker daemon is required.
"""
from __future__ import annotations

import subprocess
from dataclasses import dataclass

import pytest

from services.docker_client import DockerClient, DaemonInfo


@dataclass
class FakeResult:
    stdout: str = ""
    stderr: str = ""
    returncode: int = 0


def make_runner(*, stdout="", stderr="", returncode=0, raises=None):
    """Build a fake subprocess.run for injection."""
    calls: list[list[str]] = []

    def runner(cmd, **_kwargs):
        calls.append(list(cmd))
        if raises is not None:
            raise raises
        return FakeResult(stdout=stdout, stderr=stderr, returncode=returncode)

    runner.calls = calls  # attach for assertion
    return runner


# ── find_containers_for_job ──────────────────────────────────────────


def test_find_containers_returns_ids():
    runner = make_runner(stdout="abc123\ndef456\n")
    client = DockerClient(runner=runner)
    ids = client.find_containers_for_job("JID")
    assert ids == ["abc123", "def456"]
    assert runner.calls == [[
        "docker", "ps", "-q",
        "--filter", "label=aviti_job_id=JID",
    ]]


def test_find_containers_empty_when_none():
    client = DockerClient(runner=make_runner(stdout=""))
    assert client.find_containers_for_job("JID") == []


def test_find_containers_returns_empty_on_oserror():
    client = DockerClient(runner=make_runner(raises=OSError("nope")))
    assert client.find_containers_for_job("JID") == []


def test_find_containers_returns_empty_on_timeout():
    client = DockerClient(runner=make_runner(
        raises=subprocess.TimeoutExpired(cmd="docker", timeout=10)))
    assert client.find_containers_for_job("JID") == []


def test_find_containers_ignores_blank_lines():
    client = DockerClient(runner=make_runner(stdout="abc\n\n   \ndef\n"))
    assert client.find_containers_for_job("JID") == ["abc", "def"]


# ── inspect_status ───────────────────────────────────────────────────


def test_inspect_status_running():
    client = DockerClient(runner=make_runner(stdout="running\n"))
    assert client.inspect_status("cid") == "running"


def test_inspect_status_unknown_on_blank():
    client = DockerClient(runner=make_runner(stdout=""))
    assert client.inspect_status("cid") == "unknown"


def test_inspect_status_unknown_on_oserror():
    client = DockerClient(runner=make_runner(raises=OSError()))
    assert client.inspect_status("cid") == "unknown"


def test_inspect_status_unknown_on_timeout():
    client = DockerClient(runner=make_runner(
        raises=subprocess.TimeoutExpired(cmd="docker", timeout=10)))
    assert client.inspect_status("cid") == "unknown"


# ── daemon_info ──────────────────────────────────────────────────────


def test_daemon_info_ok_returns_version():
    client = DockerClient(runner=make_runner(stdout="24.0.7\n"))
    info = client.daemon_info()
    assert info.ok is True
    assert info.version == "24.0.7"
    assert info.error is None


def test_daemon_info_docker_missing():
    client = DockerClient(runner=make_runner(raises=FileNotFoundError()))
    info = client.daemon_info()
    assert info.ok is False
    assert "not on PATH" in info.error


def test_daemon_info_timeout():
    client = DockerClient(runner=make_runner(
        raises=subprocess.TimeoutExpired(cmd="docker", timeout=10)))
    info = client.daemon_info()
    assert info.ok is False
    assert "timeout" in info.error.lower()


def test_daemon_info_non_zero_rc():
    client = DockerClient(runner=make_runner(
        returncode=1, stderr="Cannot connect to the Docker daemon\n"))
    info = client.daemon_info()
    assert info.ok is False
    assert "Cannot connect" in info.error


def test_daemon_info_non_zero_rc_no_stderr_falls_back():
    client = DockerClient(runner=make_runner(returncode=1))
    info = client.daemon_info()
    assert info.ok is False
    assert "docker info failed" in info.error


def test_daemon_info_generic_oserror():
    client = DockerClient(runner=make_runner(raises=OSError("permission denied")))
    info = client.daemon_info()
    assert info.ok is False
    assert "permission denied" in info.error


# ── stop_containers ──────────────────────────────────────────────────


def test_stop_containers_calls_docker_stop():
    runner = make_runner()
    client = DockerClient(runner=runner)
    client.stop_containers(["abc", "def"])
    assert runner.calls == [["docker", "stop", "abc", "def"]]


def test_stop_containers_noop_on_empty_list():
    runner = make_runner()
    client = DockerClient(runner=runner)
    client.stop_containers([])
    assert runner.calls == []


def test_stop_containers_absorbs_oserror():
    client = DockerClient(runner=make_runner(raises=OSError()))
    # Must not raise.
    client.stop_containers(["abc"])


def test_stop_containers_absorbs_timeout():
    client = DockerClient(runner=make_runner(
        raises=subprocess.TimeoutExpired(cmd="docker stop", timeout=30)))
    client.stop_containers(["abc"])  # no raise


# ── DaemonInfo dataclass ─────────────────────────────────────────────


def test_daemon_info_defaults():
    d = DaemonInfo(ok=True)
    assert d.version is None
    assert d.error is None
