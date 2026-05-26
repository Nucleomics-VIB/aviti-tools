"""Load and validate the webui configuration.

`webui_config.yaml` is read once at app startup into an immutable dataclass.
Paths are resolved relative to the config file's location.
"""
from __future__ import annotations

import os
from dataclasses import dataclass, field
from pathlib import Path

import yaml


@dataclass(frozen=True)
class WebUIConfig:
    nas_root: Path
    sequencer_subdirs_glob: str
    run_folder_marker: str
    run_folder_regex: str
    run_age_days: int
    deep_validate: bool

    results_root: Path
    jobs_dir: Path
    db_path: Path

    max_global_containers: int
    max_concurrent_jobs: int
    max_jobs_per_user: int
    max_inner_jobs: int
    threads: int

    host: str
    port: int

    retain_jobs_days: int
    retain_jobs_min_keep: int
    purge_on_each_request: bool

    users_file: Path
    masks_file: Path

    raw: dict = field(default_factory=dict)


def _resolve(base: Path, value: str) -> Path:
    p = Path(value).expanduser()
    return p if p.is_absolute() else (base / p).resolve()


def load(path: str | Path) -> WebUIConfig:
    cfg_path = Path(path).expanduser().resolve()
    base = cfg_path.parent
    with cfg_path.open() as fh:
        raw = yaml.safe_load(fh) or {}

    required = [
        "nas_root", "sequencer_subdirs_glob", "run_folder_marker",
        "run_folder_regex", "run_age_days", "results_root", "jobs_dir",
        "db_path", "max_global_containers", "max_concurrent_jobs",
        "max_jobs_per_user", "max_inner_jobs", "threads", "host", "port",
        "retain_jobs_days", "retain_jobs_min_keep", "purge_on_each_request",
        "users_file", "masks_file",
    ]
    missing = [k for k in required if k not in raw]
    if missing:
        raise ValueError(f"webui_config.yaml missing required keys: {missing}")

    cfg = WebUIConfig(
        nas_root=_resolve(base, raw["nas_root"]),
        sequencer_subdirs_glob=raw["sequencer_subdirs_glob"],
        run_folder_marker=raw["run_folder_marker"],
        run_folder_regex=raw["run_folder_regex"],
        run_age_days=int(raw["run_age_days"]),
        deep_validate=bool(raw.get("deep_validate", False)),
        results_root=_resolve(base, raw["results_root"]),
        jobs_dir=_resolve(base, raw["jobs_dir"]),
        db_path=_resolve(base, raw["db_path"]),
        max_global_containers=int(raw["max_global_containers"]),
        max_concurrent_jobs=int(raw["max_concurrent_jobs"]),
        max_jobs_per_user=int(raw["max_jobs_per_user"]),
        max_inner_jobs=int(raw["max_inner_jobs"]),
        threads=int(raw["threads"]),
        host=str(raw["host"]),
        port=int(raw["port"]),
        retain_jobs_days=int(raw["retain_jobs_days"]),
        retain_jobs_min_keep=int(raw["retain_jobs_min_keep"]),
        purge_on_each_request=bool(raw["purge_on_each_request"]),
        users_file=_resolve(base, raw["users_file"]),
        masks_file=_resolve(base, raw["masks_file"]),
        raw=raw,
    )

    if len(cfg.results_root.parts) < 3:
        raise ValueError(
            f"results_root too shallow ({cfg.results_root}); refusing for safety."
        )
    cfg.results_root.mkdir(parents=True, exist_ok=True)
    cfg.db_path.parent.mkdir(parents=True, exist_ok=True)
    return cfg


def env_config_path() -> Path:
    return Path(os.environ.get("AVITI_WEBUI_CONFIG", Path(__file__).parent / "webui_config.yaml"))
