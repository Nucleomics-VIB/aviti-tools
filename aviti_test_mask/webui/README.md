# aviti_test_mask — web UI

Web UI for the AVITI mask-suite QC tool described in
[../dev_docs/plan_webui.md](../dev_docs/plan_webui.md).

Part of **aviti_test_mask** — VIB Nucleomics Core.
Author: Stephane Plaisance ([stephane.plaisance@vib.be](mailto:stephane.plaisance@vib.be))

## Setup

Hard rule: **every** dependency — Python packages and unix tools alike —
lives in a dedicated conda env. The host provides only `bash` and
`docker`; anything else (`parallel`, `gawk`, `sed`, `jq`, `yq`,
`coreutils`, …) ships from `environment.yml`.

```bash
cd webui
make env                       # one-time
conda activate aviti_test_mask_webui
make run                       # http://127.0.0.1:8765/
```

Override the config path:

```bash
AVITI_WEBUI_CONFIG=/path/to/custom.yaml make run
```

## Tests

```bash
make test
```

## Layout

```
webui/
  app.py                       # slim Flask entry — create_app() + worker boot
  Makefile                     # env / run / test wrappers
  environment.yml              # conda env spec (Python + unix tools)
  config/                      # bind-mount target in Docker
    webui_config.yaml          # tunables (NAS path, concurrency, retention, …)
    users.yaml                 # lab-member allowlist
  services/                    # pure domain logic (no Flask imports)
    config_loader.py           # WebUIConfig dataclass + YAML loader
    db.py                      # SQLite DAOs (JobsDAO, RunsMetadataDAO)
    job_worker.py              # background thread spawning aviti_test_mask.sh
    masks_loader.py            # builtin / uploaded / typed mask parsing
    users_loader.py            # users.yaml parsing
    discovery/                 # NAS scan + validation + metadata + tile resolver
      scan.py                  # scan_nas_for_runs, check_nas_mount, …
      validation.py            # validate_run, iter_validated
      metadata.py              # read_run_metadata, read_run_start
      tiles.py                 # resolve_tile_spec
  routes/                      # Flask blueprints (HTTP layer only)
    pages.py                   # /, /about, /queue, /submit/<id>, /resubmit/<id>
    api_misc.py                # /api/v1/health|config|users|masks
    api_runs.py                # /api/v1/runs[/<id>|/validated]
    api_jobs.py                # /api/v1/queue + /api/v1/jobs/*
  templates/                   # Jinja2 (extends base.html)
  static/                      # css / js / VIB logos
  tests/                       # pytest suite (39 tests)
```

Bash scripts (`aviti_test_mask.sh`, `integrate_mask_results.sh`) and
their YAMLs live in `../scripts/` at the project root. The worker
resolves them via `WebUIConfig.scripts_dir`.

The empty `../docker/` folder holds the planned Dockerfile +
compose layout — see [`../docker/README.md`](../docker/README.md).
