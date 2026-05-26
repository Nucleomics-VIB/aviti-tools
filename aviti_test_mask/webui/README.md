# aviti_test_mask — web UI

Backend foundation for the web UI described in
[../dev_docs/plan_webui.md](../dev_docs/plan_webui.md).

Part of **aviti_test_mask** — VIB Nucleomics Core.
Author: Stephane Plaisance ([stephane.plaisance@vib.be](mailto:stephane.plaisance@vib.be))

## v1 scope

Read-only HTTP API:
- `GET /api/v1/health` — server liveness + NAS mount status.
- `GET /api/v1/config` — current webui_config.yaml.
- `GET /api/v1/users` — users.yaml as JSON.
- `GET /api/v1/masks` — built-in masks from `../masks.yaml`.
- `GET /api/v1/runs` — candidate runs (name + mtime only, fast).
- `GET /api/v1/runs/validated` — same list, each run deep-validated.

Job submission, queue, history, and HTML pages land in subsequent
iterations.

## Setup

Hard rule: **every** dependency — Python packages and unix tools alike —
lives in a dedicated conda env. The host is assumed to provide only
`bash` and `docker`; anything else (`parallel`, `awk`, `sed`, `jq`,
`yq`, `realpath`, …) ships from `environment.yml`. Adding a new tool
means adding it to the env, never `apt install`-ing on the host.

```bash
cd webui
make env                    # one-time
conda activate aviti_test_mask_webui
make run                    # http://127.0.0.1:8765/api/v1/health
```

Override the config path:

```bash
AVITI_WEBUI_CONFIG=/path/to/custom.yaml make run
```

## Tests

```bash
make test
```

Discovery tests build synthetic AVITI run trees in `tmp_path`, so they
run without a real NAS. The DB tests use SQLite in `tmp_path` and don't
need any external service.

## Layout

```
webui/
  environment.yml        # conda env spec
  Makefile               # env / run / test wrappers
  webui_config.yaml      # dev defaults (Mac NAS mount)
  users.yaml             # lab-member allowlist
  app.py                 # Flask routes
  config_loader.py       # WebUIConfig dataclass
  discovery.py           # scan + validate
  db.py                  # SQLite DAO (schema v1)
  masks_loader.py        # builtin / uploaded / typed mask parsing
  users_loader.py        # users.yaml parsing
  tests/                 # pytest suite
```
