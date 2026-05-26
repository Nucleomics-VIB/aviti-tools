# Docker deployment — planned layout

This folder is reserved for the eventual `Dockerfile`, `docker-compose.yml`,
and entrypoint scripts. The current refactor only prepares the directory
structure so that adding the image later requires zero file moves.

Part of **aviti_test_mask** — VIB Nucleomics Core.
Author: Stephane Plaisance ([stephane.plaisance@vib.be](mailto:stephane.plaisance@vib.be))

## Volume layout (planned)

| Container path | Host source | Mode | Purpose |
|---|---|---|---|
| `/app/scripts/` | image-baked from `../scripts/` | ro | Bash scripts (`aviti_test_mask.sh`, `integrate_mask_results.sh`) + their `config.yaml`/`masks.yaml`. Immutable per release. |
| `/app/webui/services/` | image-baked | ro | Domain Python package. |
| `/app/webui/routes/` | image-baked | ro | Flask blueprints. |
| `/app/webui/templates/`, `/app/webui/static/` | image-baked | ro | Views and assets. |
| `/app/webui/config/` | host bind-mount | ro | `webui_config.yaml` + `users.yaml`. Deploy-time tunables. |
| `/data/nas/` | host bind-mount | ro | NAS share with the AVITI run folders. |
| `/data/results/` | host bind-mount | rw | Session folders + `jobs.db`. |
| `/var/run/docker.sock` | host bind-mount | rw | Docker-out-of-docker — the webui spawns `bases2fastq` containers as siblings on the host daemon. |

## Base image

Ubuntu (per project policy — no Mac-specific assumptions in production
artifacts). The Dockerfile installs the conda env from `webui/environment.yml`,
copies `scripts/` and `webui/{services,routes,templates,static,app.py}`,
exposes the Flask port, runs as a non-root user, and uses
`HEALTHCHECK` against `/api/v1/health`.

## Production `webui_config.yaml` (sketch)

```yaml
nas_root: /data/nas
results_root: /data/results
jobs_dir: /data/results
db_path: /data/results/jobs.db
scripts_dir: /app/scripts
host: 0.0.0.0
port: 8765

# Tuned for a real Ubuntu host (vs the Mac-dev defaults shipped at HEAD)
max_global_containers: 3
max_concurrent_jobs: 3
max_jobs_per_user: 1
max_inner_jobs: 3
threads: 8
```

## Build / run (planned commands — not yet implemented)

```bash
cd docker
docker compose build
docker compose up -d
curl http://localhost:8765/api/v1/health
```

The Stage 2 commit that lands the Dockerfile will fill in the rest;
this README locks in the contract so reviewers know what to expect.
