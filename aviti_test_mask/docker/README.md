# Docker deployment

Production-ready image + compose for the **aviti_test_mask** web UI.
Build artefacts only — this folder does not contain runtime state.

Part of **aviti_test_mask** — VIB Nucleomics Core.
Author: Stephane Plaisance ([stephane.plaisance@vib.be](mailto:stephane.plaisance@vib.be))

## Files in this folder

| File | What it is |
|---|---|
| `Dockerfile` | Ubuntu 22.04 + miniforge → conda env from `webui/environment.yml` + docker CLI (for docker-out-of-docker) + non-root user + tini. |
| `entrypoint.sh` | Activates the conda env and `exec`s the Flask app. |
| `docker-compose.yml` | Wires the image to host port **11006**, two data bind mounts, the docker socket, and a config bind mount. |
| `.dockerignore` | Keeps the build context small; excludes `results/`, tests, dev caches. |
| `webui_config.prod.yaml` | Sample production config (paths inside container, 3/3/8 concurrency). Copy to `config/webui_config.yaml` next to `docker-compose.yml` before `up`. |

## Runtime architecture

Pattern: **docker-out-of-docker, not docker-in-docker.**

```text
+---------------------------+         +-------------------------+
|  aviti_test_mask_webui    |  exec   |   host docker daemon    |
|  (this container)         | <-----> |  /var/run/docker.sock   |
|                           |         +-------------------------+
|  Flask app  ----+         |                   |
|  worker thread  |         |                   v
|                 +-> docker run elembio/bases2fastq:latest ...
|                            (SIBLING container on host)
+---------------------------+
```

The webui spawns one sibling container per mask via the host daemon.
Path identity matters: when the webui tells the host daemon
`-v /data/results/<job>:/output`, the daemon resolves that path on
the **host**. So `/data/results` (and `/data/nas`) must exist on the
host and be mapped 1:1 into this container (see compose volumes).

## Volume layout

| Container path | Host source | Mode | Owner of data |
|---|---|---|---|
| `/data/nas/` | host NAS mount (eg. `/mnt/lvs/...` or a CIFS/sshfs mount point) | **ro** | sequencer |
| `/data/results/` | host directory writable by `APP_UID` | **rw** | webui (session folders + `jobs.db`) |
| `/app/webui/config/` | `./config/` (next to `docker-compose.yml`) | ro | operator (deploy-time tunables) |
| `/var/run/docker.sock` | host docker socket | rw | plumbing |
| `/app/scripts/`, `/app/webui/{services,routes,templates,static}/` | image-baked | ro | release artefact |

The two user-facing data mounts mirror what's running on the Mac dev
setup right now (NAS ro + project-local results rw) — same contract,
different host paths.

## Build / run

```bash
# From the project root, with NAS at /mnt/lvs and a results dir at /data/results:
cd docker
mkdir -p config && cp webui_config.prod.yaml config/webui_config.yaml
# (edit config/users.yaml as needed — see ../webui/config/users.yaml for the schema)

NAS_ROOT=/mnt/lvs \
RESULTS_ROOT=/data/results \
DOCKER_GID=$(getent group docker | cut -d: -f3) \
APP_UID=$(id -u aviti) APP_GID=$(id -g aviti) \
docker compose build

NAS_ROOT=/mnt/lvs RESULTS_ROOT=/data/results docker compose up -d
curl http://localhost:11006/api/v1/health
```

The four environment variables above are all that the compose file
expects. Defaults: `NAS_ROOT=/data/nas`, `RESULTS_ROOT=/data/results`,
`APP_UID=APP_GID=1000`, `DOCKER_GID=999`.

## Stop / update

```bash
docker compose pull             # if pulling an external tag
docker compose build --pull     # rebuild from local source
docker compose up -d            # zero-downtime restart (compose drains old)
docker compose logs -f webui
docker compose down             # stop everything
```

`jobs.db` and session folders persist on the host bind mount, so
`down` / `up` does not lose history.

## Healthcheck

The image declares `HEALTHCHECK` against `/api/v1/health`. `docker
compose ps` reports `(healthy)` once worker, docker daemon and NAS
checks all return `ok`. CI / monitoring can use the same endpoint.

## Things NOT in this image

- The `elembio/bases2fastq` image — pulled lazily by the worker on
  first mask launch. To pre-warm: `docker pull elembio/bases2fastq:latest`
  on the host before starting the webui.
- HTTPS termination — front this with nginx / Caddy / your existing
  reverse proxy if exposing outside the host.
- Authentication — not yet implemented (`plan_webui.md` deferred it).

## Updating the conda env

`environment.yml` changes invalidate the env layer (slow). Code-only
changes only rebuild the COPY layers (fast). Bump the image tag in
`docker-compose.yml` when you ship.
