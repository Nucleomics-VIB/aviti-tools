# aviti_test_mask web UI — production image.
#
# Layered so the conda env (slow, large) caches on rebuild while the
# Python source layer rebuilds quickly.
#
# Pattern: docker-out-of-docker. This container holds the webui + the
# bash helpers + a docker CLI. The Flask worker shells out to
# /usr/bin/docker which talks to the HOST daemon (via the
# /var/run/docker.sock bind-mount in docker-compose.yml). bases2fastq
# therefore runs as a SIBLING container on the host, not nested.
#
# Part of aviti_test_mask — VIB Nucleomics Core.
# Author: Stephane Plaisance <stephane.plaisance@vib.be>

FROM ubuntu:22.04

# Avoid tzdata prompts; use UTF-8 everywhere.
ENV DEBIAN_FRONTEND=noninteractive \
    LC_ALL=C.UTF-8 \
    LANG=C.UTF-8 \
    PATH=/opt/conda/bin:$PATH

# UID/GID can be overridden at build time to match the host user that
# owns the bind-mounted results directory. Set both to the same value
# as the host's webui owner to avoid chown dances.
ARG APP_UID=1000
ARG APP_GID=1000

# --- 1. OS layer: minimum host tools + docker CLI ----------------------
# Only bash and the docker client need to be installed by apt — every
# other tool the bash scripts use ships in the conda env (see
# environment.yml).
RUN apt-get update && apt-get install -y --no-install-recommends \
        bash \
        ca-certificates \
        curl \
        wget \
        gnupg \
        bzip2 \
        tini \
    && install -m 0755 -d /etc/apt/keyrings \
    && curl -fsSL https://download.docker.com/linux/ubuntu/gpg \
       | gpg --dearmor -o /etc/apt/keyrings/docker.gpg \
    && chmod a+r /etc/apt/keyrings/docker.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] \
            https://download.docker.com/linux/ubuntu \
            $(. /etc/os-release && echo $VERSION_CODENAME) stable" \
       > /etc/apt/sources.list.d/docker.list \
    && apt-get update && apt-get install -y --no-install-recommends \
        docker-ce-cli \
    && rm -rf /var/lib/apt/lists/*

# --- 2. Conda layer: install miniforge + create the env from yml -------
# Using miniforge (conda-forge default channel — matches environment.yml).
RUN wget -qO /tmp/miniforge.sh \
      https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-$(uname -m).sh \
    && bash /tmp/miniforge.sh -b -p /opt/conda \
    && rm /tmp/miniforge.sh \
    && conda clean -afy

# Resolve the env before COPY-ing the rest so layer cache stays hot
# across code-only rebuilds.
COPY webui/environment.yml /tmp/environment.yml
RUN conda env create -f /tmp/environment.yml \
    && conda clean -afy \
    && rm /tmp/environment.yml

# --- 3. Non-root app user ----------------------------------------------
RUN groupadd --system --gid ${APP_GID} aviti \
    && useradd  --system --uid ${APP_UID} --gid ${APP_GID} \
                --create-home --shell /bin/bash aviti

# --- 4. Application code (last so iteration is fast) -------------------
WORKDIR /app
COPY --chown=aviti:aviti scripts/ /app/scripts/
COPY --chown=aviti:aviti webui/  /app/webui/
COPY --chown=aviti:aviti docker/entrypoint.sh /app/entrypoint.sh
RUN chmod +x /app/entrypoint.sh /app/scripts/*.sh

# Drop privileges. The bind-mounted /data/results must be writable by
# UID ${APP_UID} on the host.
USER aviti
WORKDIR /app/webui

# Internal port — the host port is decided in docker-compose.yml.
EXPOSE 8765

# Probe the existing /api/v1/health endpoint. Marks unhealthy if the
# worker has died or the NAS check fails.
HEALTHCHECK --interval=30s --timeout=10s --start-period=20s --retries=3 \
  CMD curl -fsS http://127.0.0.1:8765/api/v1/health \
        | grep -q '"status": "ok"' || exit 1

# tini reaps zombies from the docker CLI children the worker spawns.
ENTRYPOINT ["/usr/bin/tini", "--", "/app/entrypoint.sh"]
