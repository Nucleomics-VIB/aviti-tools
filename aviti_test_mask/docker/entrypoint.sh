#!/usr/bin/env bash
# Container entrypoint — activate the conda env then exec the Flask app.
#
# Pure exec (no background processes) so PID 1 is Flask, and tini
# (declared in the Dockerfile) reaps any docker-CLI children the worker
# spawns when launching bases2fastq.
#
# Part of aviti_test_mask — VIB Nucleomics Core.
# Author: Stephane Plaisance <stephane.plaisance@vib.be>

set -euo pipefail

# shellcheck disable=SC1091
source /opt/conda/etc/profile.d/conda.sh
conda activate aviti_test_mask_webui

# Default config: bind-mounted at /app/webui/config/webui_config.yaml
# but the loader accepts override via AVITI_WEBUI_CONFIG.
export AVITI_WEBUI_CONFIG="${AVITI_WEBUI_CONFIG:-/app/webui/config/webui_config.yaml}"

cd /app/webui
exec python app.py
