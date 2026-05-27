#!/usr/bin/env bash
# Local dev launcher: ensures colima is up with the required mounts,
# checks the NAS share, then starts the Flask webui.
#
# Optional flags:
#   --restart   Kill any webui on the port, reap zombie jobs (orphaned
#               aviti_test_mask.sh workers + docker containers labelled
#               aviti_job_id), and clear Python __pycache__ before starting.
#               Use after important code edits.
#   --wipe      Delete the SQLite DB and every analysis result under results/.
#               DESTRUCTIVE — prompts for confirmation unless --yes is given.
#   --yes       Skip the --wipe confirmation prompt.
#   -h|--help   Show this help and exit.
set -euo pipefail

PROJECT_DIR="/Users/u0002316/Documents/GitHub/Nucleomics-VIB/aviti-tools/aviti_test_mask"
RESULTS_DIR="${PROJECT_DIR}/results"
NAS_DIR="/Volumes/lvs"
PYTHON_BIN="/opt/miniconda3/envs/aviti_test_mask_webui/bin/python"
WEBUI_PORT=8765

DO_RESTART=0
DO_WIPE=0
ASSUME_YES=0

usage() { sed -n '2,12p' "$0"; }

while [[ $# -gt 0 ]]; do
  case "$1" in
    --restart) DO_RESTART=1 ;;
    --wipe)    DO_WIPE=1 ;;
    --yes|-y)  ASSUME_YES=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage; exit 2 ;;
  esac
  shift
done

# --- --wipe: destroy DB and analysis results --------------------------------
if [[ "${DO_WIPE}" -eq 1 ]]; then
  if [[ "${ASSUME_YES}" -ne 1 ]]; then
    echo "About to DELETE all contents of:"
    echo "  ${RESULTS_DIR}"
    echo "(jobs.db + every analysis session folder). This cannot be undone."
    read -r -p "Type 'wipe' to confirm: " reply
    [[ "${reply}" == "wipe" ]] || { echo "Aborted."; exit 1; }
  fi
  if [[ -d "${RESULTS_DIR}" ]]; then
    # Delete contents, not the dir itself — colima mounts the path.
    find "${RESULTS_DIR}" -mindepth 1 -maxdepth 1 -exec rm -rf {} +
    echo "Wiped ${RESULTS_DIR}/*"
  fi
fi

# --- --restart: reap webui + zombie jobs, clear __pycache__ -----------------
kill_pids() {
  # $1 = label for logs, remaining args = pids
  local label="$1"; shift
  [[ $# -eq 0 ]] && return 0
  echo "Killing ${label}: $*"
  kill "$@" 2>/dev/null || true
  sleep 1
  local survivors=()
  for pid in "$@"; do
    kill -0 "${pid}" 2>/dev/null && survivors+=("${pid}")
  done
  [[ ${#survivors[@]} -gt 0 ]] && kill -9 "${survivors[@]}" 2>/dev/null || true
}

if [[ "${DO_RESTART}" -eq 1 ]]; then
  # 1. webui on the port
  mapfile -t webui_pids < <(lsof -ti ":${WEBUI_PORT}" 2>/dev/null || true)
  kill_pids "webui on port ${WEBUI_PORT}" "${webui_pids[@]}"

  # 2. zombie aviti_test_mask.sh workers (orphaned bash drivers)
  mapfile -t worker_pids < <(pgrep -f aviti_test_mask.sh || true)
  kill_pids "aviti_test_mask.sh workers" "${worker_pids[@]}"

  # 3. zombie docker containers tagged by the worker
  if command -v docker >/dev/null 2>&1; then
    mapfile -t job_containers < <(docker ps -aq --filter 'label=aviti_job_id' 2>/dev/null || true)
    if [[ ${#job_containers[@]} -gt 0 ]]; then
      echo "Removing zombie aviti_job_id containers: ${job_containers[*]}"
      docker rm -f "${job_containers[@]}" >/dev/null || true
    fi
  fi

  # 4. python caches
  echo "Clearing Python caches under webui/"
  find "${PROJECT_DIR}/webui" -type d -name __pycache__ -prune -exec rm -rf {} +
  find "${PROJECT_DIR}/webui" -type f -name '*.pyc' -delete
fi

# --- preflight ---------------------------------------------------------------
if [[ ! -d "${NAS_DIR}/GBW-0047_NUC_Transfers" ]]; then
  echo "WARN: NAS share ${NAS_DIR} is not mounted — webui banner will warn." >&2
fi

mkdir -p "${RESULTS_DIR}"

# --- colima ------------------------------------------------------------------
# Failed yesterday:
#   colima start --mount '/Volumes/lvs:r' --mount '~/.../results:rw'
# Two issues fixed below:
#   1. '~' does not expand inside single quotes — use the absolute path.
#   2. colima --mount accepts ':r' (read-only) or ':w' (writable), not ':rw'.
COLIMA_MOUNTS=(
  --mount "${NAS_DIR}:r"
  --mount "${RESULTS_DIR}:w"
)

if colima status >/dev/null 2>&1; then
  echo "colima already running — assuming mounts are correct."
  echo "  (if mounts changed, run: colima stop && $0)"
else
  echo "Starting colima with mounts:"
  printf '  %s\n' "${COLIMA_MOUNTS[@]}"
  colima start "${COLIMA_MOUNTS[@]}"
fi

# --- port check --------------------------------------------------------------
if lsof -ti ":${WEBUI_PORT}" >/dev/null 2>&1; then
  echo "ERROR: port ${WEBUI_PORT} is already in use. Re-run with --restart to kill it." >&2
  exit 1
fi

# --- launch webui ------------------------------------------------------------
cd "${PROJECT_DIR}/webui"
echo "Starting webui at http://127.0.0.1:${WEBUI_PORT}"
exec "${PYTHON_BIN}" app.py
