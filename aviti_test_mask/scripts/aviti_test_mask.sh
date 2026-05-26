#!/usr/bin/env bash
# script: aviti_test_mask.sh v1.9
# Full-run QC masks via bases2fastq
# SP@NC, 2026-02-27, v1.1

set -euo pipefail

usage() {
  echo "Usage: $0 -i RUN_DIR -o OUTPUT_DIR [-p THREADS] [-j JOBS] [-m MASKS_FILE] [-c CONFIG] [--cache-input] [--include-tile PATTERN] [--job-id ID]"
  echo "  -p THREADS:        bases2fastq worker threads per container (default: 8)"
  echo "  -j JOBS:           max concurrent mask runs (default: 4)"
  echo "  -m MASKS_FILE:     YAML file with mask list (default: built-in array)"
  echo "  -c CONFIG:         YAML config file (default: config.yaml next to this script)"
  echo "  --cache-input:     copy input to local fast storage before running (recommended on NAS)"
  echo "  --include-tile P:  passthrough to bases2fastq --include-tile (e.g. L1R..C..S. or L1R02C01S1|L1R05C01S1)"
  echo "  --job-id ID:       tag every spawned docker container with --label aviti_job_id=ID (for cancel/cleanup)"
}

INPUT_DIR=""
OUTPUT_BASE=""
THREADS="8"
MAX_JOBS=4
CACHE_INPUT=0
MEM_LIMIT=""
DOCKER_IMAGE="elembio/bases2fastq:latest"
CACHE_DIR=""
_SEMFIFO=""
MASKS_FILE=""
INCLUDE_TILE=""
JOB_ID=""
CONFIG_FILE="$(dirname "$0")/config.yaml"

# Pre-scan argv for -c/--config so the correct file is loaded before full parsing
for ((_i=1; _i<=$#; _i++)); do
  if [[ "${!_i}" == "-c" || "${!_i}" == "--config" ]]; then
    _j=$((_i+1)); CONFIG_FILE="${!_j}"; break
  fi
done; unset _i _j
MASKS=(
  "R1:Y18N*-R2:Y18N*"
  "R1:N16Y15N*-R2:N16Y15N*"
  "R1:N16Y15N*-R2:Y15N*"
  "R1:Y15N*-R2:Y15N*"
  "R1:Y12N*-R2:Y12N*"
  "R1:N12Y15N*-R2:Y8N*"
  "R1:Y10N*-R2:Y10N*"
  "R1:Y8N*-R2:Y8N*"
  "R1:N*-R2:N*"
)

# Load config.yaml — CLI flags parsed below will override these values
if [[ -f "$CONFIG_FILE" ]]; then
  while IFS='=' read -r _key _val; do
    case "$_key" in
      THREADS)    THREADS="$_val"    ;;
      MAX_JOBS)   MAX_JOBS="$_val"   ;;
      CACHE_INPUT)   CACHE_INPUT="$_val"   ;;
      MEM_LIMIT)     MEM_LIMIT="$_val"     ;;
      DOCKER_IMAGE)  DOCKER_IMAGE="$_val"  ;;
    esac
  done < <(python3 - "$CONFIG_FILE" <<'PY'
import re, sys
keys = {'threads': 'THREADS', 'max_jobs': 'MAX_JOBS',
        'cache_input': 'CACHE_INPUT', 'mem_limit_per_job': 'MEM_LIMIT',
        'docker_image': 'DOCKER_IMAGE'}
with open(sys.argv[1]) as fh:
    for line in fh:
        m = re.match(r'^(\w+)\s*:\s*(.*?)\s*$', line)
        if not m or m.group(1) not in keys:
            continue
        val = m.group(2).strip().strip('"\'')
        if val.lower() in ('true', 'yes'):
            val = '1'
        elif val.lower() in ('false', 'no', '~', ''):
            val = '0' if m.group(1) == 'cache_input' else ''
        print(f"{keys[m.group(1)]}={val}")
PY
)
  echo "⚙️  Config: $CONFIG_FILE"
fi

# Parse args
while [[ $# -gt 0 ]]; do
  case "$1" in
    -i|--input)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      INPUT_DIR="$2"
      shift 2
      ;;
    -o|--output)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      OUTPUT_BASE="$2"
      shift 2
      ;;
    -p|--threads)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      THREADS="$2"
      shift 2
      ;;
    -c|--config)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      CONFIG_FILE="$2"
      shift 2
      ;;
    -j|--jobs)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      MAX_JOBS="$2"
      shift 2
      ;;
    --cache-input)
      CACHE_INPUT=1
      shift
      ;;
    -m|--masks-file)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      MASKS_FILE="$2"
      shift 2
      ;;
    --include-tile)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      INCLUDE_TILE="$2"
      shift 2
      ;;
    --job-id)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      JOB_ID="$2"
      shift 2
      ;;
    --mem-limit)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      MEM_LIMIT="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1"
      usage
      exit 1
      ;;
  esac
done

[[ -z "$INPUT_DIR" || -z "$OUTPUT_BASE" ]] && { usage; exit 1; }
[[ "$THREADS"  =~ ^[1-9][0-9]*$ ]] || { echo "Invalid thread count: $THREADS"; exit 1; }
[[ "$MAX_JOBS" =~ ^[1-9][0-9]*$ ]] || { echo "Invalid job count: $MAX_JOBS"; exit 1; }

if [[ -n "$MASKS_FILE" ]]; then
  [[ -f "$MASKS_FILE" ]] || { echo "Masks file not found: $MASKS_FILE"; exit 1; }
  set -f  # disable glob expansion while loading mask strings containing *
  mapfile -t MASKS < <(python3 - "$MASKS_FILE" <<'PY'
import re, sys
with open(sys.argv[1]) as fh:
    for line in fh:
        m = re.match(r'\s*-\s*(.*?)\s*$', line)
        if not m:
            continue
        val = m.group(1)
        if (val.startswith('"') and val.endswith('"')) or \
           (val.startswith("'") and val.endswith("'")):
            val = val[1:-1]
        if val and not val.startswith('#'):
            print(val)
PY
)
  set +f
  [[ ${#MASKS[@]} -gt 0 ]] || { echo "No masks found in: $MASKS_FILE"; exit 1; }
  echo "📋 Loaded ${#MASKS[@]} masks from $MASKS_FILE"
fi

# Resolve absolute path without requiring GNU realpath on macOS
abspath() {
  local path="$1"
  if command -v realpath >/dev/null 2>&1; then
    realpath "$path" 2>/dev/null || (
      cd "$(dirname "$path")" && printf '%s/%s\n' "$PWD" "$(basename "$path")"
    )
  else
    (
      cd "$(dirname "$path")"
      printf '%s/%s\n' "$PWD" "$(basename "$path")"
    )
  fi
}

# Return 0 if path is on a network/LAN mount (NFS or SMB)
is_lan_mount() {
  local path="$1"
  local device
  device=$(df "$path" 2>/dev/null | awk 'NR==2{print $1}') || return 1
  # NFS: server:/export  SMB/CIFS: //server/share
  [[ "$device" =~ ^// ]] && return 0
  [[ "$device" =~ ^[A-Za-z0-9._-]+:/ ]] && return 0
  return 1
}

# Verify Docker can actually see files inside a bind-mounted path
verify_docker_input() {
  local host_path="$1"
  local count
  count=$(docker run --rm \
    --platform linux/amd64 \
    -v "$host_path:/input:ro" \
    "$DOCKER_IMAGE" \
    sh -c 'ls /input 2>/dev/null | wc -l' 2>/dev/null) || count=0
  [[ "${count// /}" -gt 0 ]] 2>/dev/null
}

# Stage input to fast local storage (tmpfs on Linux, /tmp on macOS)
# Updates INPUT_ABS and DOCKER_INPUT_ARGS on success; returns 1 and warns on failure.
stage_input_to_ram() {
  local src="$1"
  local stage_root
  if [[ -d /dev/shm ]] && df /dev/shm 2>/dev/null | awk 'NR==2{print $1}' | grep -qi tmpfs; then
    stage_root=/dev/shm
  else
    stage_root="${TMPDIR:-/tmp}"
    echo "ℹ️  No tmpfs at /dev/shm — staging to $stage_root (local disk)"
  fi
  local run_kb free_kb
  run_kb=$(du -sk "$src" 2>/dev/null | cut -f1)
  free_kb=$(df -k "$stage_root" | awk 'NR==2{print $4}')
  if (( run_kb > free_kb * 90 / 100 )); then
    echo "⚠️  Run ($((run_kb / 1024)) MB) exceeds 90% of space in $stage_root ($((free_kb / 1024)) MB) — skipping cache"
    return 1
  fi
  CACHE_DIR=$(mktemp -d "$stage_root/aviti_qc_XXXXX")
  echo "💾 Staging input ($((run_kb / 1024)) MB) → $CACHE_DIR …"
  if ! cp -r "$src/." "$CACHE_DIR/"; then
    echo "❌ Staging failed — removing partial copy, running against original path"
    rm -rf "$CACHE_DIR"
    CACHE_DIR=""
    return 1
  fi
  INPUT_ABS="$CACHE_DIR"
  DOCKER_INPUT_ARGS=(-v "$INPUT_ABS:/input:ro")
  echo "✅ Input staged to fast storage"
}

# Report current host resource headroom against the requested allocation.
# Warns on overcommit; never aborts — the user decides whether to continue.
check_resources() {
  local n_cpus load1 req_threads req_mem_kb avail_mem_kb

  # --- CPU ---
  n_cpus=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 0)
  req_threads=$(( MAX_JOBS * THREADS ))
  load1=$(awk '{print $1}' /proc/loadavg 2>/dev/null || uptime | grep -Eo 'load average[s]?: [0-9.]+' | grep -Eo '[0-9.]+$' || echo 0)

  echo "── Resource check ──────────────────────────────"
  if [[ "$n_cpus" -gt 0 ]]; then
    if (( req_threads > n_cpus )); then
      echo "⚠️  CPU  : ${req_threads} threads requested (${MAX_JOBS} jobs × ${THREADS}) — host has ${n_cpus} logical CPUs"
      echo "         Containers will time-share; consider reducing --jobs or -p"
    else
      echo "✅ CPU  : ${req_threads} / ${n_cpus} logical CPUs requested"
    fi
    echo "   Load : ${load1} (1-min avg, ${n_cpus} CPUs)"
  fi

  # --- RAM (only when MEM_LIMIT is set) ---
  if [[ -n "$MEM_LIMIT" ]]; then
    req_mem_kb=$(python3 - "$MEM_LIMIT" "$MAX_JOBS" <<'PY'
import re, sys
m = re.fullmatch(r'([0-9]+(?:\.[0-9]+)?)([kmgKMG]?)[bB]?', sys.argv[1].strip())
if not m:
    print(0); sys.exit()
n, u = float(m.group(1)), m.group(2).lower()
kb = int(n * {'k': 1, 'm': 1024, 'g': 1024*1024}.get(u, 1))
print(kb * int(sys.argv[2]))
PY
)
    if [[ -f /proc/meminfo ]]; then
      avail_mem_kb=$(awk '/^MemAvailable/{print $2}' /proc/meminfo)
    else
      avail_mem_kb=0
    fi
    local req_gb=$(( req_mem_kb / 1024 / 1024 ))
    local avail_gb=$(( avail_mem_kb / 1024 / 1024 ))
    if [[ "$avail_mem_kb" -gt 0 ]]; then
      if (( req_mem_kb > avail_mem_kb )); then
        echo "⚠️  RAM  : ${req_gb} GB requested (${MAX_JOBS} × ${MEM_LIMIT}) — only ${avail_gb} GB available now"
        echo "         Risk of OOM; reduce --jobs or mem_limit_per_job, or free memory first"
      else
        echo "✅ RAM  : ${req_gb} GB requested / ${avail_gb} GB currently available"
      fi
    fi
  else
    if [[ -f /proc/meminfo ]]; then
      avail_mem_kb=$(awk '/^MemAvailable/{print $2}' /proc/meminfo)
      echo "ℹ️  RAM  : $(( avail_mem_kb / 1024 / 1024 )) GB available (no per-job cap set)"
    fi
  fi
  echo "────────────────────────────────────────────────"
}

_cleanup() {
  [[ -n "$_SEMFIFO" ]] && { exec 3>&- 2>/dev/null || true; rm -f "$_SEMFIFO"; }
  [[ -n "$CACHE_DIR" ]] && rm -rf "$CACHE_DIR"
}
trap _cleanup EXIT

# Requirements
command -v docker >/dev/null 2>&1 || { echo "Install Docker"; exit 1; }
echo "🐳 Pulling $DOCKER_IMAGE …"
docker pull --platform linux/amd64 "$DOCKER_IMAGE"

HOST_UID=$(id -u)
HOST_GID=$(id -g)
DOCKER_USER_ARGS=(--user "${HOST_UID}:${HOST_GID}")

if command -v conda >/dev/null 2>&1; then
  eval "$(conda shell.bash hook)"
  CONDA_ENV_NAME="${CONDA_ENV_NAME:-pythonenv}"
  if conda env list | awk '{print $1}' | grep -qx "$CONDA_ENV_NAME"; then
    conda activate "$CONDA_ENV_NAME"
  else
    echo "ℹ  conda env '$CONDA_ENV_NAME' not found, staying in '${CONDA_DEFAULT_ENV:-base}'"
  fi
fi

[[ -d "$INPUT_DIR" ]] || { echo "Input run directory not found: $INPUT_DIR"; exit 1; }
mkdir -p "$OUTPUT_BASE"

INPUT_ABS=$(abspath "$INPUT_DIR")
OUTPUT_ABS=$(abspath "$OUTPUT_BASE")
export OUTPUT_BASE="$OUTPUT_ABS"
DOCKER_INPUT_ARGS=(-v "$INPUT_ABS:/input:ro")

# Warn when input is on a network mount — Docker Desktop requires it to be
# listed under Settings > Resources > File Sharing or the bind mount will be empty.
if is_lan_mount "$INPUT_ABS"; then
  echo "⚠️  Input is on a LAN/network mount."
  echo "   Docker Desktop must have '$INPUT_ABS' (or a parent path) in"
  echo "   Settings > Resources > File Sharing, otherwise /input will be empty."
  echo "   Verifying Docker can see the input files..."
  if ! verify_docker_input "$INPUT_ABS"; then
    echo "❌ Docker cannot read '$INPUT_ABS' — add it to Docker file sharing and retry."
    exit 1
  fi
  echo "✅ Docker mount verified."
fi

BASECALLS_DIR="$INPUT_ABS/BaseCalls"
[[ -d "$BASECALLS_DIR" ]] || { echo "Missing BaseCalls directory: $BASECALLS_DIR"; exit 1; }

if [[ "$CACHE_INPUT" -eq 1 ]]; then
  stage_input_to_ram "$INPUT_ABS" || true  # fallback to original path on failure
fi

echo "📁 Input:   $INPUT_ABS"
echo "📁 Output:  $OUTPUT_ABS"
echo "🧵 Threads: $THREADS  |  ⚙️  Jobs: $MAX_JOBS  |  💾 Cache: $([[ -n $CACHE_DIR ]] && echo yes || echo no)  |  🛡️  Mem/job: ${MEM_LIMIT:-unlimited}"

# bases2fastq version (best-effort)
B2F_VERSION_RAW=$(docker run --rm "${DOCKER_USER_ARGS[@]}" --platform linux/amd64 "$DOCKER_IMAGE" bases2fastq --version 2>&1 || true)
B2F_VERSION=$(printf '%s\n' "$B2F_VERSION_RAW" \
  | grep -Eio 'bases2fastq[^0-9]*v?[0-9]+(\.[0-9]+){1,3}' \
  | head -1 \
  | grep -Eo 'v?[0-9]+(\.[0-9]+){1,3}' \
  | sed 's/^v//')

if [[ -z "$B2F_VERSION" ]]; then
  B2F_VERSION=$(printf '%s\n' "$B2F_VERSION_RAW" \
    | grep -Eo 'v?[0-9]+(\.[0-9]+){1,3}' \
    | head -1 \
    | sed 's/^v//')
fi

[[ -z "$B2F_VERSION" ]] && B2F_VERSION="unknown"
echo "📦 bases2fastq v$B2F_VERSION"

run_mask_qc() {
  local mask="$1"
  local outdir="$2"
  local logfile="$outdir/run.log"
  local mem_args=()
  local label_args=()
  local b2f_extra_args=()
  # --memory-swap equal to --memory disables swap, capping hard at MEM_LIMIT (OOM prevention)
  [[ -n "$MEM_LIMIT" ]] && mem_args=(--memory "$MEM_LIMIT" --memory-swap "$MEM_LIMIT")
  # --job-id labels every container so the orchestrator can cancel via:
  #   docker ps --filter label=aviti_job_id=<id> -q | xargs docker stop
  [[ -n "$JOB_ID" ]] && label_args=(--label "aviti_job_id=$JOB_ID")
  # --include-tile passes through to bases2fastq (a regex pattern or
  # pipe-joined tile-ID list, resolved by the web UI's tile selector).
  [[ -n "$INCLUDE_TILE" ]] && b2f_extra_args+=(--include-tile "$INCLUDE_TILE")

  docker run --rm \
    "${DOCKER_USER_ARGS[@]}" \
    "${DOCKER_INPUT_ARGS[@]}" \
    "${mem_args[@]}" \
    "${label_args[@]}" \
    -v "$outdir:/output" \
    --platform linux/amd64 \
    "$DOCKER_IMAGE" \
    bases2fastq /input /output \
    --qc-only \
    --filter-mask "$mask" \
    "${b2f_extra_args[@]}" \
    -p "$THREADS" 2>&1 | tee "$logfile"
}

check_resources

# Run QC for each filter mask — at most MAX_JOBS containers concurrently
mkdir -p "$OUTPUT_ABS/qc_runs"
PIDS=()
declare -A _MASK_FOR_PID _OUTDIR_FOR_PID

_SEMFIFO="$(mktemp -u)"
mkfifo "$_SEMFIFO"
exec 3<>"$_SEMFIFO"
for ((si=0; si<MAX_JOBS; si++)); do printf ' ' >&3; done

for i in "${!MASKS[@]}"; do
  read -r -n1 -u3  # block until a concurrency slot is free
  MASK="${MASKS[$i]}"
  SAFE_MASK=$(echo "$MASK" | tr -c '[:alnum:]_.-' '_' | tr -s '_')
  OUTDIR="$OUTPUT_ABS/qc_runs/mask_${i}_${SAFE_MASK}"
  mkdir -p "$OUTDIR"
  echo "▶ [$((i + 1))/${#MASKS[@]}] $MASK"
  (
    trap 'printf " " >&3' EXIT  # release slot on subshell exit (normal or error)
    if run_mask_qc "$MASK" "$OUTDIR"; then
      echo "✅ [$MASK] completed"
    else
      ec=$?
      if [[ $ec -eq 137 ]]; then
        echo "❌ [$MASK] KILLED — OOM (exit 137); raise mem_limit_per_job or reduce --jobs"
      else
        echo "❌ [$MASK] FAILED (exit $ec) — see $OUTDIR/run.log"
      fi
      exit "$ec"
    fi
  ) &
  PIDS+=("$!")
  _MASK_FOR_PID["$!"]="$MASK"
  _OUTDIR_FOR_PID["$!"]="$OUTDIR"
done

# Wait for all jobs; per-mask success/fail line was already emitted by
# the subshell, so just tally exit codes here for the final summary.
SUCCESS=0
FAILED=0
for pid in "${PIDS[@]}"; do
  if wait "$pid" 2>/dev/null; then
    SUCCESS=$((SUCCESS + 1))
  else
    FAILED=$((FAILED + 1))
  fi
done

echo "📊 $SUCCESS/${#PIDS[@]} succeeded  |  $FAILED failed"
[[ $FAILED -gt 0 ]] && exit 1

# Integration moved to standalone helper to keep this script short and robust.
INTEGRATOR_SCRIPT="$(dirname "$0")/integrate_mask_results.sh"
echo "🧾 QC outputs are ready under: $OUTPUT_ABS/qc_runs"
if [[ -x "$INTEGRATOR_SCRIPT" ]]; then
  echo "➡️ Run integration with: $INTEGRATOR_SCRIPT -o \"$OUTPUT_ABS\""
else
  echo "➡️ Run integration with: scripts/integrate_mask_results.sh -o \"$OUTPUT_ABS\""
fi

if command -v conda >/dev/null 2>&1; then
  conda deactivate || true
fi

echo "✅ Complete! $OUTPUT_ABS"
