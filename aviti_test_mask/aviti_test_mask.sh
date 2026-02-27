#!/usr/bin/env bash
# script: aviti_test_mask.sh v1.9
# Full-run QC masks via bases2fastq
# SP@NC, 2026-02-27, v1.1

set -euo pipefail

usage() {
  echo "Usage: $0 -i RUN_DIR -o OUTPUT_DIR [-p THREADS]"
  echo "  -p THREADS: bases2fastq worker threads (default: 8)"
}

INPUT_DIR=""
OUTPUT_BASE=""
THREADS="8"
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
[[ "$THREADS" =~ ^[1-9][0-9]*$ ]] || { echo "Invalid thread count: $THREADS"; exit 1; }

# Resolve absolute path without requiring GNU realpath on macOS
abspath() {
  local path="$1"
  if command -v realpath >/dev/null 2>&1; then
    realpath "$path"
  else
    (
      cd "$(dirname "$path")"
      printf '%s/%s\n' "$PWD" "$(basename "$path")"
    )
  fi
}

# Requirements
command -v docker >/dev/null 2>&1 || { echo "Install Docker"; exit 1; }

HOST_UID=$(id -u)
HOST_GID=$(id -g)
DOCKER_USER_ARGS=(--user "${HOST_UID}:${HOST_GID}")

if command -v conda >/dev/null 2>&1; then
  eval "$(conda shell.bash hook)"
  conda activate pythonenv || true
fi

[[ -d "$INPUT_DIR" ]] || { echo "Input run directory not found: $INPUT_DIR"; exit 1; }
mkdir -p "$OUTPUT_BASE"

INPUT_ABS=$(abspath "$INPUT_DIR")
OUTPUT_ABS=$(abspath "$OUTPUT_BASE")
export OUTPUT_BASE="$OUTPUT_ABS"
DOCKER_INPUT_ARGS=(-v "$INPUT_ABS:/input:ro")

echo "📁 Input:  $INPUT_ABS"
echo "📁 Output: $OUTPUT_ABS"
echo "🧵 Threads: $THREADS"

BASECALLS_DIR="$INPUT_ABS/BaseCalls"
[[ -d "$BASECALLS_DIR" ]] || { echo "Missing BaseCalls directory: $BASECALLS_DIR"; exit 1; }

# bases2fastq version (best-effort)
B2F_VERSION_RAW=$(docker run --rm "${DOCKER_USER_ARGS[@]}" --platform linux/amd64 elembio/bases2fastq:latest bases2fastq --version 2>&1 || true)
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

  docker run --rm \
    "${DOCKER_USER_ARGS[@]}" \
    "${DOCKER_INPUT_ARGS[@]}" \
    -v "$outdir:/output" \
    --platform linux/amd64 \
    elembio/bases2fastq:latest \
    bases2fastq /input /output \
    --qc-only \
    --filter-mask "$mask" \
    -p "$THREADS" 2>&1 | tee "$logfile"
}

# Run QC for each filter mask
mkdir -p "$OUTPUT_ABS/qc_runs"
PIDS=()

for i in "${!MASKS[@]}"; do
  MASK="${MASKS[$i]}"
  SAFE_MASK=$(echo "$MASK" | tr -c '[:alnum:]_.-' '_' | tr -s '_')
  OUTDIR="$OUTPUT_ABS/qc_runs/mask_${i}_${SAFE_MASK}"
  mkdir -p "$OUTDIR"

  echo "[$((i + 1))/${#MASKS[@]}] $MASK"
  run_mask_qc "$MASK" "$OUTDIR" &

  PIDS+=("$!")
done

# Wait for completion
SUCCESS=0
for pid in "${PIDS[@]}"; do
  if wait "$pid" 2>/dev/null; then
    SUCCESS=$((SUCCESS + 1))
    echo "✅ $pid"
  fi
done

echo "📊 $SUCCESS/${#PIDS[@]} succeeded"

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
