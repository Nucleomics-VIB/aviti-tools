#!/usr/bin/env bash
# script: aviti_test_mask.sh v1.8
# SP@NC, 2026-02-26

set -euo pipefail

usage() {
  echo "Usage: $0 -i RUN_DIR -o OUTPUT_DIR [-t TILE]"
  echo "  -t TILE: optional strict single-tile mode (e.g. L1R01C02S1)"
  echo "  Omit -t to run on full data (all available tiles)"
}

INPUT_DIR=""
OUTPUT_BASE=""
TILE=""
TILE_SOURCE="auto"
TARGET_LANE=""
MASKS=(
  "R1:Y15N*-R2:Y15N*"
  "R1:Y12N*-R2:Y12N*"
  "R1:Y18N*-R2:Y18N*"
  "R1:Y10N*-R2:Y10N*"
  "R1:N*-R2:N*"
  "R1:Y15N*-R2:N*"
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
    -t|--tile)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      TILE="$2"
      TILE_SOURCE="user"
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
command -v python3 >/dev/null 2>&1 || { echo "Install Python 3"; exit 1; }

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
TEMP_ISOLATED_RUN=""
MIN_MANIFEST_PATH=""
DOCKER_INPUT_ARGS=(-v "$INPUT_ABS:/input:ro")

cleanup() {
  if [[ -n "$TEMP_ISOLATED_RUN" && -d "$TEMP_ISOLATED_RUN" ]]; then
    rm -rf "$TEMP_ISOLATED_RUN"
  fi
}

trap cleanup EXIT INT TERM

echo "📁 Input:  $INPUT_ABS"
echo "📁 Output: $OUTPUT_ABS"

BASECALLS_DIR="$INPUT_ABS/BaseCalls"
[[ -d "$BASECALLS_DIR" ]] || { echo "Missing BaseCalls directory: $BASECALLS_DIR"; exit 1; }

# 1. GUNZIP (if needed)
GZ_FILES=$(find "$BASECALLS_DIR" -type f -name "*.gz" 2>/dev/null | wc -l | tr -d '[:space:]')
if [[ "$GZ_FILES" -gt 0 ]]; then
  echo "📦 Gunzipping $GZ_FILES files..."
  find "$BASECALLS_DIR" -type f -name "*.gz" -exec gunzip {} +
fi

# 2. TILE MODE
# - If -t is provided: strict single-tile mode.
# - If -t is omitted: full run mode across all tiles.
if [[ -n "$TILE" ]]; then
  echo "✅ Using user-provided tile: $TILE"
else
  echo "✅ No tile provided: running on full data (all tiles)"
fi

if [[ "$TILE_SOURCE" == "user" ]]; then
  if [[ ! -d "$BASECALLS_DIR/$TILE" ]]; then
    echo "❌ Requested tile directory not found: $BASECALLS_DIR/$TILE"
    exit 1
  fi

  if [[ "$TILE" =~ ^L([0-9]+)R[0-9]+C[0-9]+S[0-9]+$ ]]; then
    TARGET_LANE="${BASH_REMATCH[1]}"
  else
    echo "❌ Could not derive lane from tile '$TILE' (expected format L<lane>RxxCxxSx)"
    exit 1
  fi

  RUN_MANIFEST_PATH="$INPUT_ABS/RunManifest.csv"
  if [[ ! -f "$RUN_MANIFEST_PATH" ]]; then
    echo "❌ Run manifest not found: $RUN_MANIFEST_PATH"
    exit 1
  fi

  TEMP_ISOLATED_RUN="$OUTPUT_ABS/.strict_run_${TILE}_$$"
  mkdir -p "$TEMP_ISOLATED_RUN"

  for entry in "$INPUT_ABS"/*; do
    name=$(basename "$entry")
    [[ "$name" == "BaseCalls" ]] && continue
    cp -a "$entry" "$TEMP_ISOLATED_RUN/"
  done

  mkdir -p "$TEMP_ISOLATED_RUN/BaseCalls"
  cp -a "$BASECALLS_DIR/$TILE" "$TEMP_ISOLATED_RUN/BaseCalls/"

  MIN_MANIFEST_PATH="$TEMP_ISOLATED_RUN/RunManifest.csv"
  python3 - "$RUN_MANIFEST_PATH" "$MIN_MANIFEST_PATH" "$TARGET_LANE" <<'PY'
import csv
import io
import sys

src, dst, target_lane = sys.argv[1], sys.argv[2], sys.argv[3]

section = ""
settings_lane_idx = None
samples_lane_idx = None

def parse_csv_line(line: str):
    return next(csv.reader([line]))

def to_csv_line(row):
    buffer = io.StringIO()
    writer = csv.writer(buffer, lineterminator="")
    writer.writerow(row)
    return buffer.getvalue()


with open(src, "r", encoding="utf-8", errors="ignore") as fin:
    lines = fin.readlines()

filtered = []

for line in lines:
    stripped = line.strip()

    if stripped.startswith("[") and stripped.endswith("]"):
        section = stripped.upper()
        settings_lane_idx = None
        samples_lane_idx = None
        filtered.append(line)
        continue

    if stripped == "" or stripped.startswith("#"):
        filtered.append(line)
        continue

    row = parse_csv_line(line.rstrip("\n"))

    if section == "[SETTINGS]":
        if row and row[0].strip().lower() == "settingname":
            lowered = [col.strip().lower() for col in row]
            settings_lane_idx = lowered.index("lane") if "lane" in lowered else 2
        elif settings_lane_idx is not None and len(row) > settings_lane_idx:
            lane_val = row[settings_lane_idx].strip().replace(" ", "")
            if lane_val in {"1+2", "2+1", "1,2", "2,1", "1-2"}:
                row[settings_lane_idx] = target_lane

    elif section == "[SAMPLES]":
        if row and row[0].strip().lower() == "samplename":
            lowered = [col.strip().lower() for col in row]
            if "lane" in lowered:
                samples_lane_idx = lowered.index("lane")
        elif samples_lane_idx is not None and len(row) > samples_lane_idx:
            row[samples_lane_idx] = target_lane

    filtered.append(to_csv_line(row) + "\n")


if not filtered:
    raise RuntimeError("Filtered RunManifest is empty")

with open(dst, "w", encoding="utf-8", newline="") as fout:
    fout.writelines(filtered)
PY

  DOCKER_INPUT_ARGS=(-v "$TEMP_ISOLATED_RUN:/input:ro")

  echo "✅ Strict tile mode enabled via isolated run copy: only BaseCalls/$TILE is present"
  echo "✅ Lane-restricted manifest generated for lane $TARGET_LANE"
  echo "🧾 Minimal manifest written: $MIN_MANIFEST_PATH"
fi

# Version (best-effort)
B2F_VERSION=$(docker run --rm "${DOCKER_USER_ARGS[@]}" --platform linux/amd64 elembio/bases2fastq:latest bases2fastq --version 2>&1 | sed -n 's/.*bases2fastq \([0-9.]*\).*/\1/p' | head -1)
[[ -z "$B2F_VERSION" ]] && B2F_VERSION="unknown"
echo "📦 bases2fastq v$B2F_VERSION"

run_mask_qc() {
  local mask="$1"
  local outdir="$2"
  local tile="$3"
  local tile_source="$4"
  local logfile="$outdir/run.log"

  local filter_missing_warnings=false
  if [[ "$tile_source" == "user" ]]; then
    filter_missing_warnings=true
  fi

  run_and_capture() {
    local use_tile="$1"
    local append_log="$2"

    local -a cmd=(
      docker run --rm
      "${DOCKER_USER_ARGS[@]}"
      "${DOCKER_INPUT_ARGS[@]}"
      -v "$outdir:/output"
      --platform linux/amd64
      elembio/bases2fastq:latest
      bases2fastq /input /output
      --qc-only
      --filter-mask "$mask"
    )

    if [[ "$use_tile" == "true" && -n "$tile" ]]; then
      cmd+=(--include-tile "$tile")
    fi

    if [[ "$append_log" == "true" ]]; then
      if [[ "$filter_missing_warnings" == "true" ]]; then
        "${cmd[@]}" 2>&1 | sed '/Missing Bases file/d' | tee -a "$logfile"
      else
        "${cmd[@]}" 2>&1 | tee -a "$logfile"
      fi
    else
      if [[ "$filter_missing_warnings" == "true" ]]; then
        "${cmd[@]}" 2>&1 | sed '/Missing Bases file/d' | tee "$logfile"
      else
        "${cmd[@]}" 2>&1 | tee "$logfile"
      fi
    fi
  }

  if [[ -n "$tile" ]]; then
    if run_and_capture true false; then
      return 0
    fi

    if [[ "$tile_source" == "user" ]]; then
      echo "❌ bases2fastq failed with user-provided --include-tile '$tile' for mask '$mask'"
      echo "❌ Not retrying without tile because -t was explicitly requested"
      return 1
    fi

    echo "⚠️ bases2fastq failed with --include-tile '$tile' for mask '$mask'; retrying without tile filter"
    echo "---- retry without include-tile ----" >> "$logfile"
    run_and_capture false true
    return $?
  fi

  run_and_capture false false
}

# Run tests
mkdir -p "$OUTPUT_ABS/qc_runs"
PIDS=()

for i in "${!MASKS[@]}"; do
  MASK="${MASKS[$i]}"
  SAFE_MASK=$(echo "$MASK" | tr -c '[:alnum:]_.-' '_' | tr -s '_')
  OUTDIR="$OUTPUT_ABS/qc_runs/mask_${i}_${SAFE_MASK}"
  mkdir -p "$OUTDIR"

  echo "[$((i + 1))/${#MASKS[@]}] $MASK"
  run_mask_qc "$MASK" "$OUTDIR" "$TILE" "$TILE_SOURCE" &

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
