#!/usr/bin/env bash
# script: integrate_mask_results.sh
# Integrate already-generated mask QC outputs (no rerun required)

set -euo pipefail

usage() {
  echo "Usage: $0 -o OUTPUT_DIR"
  echo "  OUTPUT_DIR should contain qc_runs/ (e.g. test1/)"
}

OUTPUT_BASE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--output)
      [[ $# -lt 2 ]] && { usage; exit 1; }
      OUTPUT_BASE="$2"
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

[[ -z "$OUTPUT_BASE" ]] && { usage; exit 1; }
[[ -d "$OUTPUT_BASE" ]] || { echo "Output directory not found: $OUTPUT_BASE"; exit 1; }

python3 - "$OUTPUT_BASE" <<'PY'
import csv
import json
import re
import sys
from pathlib import Path

output_base = Path(sys.argv[1]).resolve()
qc_dir = output_base / "qc_runs"

if not qc_dir.exists():
    print(f"No qc_runs directory found under: {output_base}")
    sys.exit(1)

def extract_metric(content: str, labels):
    number = r"([0-9]+(?:\.[0-9]+)?)"
    patterns = []
    for label in labels:
        esc = re.escape(label)
        patterns.extend([
            rf"{esc}\s*[:=]\s*{number}\s*%",
            rf"{esc}.*?{number}\s*%",
        ])
    for pattern in patterns:
        match = re.search(pattern, content, re.I | re.S)
        if match:
            return float(match.group(1))
    return None

def extract_metric_with_suffix(content: str, labels, suffix):
    number = r"([0-9]+(?:\.[0-9]+)?)"
    for label in labels:
        esc = re.escape(label)
        pattern = rf"{esc}\s*[:=]\s*{number}\s*{re.escape(suffix)}"
        match = re.search(pattern, content, re.I | re.S)
        if match:
            return float(match.group(1))
    return None

def parse_percent(value):
    if value is None:
        return None
    text = str(value).strip().replace("%", "")
    match = re.search(r"([0-9]+(?:\.[0-9]+)?)", text)
    if not match:
        return None
    return float(match.group(1))

def extract_by_keys(mapping, keys):
    lowered = {str(k).strip().lower(): v for k, v in mapping.items()}
    for key in keys:
        key_l = key.lower()
        if key_l in lowered:
            val = parse_percent(lowered[key_l])
            if val is not None:
                return val
    for key, value in lowered.items():
        for probe in keys:
            if probe.lower() in key:
                val = parse_percent(value)
                if val is not None:
                    return val
    return None

def extract_from_metrics_csv(path: Path):
    if not path.exists():
        return None

    try:
        with path.open("r", encoding="utf-8", errors="ignore", newline="") as handle:
            rows = list(csv.reader(handle))
    except Exception:
        return None

    if not rows:
        return None

    header = [h.strip() for h in rows[0]]
    if len(rows) >= 2 and len(header) >= 2:
        for row in rows[1:]:
            if not any(str(cell).strip() for cell in row):
                continue
            mapping = {header[i]: row[i] if i < len(row) else "" for i in range(len(header))}
            pf = extract_by_keys(mapping, ["%pf", "pf", "pass filter", "reads assigned", "percent assigned"])
            q30 = extract_by_keys(mapping, ["q30", "q30%", "percent q30", "%>=q30"])
            if pf is not None and q30 is not None:
                return pf, q30, "metrics.csv"

    kv = {}
    for row in rows:
        if len(row) >= 2:
            key = str(row[0]).strip()
            value = str(row[1]).strip()
            if key:
                kv[key] = value

    if kv:
        pf = extract_by_keys(kv, ["%pf", "pf", "pass filter", "reads assigned", "percent assigned"])
        q30 = extract_by_keys(kv, ["q30", "q30%", "percent q30", "%>=q30"])
        if pf is not None and q30 is not None:
            return pf, q30, "metrics.csv"

    return None

def extract_from_html(content: str):
    assigned = extract_metric(content, ["Reads assigned", "Percent assigned", "Assigned reads"])
    if assigned is None:
        assigned = extract_metric_with_suffix(content, ["Reads assigned", "Percent assigned", "Assigned reads"], "%")
    q30 = extract_metric(content, ["Percent Q30", "Q30", "Q30%"])
    if assigned is None or q30 is None:
        return None
    return assigned, q30, "html"

def extract_from_log(content: str):
    assigned = extract_metric(content, ["Reads assigned", "Percent assigned", "Assigned reads"])
    q30 = extract_metric(content, ["Percent Q30", "Q30", "Q30%"])
    if assigned is None or q30 is None:
        return None
    return assigned, q30, "log"

def normalize_percent(value):
    if value is None:
        return None
    value = float(value)
    if 0.0 <= value <= 1.0:
        return value * 100.0
    return value

def flatten_json(obj, prefix=""):
    if isinstance(obj, dict):
        for key, value in obj.items():
            next_prefix = f"{prefix}.{key}" if prefix else str(key)
            yield from flatten_json(value, next_prefix)
    elif isinstance(obj, list):
        for idx, value in enumerate(obj):
            next_prefix = f"{prefix}[{idx}]" if prefix else f"[{idx}]"
            yield from flatten_json(value, next_prefix)
    else:
        yield prefix, obj

def parse_numeric(value):
    if isinstance(value, (int, float)):
        return float(value)
    text = str(value)
    match = re.search(r"([0-9]+(?:\.[0-9]+)?)", text)
    if not match:
        return None
    return float(match.group(1))

def metric_from_flat(flat_items, probes):
    scored = []
    for path, value in flat_items:
        path_l = path.lower()
        for probe in probes:
            probe_l = probe.lower()
            score = None
            if path_l.endswith(probe_l):
                score = 3
            elif f".{probe_l}." in f".{path_l}.":
                score = 2
            elif probe_l in path_l:
                score = 1
            if score is not None:
                number = parse_numeric(value)
                if number is not None:
                    scored.append((score, len(path_l), path, number))
                    break
    if not scored:
        return None
    scored.sort(key=lambda x: (-x[0], x[1]))
    return scored[0][2], scored[0][3]

def extract_from_runstats_json(path: Path):
    if not path.exists():
        return None
    try:
        data = json.loads(path.read_text(encoding="utf-8", errors="ignore"))
    except Exception:
        return None

    def first_numeric(mapping, keys):
        for key in keys:
            if isinstance(mapping, dict) and key in mapping:
                val = parse_numeric(mapping.get(key))
                if val is not None:
                    return key, val
        return None, None

    # 1) Prefer explicit run-level keys
    pf_key, pf_val = first_numeric(data, [
        "PercentPassFilter",
        "PassFilterPercent",
        "PercentPF",
        "PercentAssignedReads",
    ])
    q30_key, q30_val = first_numeric(data, ["PercentQ30", "Q30Percent"])

    # 2) If missing, derive from lane-level keys (weighted by NumPolonies when available)
    lanes = data.get("Lanes") if isinstance(data, dict) else None
    if isinstance(lanes, list) and lanes:
        if pf_val is None:
            weighted = []
            for lane in lanes:
                if not isinstance(lane, dict):
                    continue
                lane_pf = parse_numeric(lane.get("PercentPassFilter"))
                lane_pf_key = "PercentPassFilter"
                if lane_pf is None:
                    lane_pf = parse_numeric(lane.get("PercentAssignedReads"))
                    lane_pf_key = "PercentAssignedReads"
                if lane_pf is None:
                    continue
                weight = parse_numeric(lane.get("NumPolonies"))
                if weight is None or weight <= 0:
                    weight = 1.0
                weighted.append((lane_pf, weight, lane_pf_key))
            if weighted:
                pf_val = sum(v * w for v, w, _ in weighted) / sum(w for _, w, _ in weighted)
                pf_key = f"Lanes[].{weighted[0][2]}"

        if q30_val is None:
            weighted = []
            for lane in lanes:
                if not isinstance(lane, dict):
                    continue
                lane_q30 = parse_numeric(lane.get("PercentQ30"))
                if lane_q30 is None:
                    continue
                weight = parse_numeric(lane.get("NumPolonies"))
                if weight is None or weight <= 0:
                    weight = 1.0
                weighted.append((lane_q30, weight))
            if weighted:
                q30_val = sum(v * w for v, w in weighted) / sum(w for _, w in weighted)
                q30_key = "Lanes[].PercentQ30"

    # 3) Final fallback: generic flattened search
    flat_items = list(flatten_json(data))
    if pf_val is None:
        pf_hit = metric_from_flat(flat_items, [
            "percentPassFilter", "passFilterPercent", "percentPf", "percentAssignedReads"
        ])
        if pf_hit:
            pf_key, pf_val = pf_hit
    if q30_val is None:
        q30_hit = metric_from_flat(flat_items, ["percentQ30", "q30Percent"])
        if q30_hit:
            q30_key, q30_val = q30_hit

    pf = normalize_percent(pf_val)
    q30 = normalize_percent(q30_val)
    if pf is None and q30 is None:
        return None

    return {
        "RunPF": round(pf, 3) if pf is not None else None,
        "RunQ30": round(q30, 3) if q30 is not None else None,
        "RunPFPath": pf_key or "",
        "RunQ30Path": q30_key or "",
    }

results = []
statuses = []
for run_dir in sorted(qc_dir.glob("mask_*")):
    metric_tuple = None

    metric_tuple = extract_from_metrics_csv(run_dir / "Metrics.csv")

    if metric_tuple is None:
        log_path = run_dir / "run.log"
        if log_path.exists():
            metric_tuple = extract_from_log(log_path.read_text(errors="ignore"))

    if metric_tuple is None:
        html_candidates = sorted(run_dir.glob("*_QC.html")) + sorted(run_dir.glob("multiqc_report.html"))
        reports_dir = run_dir / "Reports"
        if reports_dir.exists():
            html_candidates += sorted(reports_dir.glob("*Demultiplex*.html"))

        for html in html_candidates:
            content = html.read_text(errors="ignore")
            metric_tuple = extract_from_html(content)
            if metric_tuple is not None:
                break

    if metric_tuple is None:
        statuses.append((run_dir.name, "no parsable metrics"))
        continue

    pf, q30, source = metric_tuple
    parts = run_dir.name.split("_", 2)
    mask = parts[2] if len(parts) > 2 else "unknown"
    score_raw = (pf * q30) / 100.0
    results.append({
        "Mask": mask,
        "%Assigned": round(pf, 3),
        "Q30%": round(q30, 3),
        "Score": round(score_raw, 6),
        "ScoreRaw": score_raw,
        "Source": source,
        "Folder": run_dir.name,
    })

    run_json = extract_from_runstats_json(run_dir / "RunStats.json")
    if run_json is not None:
        results[-1].update(run_json)

    statuses.append((run_dir.name, f"ok ({source})"))

if not results:
    print("\n## MASK DIAGNOSTIC")
    print("No parsable mask metrics found.")
    print("\nMask run status:")
    for name, status in statuses:
        print(f"- {name}: {status}")
    sys.exit(2)

results.sort(key=lambda x: x["ScoreRaw"], reverse=True)

print("\n## MASK DIAGNOSTIC")
print(f"{'Mask':50} {'%Assigned':>10} {'Q30%':>10} {'Score':>10} {'Src':>10}")
for r in results:
    print(f"{r['Mask'][:50]:50} {r['%Assigned']:10.3f} {r['Q30%']:10.3f} {r['Score']:10.6f} {r['Source']:>10}")

non_ok = [(name, status) for name, status in statuses if not status.startswith("ok")]
if non_ok:
    print("\nMask run status:")
    for name, status in non_ok:
        print(f"- {name}: {status}")

best = results[0]
print(f"\n🎯 RECOMMEND: {best['Mask']} (Score: {best['Score']:.6f})")

json_rows = [r for r in results if r.get("RunPF") is not None or r.get("RunQ30") is not None]
if json_rows:
    print("\n## RUNSTATS JSON")
    print(f"{'Mask':50} {'RunPF%':>10} {'RunQ30%':>10}")
    for row in json_rows:
        run_pf = f"{row['RunPF']:.3f}" if row.get("RunPF") is not None else "n/a"
        run_q30 = f"{row['RunQ30']:.3f}" if row.get("RunQ30") is not None else "n/a"
        print(f"{row['Mask'][:50]:50} {run_pf:>10} {run_q30:>10}")

if len(results) > 1:
    assigned_values = {r["%Assigned"] for r in results}
    q30_values = {r["Q30%"] for r in results}
    if len(assigned_values) == 1 and len(q30_values) == 1:
        print("⚠️ All masks have identical metrics; this can be real, but often indicates non-discriminatory parsing/source data.")

summary_csv = output_base / "mask_integration_summary.csv"
with summary_csv.open("w", newline="", encoding="utf-8") as handle:
    writer = csv.DictWriter(handle, fieldnames=["Folder", "Mask", "%Assigned", "Q30%", "Score", "Source", "RunPF", "RunQ30"])
    writer.writeheader()
    for row in results:
        out_row = {k: row.get(k, "") for k in ["Folder", "Mask", "%Assigned", "Q30%", "Score", "Source", "RunPF", "RunQ30"]}
        writer.writerow(out_row)

print(f"Saved summary: {summary_csv}")
PY
