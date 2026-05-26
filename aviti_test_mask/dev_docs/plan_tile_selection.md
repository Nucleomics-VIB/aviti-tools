# Plan — tile selection for `aviti_test_mask.sh`

**Status:** proposed
**Source:** [todo.md](../todo.md) — single-tile default gives an unrepresentative
QC verdict (n=1 patch); some tiles hit bubbles, debris, edge effects, fluidics
streaks. Need to widen tile selection without paying full-lane cost every time.

---

## Goal

Let the user pass a tile-selection policy to `aviti_test_mask.sh` and have it
forwarded as `bases2fastq --include-tile <pattern>` for every mask run.

Selection policies, simplest → richest:

| Form | Example | Resolves to |
|---|---|---|
| Raw pattern (passthrough) | `--include-tile 'L1R..C..S.'` | unchanged |
| Lane preset | `--tiles lane:1` | `L1R..C..S.` |
| All-lanes preset | `--tiles all` | `L.R..C..S.` |
| Distributed sample | `--tiles spread:8` | 8 tiles spread across rows × cols × surfaces, OR'd into one pattern |
| Random sample | `--tiles random:N[:seed]` | N tiles sampled from the run's actual tile inventory |

Default: unchanged (single tile, bases2fastq default) — backwards-compatible.

---

## Why both `spread` and `random`

- **`spread:N`** — deterministic, picked from a known geometric template
  (rows 1–3, cols 1–3, surfaces 1–2). No I/O on the run folder, no manifest
  parsing. Good enough for "give me a representative QC verdict" without
  reading the run.
- **`random:N`** — requires enumerating the actual tiles present in this run
  (a run may not contain every theoretical tile). Reproducible via `:seed`.
  Use when you want a true random sample of *this* run, not the template.

`spread` is the recommended default-when-you-want-more-than-one; `random` is
for users who want sampling-with-replacement-style honesty.

---

## CLI / config surface

**New flag:** `--tiles <spec>` (also `-t`)

`<spec>` accepts:
- a raw bases2fastq pattern (contains `L` and digit/regex chars) → passthrough
- `all` | `lane:N` | `spread:N` | `random:N[:seed]` → expanded by the script

**New config.yaml key:** `tiles: <spec>` (defaults to empty = single-tile)

**Precedence:** CLI `--tiles` > config `tiles:` > unset (default).

**Help text** must show the spec grammar and one worked example.

---

## Implementation outline

All changes live in `aviti_test_mask.sh`. No new external deps.

1. **Config loader** (around lines 47–74): add `tiles` to the YAML key map →
   `INCLUDE_TILE_SPEC` variable.
2. **Arg parser** (around line 78–119): add `-t|--tiles` case.
3. **New function `resolve_tiles_spec()`** — pure bash, returns the
   `--include-tile` pattern to pass to bases2fastq:
   - empty spec → empty string (caller omits the flag entirely)
   - matches `^L[0-9.]` → passthrough
   - `all` → `L.R..C..S.`
   - `lane:N` → `L${N}R..C..S.`
   - `spread:N` → pick N positions from a small fixed template
     (rows {1,2,3} × cols {1,2,3} × surfaces {1,2} = 18 candidates), join with `|`
   - `random:N[:seed]` → enumerate tiles from input (see below), shuffle with
     `awk -v seed=`, take N, join with `|`
4. **Tile enumeration for `random:`** — read `RunStats.json` or
   `RunManifest.json` from `$INPUT_ABS` and extract tile IDs. If neither file
   exists or the schema doesn't match, fail loudly (`echo` + `exit 1`) — do
   not silently fall back, because the user explicitly asked for randomness.
5. **`run_mask_qc()`** (line 351): inject `--include-tile "$INCLUDE_TILE"`
   into the docker `bases2fastq` line when non-empty.
6. **Status echo** at startup: print the resolved tile pattern so the user
   sees exactly what bases2fastq will see.

---

## Tile-source schema (confirmed on dev data)

`RunParameters.json` has a top-level `Tiles` key — a flat list of strings
matching the bases2fastq tile-ID format:

```json
"Tiles": ["L1R06C01S1", "L1R06C01S2", "L1R07C01S2", "L1R07C01S1", "L1R08C01S1", ...]
```

Dev run `20260212_AV224503_5167_1` has 240 entries. `random:` and `spread:`
both read from this list — `spread:` falls back to the geometric template
only if the list is missing.

---

## Cost / data-transfer note

From [todo.md](../todo.md): the more tiles included, the more of the
BaseCalls tree must be present locally. Document this in `--help` and in the
README — single-tile is the "did the run basically work" smoke test;
`spread:8`+ is the representative QC verdict; `all`/`lane:` is the
gold-standard but defeats minimal-copy-over-SSH savings.

---

## Test plan (on chicken)

After the current `--cache-input` + `-j` validation completes:

1. Re-run one mask with `--tiles spread:8` — confirm bases2fastq accepts the
   OR-joined `--include-tile` pattern.
2. Compare Q30 / %Assigned between single-tile and `spread:8` for the same
   mask — quantify how much the verdict moves.
3. Time `spread:8` vs `lane:1` vs default — show the speed-vs-representativeness
   tradeoff in numbers.

---

## Out of scope

- Per-mask tile selection (different tiles for different masks). Single
  selection applies to all masks in a run.
- Adaptive sampling ("if first tile looks bad, widen automatically"). Keep
  the script declarative.
- Visualising per-tile metrics — that lives in `integrate_mask_results.sh`
  follow-up, not here.

---

## Open questions

1. Does `bases2fastq --include-tile` actually accept a pipe-separated
   alternation, or does it need repeated `--include-tile` flags? **Check on
   first chicken run.** If repeated flags are required, `run_mask_qc()` builds
   a `--include-tile X --include-tile Y …` array instead of one joined string.
2. Is the tile inventory in `RunStats.json` or `RunManifest.json`? Confirm
   before implementing `random:`.
3. Should `random:N` without an explicit seed be reproducible (auto-seed from
   run ID) or genuinely random per invocation? Recommend auto-seed from input
   path so repeat runs are reproducible by default; users add `:seed=...` to
   force a fresh draw.
