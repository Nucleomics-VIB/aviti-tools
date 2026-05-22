# Project status — aviti_test_mask

**Last updated:** 2026-05-22
**Branch:** `develop`

---

## Purpose

Test a set of `bases2fastq` filter masks against a single AVITI sequencing run and
rank them by a composite score (`%Assigned × Q30% / 100`). Helps identify the best
mask for a run before committing to full demultiplexing.

Two scripts:

| Script | Role |
|--------|------|
| `aviti_test_mask.sh` | Launches one `bases2fastq --qc-only` Docker container per mask |
| `integrate_mask_results.sh` | Parses all QC outputs and prints a ranked summary + CSV |

---

## Features implemented

### Mask management
- Built-in default mask array (9 masks covering Y8–Y18 read lengths)
- `masks.yaml` — externalised default list; edit to customise without touching the script
- `--masks-file / -m` — override the mask list at runtime with any YAML file
  (e.g. uploaded from a web tool); `*` characters are never glob-expanded

### Resource configuration (`config.yaml`)
All values are defaults; every one can be overridden by a CLI flag.

| Key | Default | CLI flag | Description |
|-----|---------|----------|-------------|
| `threads` | `8` | `-p` | bases2fastq worker threads per container |
| `max_jobs` | `4` | `-j` | max concurrent Docker containers |
| `cache_input` | `false` | `--cache-input` | stage input to fast storage before run |
| `mem_limit_per_job` | `~` (none) | — | Docker `--memory` cap per container (OOM prevention) |
| `docker_image` | `elembio/bases2fastq:latest` | — | image pulled at startup; pin for reproducibility |

Custom config file: `-c / --config path/to/config.yaml`

### Parallelism and I/O (see `dev_docs/parallelism.md` for full design)
- **`--jobs N`** — named-pipe semaphore limits concurrent containers; prevents I/O
  thrashing on NAS; default 4
- **`--cache-input`** — copies run directory to `/dev/shm` (Linux tmpfs / RAM) or
  `/tmp` (macOS) once; all containers read from fast local storage; size guard
  prevents staging if free space < 110 % of run size; staged copy removed on exit

### Input handling
- NAS / NFS / SMB mount detection with Docker file-sharing verification
- `abspath()` helper works on macOS (no GNU `realpath` required)
- `BaseCalls/` directory validated before any Docker call

### OOM protection
- `mem_limit_per_job` in `config.yaml` sets `--memory` and `--memory-swap` on each
  container, hard-capping RAM per job and disabling swap expansion
- Example for a 128 GB server with `max_jobs: 4`: set `mem_limit_per_job: 16g` to
  reserve ≥ 64 GB for the OS and other tools

### Result integration (`integrate_mask_results.sh`)
- Pure Python 3 stdlib (no third-party packages); minimum Python 3.6
- Parses `Metrics.csv`, `run.log`, HTML QC reports, and `RunStats.json`
- Prints ranked table, recommends best mask, writes `mask_integration_summary.csv`
- Activates conda env `pythonenv` if available; falls back to any `python3` on PATH

---

## Current status

| Area | State |
|------|-------|
| Core QC loop | Stable |
| YAML mask file | Stable |
| config.yaml loader | Stable |
| RAM staging (`--cache-input`) | Implemented, not yet tested on target server |
| Job pool semaphore (`--jobs`) | Implemented, not yet tested on target server |
| OOM cap (`mem_limit_per_job`) | Implemented; value to be tuned after first server run |
| Docker image pinning | Implemented; still using `latest` — pin once version is confirmed |
| Result integration | Stable |

---

## Deployment target

Ubuntu server, abundant RAM (use `--cache-input`), moderate CPU.
Recommended invocation:

```bash
./aviti_test_mask.sh \
  -i /path/to/run \
  -o ./results \
  --cache-input \
  -j 4          # test with -j 8 to compare throughput
```

---

## Open / next steps

- [ ] Run on Ubuntu server and validate `--cache-input` staging path (`/dev/shm`)
- [ ] Benchmark `-j 4` vs `-j 8` on server hardware
- [ ] Set `mem_limit_per_job` in `config.yaml` after first run (tune to available RAM)
- [ ] Pin `docker_image` to a specific `bases2fastq` version once confirmed stable
- [ ] Consider web-tool integration: mask list upload → `--masks-file` path
- [ ] Merge `develop` → `main` once server validation passes
