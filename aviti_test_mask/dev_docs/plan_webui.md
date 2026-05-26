# Plan — Web UI for `aviti_test_mask`

**Status:** proposed
**Depends on:** [plan_tile_selection.md](plan_tile_selection.md) (tile spec must land first so the UI can expose it)
**Source:** [todo2.md](../todo2.md)

---

## Development environment (decided)

- **Host:** this Mac, exclusively. The chicken Ubuntu server is a deploy
  target only.
- **NAS:** real, already mounted at
  `/Volumes/lvs/GBW-0047_NUC_Transfers/0003_Runs/Aviti/`.
- **Docker:** [colima](https://github.com/abiosoft/colima) — `colima
  start` provides a Linux VM that hosts the Docker daemon. The
  `linux/amd64` platform flag in `aviti_test_mask.sh` already accounts
  for the M2 architecture mismatch.
- **Dev test run:** `20260522_AV224503_5279_1` (latest run downloaded
  by the user; full BaseCalls, ~102 GB).
- **Python:** **dedicated conda env**, never the system or user
  Python. See "Conda environment" below.
- **Production target:** chicken (Ubuntu, native Docker, NAS mount
  path TBD). Same code, same Dockerfile — only the mount paths in
  compose differ.

### Conda environment

Hard rule: **every dependency — Python packages and unix tools alike —
lives in a dedicated conda env.** The production host is assumed to
provide only `bash` and `docker`; any other binary the tool invokes
(`parallel`, `gawk`, `sed`, `grep`, `findutils`, `jq`, `yq`,
`coreutils`, …) ships from `environment.yml`. Adding a new tool means
adding it to the env, never `apt install`-ing on the host. This
applies to dev *and* the Stage 2 Docker image base.

Layout:

```
webui/
  environment.yml          # canonical env spec, checked in
  Makefile                 # `make env` / `make run` / `make test` wrappers
```

`environment.yml` (illustrative):

```yaml
name: aviti_test_mask_webui
channels:
  - conda-forge
dependencies:
  - python=3.11
  - flask>=3.0
  - jinja2
  - pyyaml
  - humanfriendly
  - pip
  - pip:
      - itsdangerous       # if pulled in by flask
```

Activation in the dev loop:

```bash
conda env create -f webui/environment.yml      # one-time
conda activate aviti_test_mask_webui
python -m flask --app webui/app.py run --debug
```

Every script, test, and Makefile target assumes the env is activated;
the Makefile prints a helpful error and exits if `which python`
doesn't point inside the env.

`requirements.txt` referenced earlier in this plan is **superseded by
`environment.yml`** — keep only one source of truth.

---

## Goal

Give users a browser form to:

1. Pick an AVITI run discovered under a NAS-mounted share.
2. Tune every parameter currently in `config.yaml` + the new `--tiles` flag.
3. Pick which masks to test (subset of `masks.yaml`).
4. Launch `aviti_test_mask.sh`, watch progress, and view a rendered
   `results.html` produced from `integrate_mask_results.sh`.

Two delivery stages — same code, same API:

- **Stage 1 — local:** Python backend + static `index.html`, run on the
  analysis host (chicken). Single-user, single-machine.
- **Stage 2 — Docker:** same backend in a container, NAS mounted read-only,
  Docker socket mounted so the container can spawn `bases2fastq` containers
  as siblings (docker-out-of-docker).

The Stage 1 → Stage 2 transition must require **zero code changes** — only
packaging. That constraint shapes every decision below.

---

## A pure-static `index.html` is not enough

`todo2.md` says "a local index.html page in the initial version." A static
file alone can't:

- List directories on the host (NAS run discovery)
- Spawn `aviti_test_mask.sh`
- Read live logs

So Stage 1 still needs a tiny local HTTP backend. The page is static; the
*server* is the local component. Recommended: **Python 3 + Flask** (already
have python3 on chicken; one `pip install flask` away). Alternatives
considered:

| Option | Pro | Con |
|---|---|---|
| `python3 -m http.server` + CGI | zero deps | CGI is fragile, no streaming logs |
| Flask | clean routing, easy to dockerize, streams via SSE | one dep |
| FastAPI | async, websockets | overkill for this UI |
| Node/Express | ecosystem | adds a runtime we don't have |

**Pick Flask.** Pin in `requirements.txt`. Lives under `webui/` next to the
shell scripts.

### Frontend stack — JS where it earns its keep, no framework tax

The UI itself is JS-heavy by necessity (live log tail via SSE, polling job
state, dynamic form, progressive dropdown fill from async validation).
**Use modern vanilla JavaScript with ES modules — no React/Vue/build
step.** Rationale:

- The form has ~6 sections and ~15 inputs. A framework adds more code
  than it saves at this size.
- No build step = no npm in the deploy chain = simpler Stage 2 Docker
  image. The browser loads `.js` files directly.
- Vanilla ES modules already give clean separation (one module per
  concern). The maintainability constraint the user named is about
  *module boundaries*, not about framework choice.
- Style with plain CSS + a small utility layer (or one CDN-loaded
  Pico.css/Water.css link) — no PostCSS, no Sass.

If a later iteration outgrows vanilla JS (rich charts, drag-and-drop
mask reordering, etc.), introduce **Alpine.js** or **HTMX** before
reaching for a SPA framework — both are sprinkle-on libraries that
don't force a rewrite or a build pipeline.

### Modularity & maintainability rules

These apply to both Python and JS code in `webui/`:

1. **One module = one responsibility.** Files in the layout already
   reflect this (`discovery.py`, `job_manager.py`, `report_render.py`
   on the backend; per-concern JS modules on the frontend — see
   below). If a file starts mixing concerns, split it before it hits
   ~300 lines.
2. **Pure functions where possible.** `discovery.scan_nas_for_runs`,
   `discovery.validate_run`, `resolve_tiles_spec`, and the JS
   `validators.js` should be pure — no Flask globals, no DOM access
   beyond their entrypoint. Unit-testable from a REPL.
3. **No hidden globals.** The Flask app holds config in a single
   `AppConfig` object passed explicitly. JS modules export named
   functions and never write to `window.*`.
4. **Server is authoritative.** Any validation/transform that affects
   correctness lives in Python and is mirrored client-side for UX
   only. Never two sources of truth.
5. **No comment-as-documentation.** Names carry meaning; comments
   reserved for non-obvious *why*.
6. **One config schema.** `webui/schema.py` defines every field with
   its validator. Both the Flask form handler and the JS form
   builder consume that schema (exposed via `GET /api/v1/schema`),
   so adding a new field touches exactly one place.

### Updated file layout

```
webui/
  app.py                # Flask routes only — thin
  config_loader.py      # load + merge config.yaml + env overrides
  schema.py             # single source of truth for form fields + validators
  discovery.py          # scan_nas_for_runs(), validate_run()
  job_manager.py        # spawn, track, kill, rehydrate jobs
  report_render.py      # CSV → results.html via Jinja2
  purge.py              # session-folder retention sweep
  requirements.txt      # flask + jinja2 + pyyaml + humanfriendly
  Dockerfile            # Stage 2
  static/
    index.html
    results.html.j2     # Jinja2 template
    css/
      app.css
    js/
      main.js           # entrypoint; wires modules
      api.js            # fetch wrappers for /api/v1/*
      schema.js         # consumes /api/v1/schema → renders form
      validators.js     # client-side mirror of schema rules
      runs.js           # run picker + progressive dropdown
      tiles.js          # tile-spec UI (per plan_tile_selection)
      job-view.js       # status polling + SSE log tail
      report-link.js    # opens results.html in new tab
  tests/
    test_discovery.py
    test_validation.py
    test_schema.py
    test_purge.py
    test_job_manager.py
```

This layout replaces the earlier "New code" sketch — same idea,
sharper module boundaries.

---

## Config extensions (`config.yaml`)

New keys, all optional with sane defaults:

```yaml
# --- NAS / run discovery ---
# Dev path on this Mac (will change on the final host — likely something like
# /mnt/nas/GBW-0047_NUC_Transfers/0003_Runs/Aviti on chicken).
nas_root: /Volumes/lvs/GBW-0047_NUC_Transfers/0003_Runs/Aviti
sequencer_subdirs_glob: "AV*"            # auto-discover AVITI sequencer dirs by prefix
# Explicit list also accepted (overrides the glob):
# sequencer_subdirs:
#   - AV224503
#   - AV232702
#   - AV242402
run_folder_marker: RunManifest.json      # file that identifies a run folder
run_folder_regex: '^\d{8}_AV\d+_\d+_\d+$' # YYYYMMDD_AVxxxxxx_NNNN_N
run_age_days: 365                        # only show runs younger than this

# --- Tile selection (from plan_tile_selection.md) ---
tiles: ""                                # spec: empty | all | lane:N | spread:N | random:N[:seed] | raw pattern

# --- Web UI ---
webui:
  host: 127.0.0.1                        # bind address
  port: 8765                             # default port
  results_root: /data/analyses/aviti_test_mask/results   # where job outputs land
  jobs_dir: /data/analyses/aviti_test_mask/jobs          # per-job metadata + logs
  max_concurrent_jobs: 3                 # max simultaneous submissions / "users" running at once
  max_jobs_per_user: 1                   # one active job per submitter (fair-share)
  queue_when_full: true                  # accept submission, queue, surface position
  max_inner_jobs: 3                      # ceiling for the -j flag of aviti_test_mask.sh (inner mask parallelism)
                                          # Caps NAS-read load: at most max_concurrent_jobs × max_inner_jobs
                                          # = 3 × 3 = 9 bases2fastq containers reading the run folder.
  retain_jobs_days: 30                   # auto-prune session folders older than this
  retain_jobs_min_keep: 5                # keep at least this many newest sessions even if all are older
  purge_on_each_request: true            # run a cheap purge sweep on every UI request
```

Existing keys (`threads`, `max_jobs`, `cache_input`, `mem_limit_per_job`,
`docker_image`) stay untouched; the UI just exposes them.

---

## New code

```
webui/
  app.py                # Flask app, routes only — thin
  discovery.py          # scan_nas_for_runs() — pure function, unit-testable
  job_manager.py        # spawn, track, kill jobs; writes job metadata
  report_render.py      # CSV → results.html
  static/
    index.html          # the form
    results.html        # template, rendered per job
    style.css
    app.js              # form logic, polling, log tail
  templates/            # Jinja2 if needed
  requirements.txt      # flask==3.x
  Dockerfile            # Stage 2
  README.md
```

`discovery.py`, `job_manager.py`, `report_render.py` are pure Python with no
Flask imports — they can be unit-tested without a server and reused by a CLI
later if needed.

---

## Run discovery

`discovery.scan_nas_for_runs(nas_root, sequencer_subdirs, marker, max_age_days) → list[Run]`

For each `<nas_root>/<sequencer>/*/` directory:

1. Existence check on `<run>/<marker>` (default `RunManifest.json`).
2. `mtime` filter against `max_age_days` (cheap pruning).
3. Parse the marker JSON to extract: `RunName`, `RunId`, `InstrumentName`,
   `StartDate`, `Lanes`, `Cycles`. Tolerate missing fields — return `None`
   for any unknown key, never crash.
4. Return list sorted newest-first.

Errors that *must not* crash discovery (log and skip):
- Unreadable directories (permissions)
- Malformed JSON in `RunManifest.json`
- Symlinks pointing nowhere
- NAS share unmounted (return empty list + a clear warning surfaced to the UI)

NAS-unreachable is the single most common failure mode here. Detect by
checking `os.path.ismount(nas_root)` if a mount is expected, and surface
"NAS not mounted" as a banner on the UI, not a 500.

---

## Run-folder validation (must pass before a run is listed)

Discovery only finds *candidates*. A folder named like a run can still be:

- mid-copy from the instrument (partial upload)
- mid-copy from another host over rsync (zip files missing)
- a previous run whose upload was interrupted
- restored from backup with some shards missing

Listing such a folder and letting the user launch it wastes minutes of
compute that fail late inside `bases2fastq`. So every candidate must pass
validation before it appears in the UI.

### What we check (cheap → expensive, short-circuit on first failure)

1. **Top-level markers** (`os.path.exists`, microseconds):
   - `RunManifest.json` (already used by discovery)
   - `RunParameters.json` (required — drives the cycle-count check below)
   - `RunAnalysisFilesUploaded.json` — instrument writes this when upload
     completes. Its presence is the strongest "transfer finished" signal.
   - `AvitiRunStats.json` — sanity, shouldn't be absent on a finished run
   - `BaseCalls/`, `Filter/`, `Location/`, `Alignment/` directories
2. **Parse `RunParameters.json`** — extract the `Cycles` dict
   (`{R1: 151, R2: 151, I1: 10, I2: 10}` on the dev data). If parsing
   fails, reject immediately.
3. **Expected zip set** — for every read in the `Cycles` dict, expect
   files `<read>_C<NNN>.zip` for `NNN = 001..cycles[read]` under
   `BaseCalls/`. Compute the expected set, compare to actual
   `os.listdir('BaseCalls')`. Missing files → invalid.
   - Confirmed against dev data: `Cycles={'R1':151,'R2':151,'I1':10,'I2':10}`
     → 322 zip files; that matches the actual count.
4. **Per-zip size sanity** — every expected zip must be > 0 bytes (catch
   half-written files). A single `os.stat` per file, ~milliseconds total
   even at 322 files.
5. **Per-zip magic byte check (optional, deepest)** — read first 4 bytes
   of each zip, expect `PK\x03\x04`. Catches truly corrupt files but
   adds I/O. **Gated by a config flag `deep_validate: false` (default
   off)**; enable on hosts where this matters.
6. **Lane consistency** — `RunParameters.AnalysisLanes` (e.g. `1+2`)
   declares which lanes will be analyzed; on the dev data the zip files
   are not lane-suffixed but lane info lives inside the zips, so this
   check is informational only — log the value, don't gate on it.

### What we do not check

- Zip contents (would require unzipping — too slow).
- Checksums (not provided by the instrument; nothing to compare against).
- `Alignment/`, `Filter/`, `Location/` *contents* — existence only.
  bases2fastq reads them lazily and will fail clearly if a file is
  missing; replicating that logic adds maintenance burden.

### Validation result schema

`discovery.validate_run(path) → ValidationResult`:

```python
{
  "valid": True | False,
  "checks": [
    {"name": "RunManifest.json", "ok": True},
    {"name": "RunParameters.json", "ok": True},
    {"name": "RunAnalysisFilesUploaded.json", "ok": True,
     "note": "upload completed 2026-02-12T18:24Z"},
    {"name": "basecalls.zip_count", "ok": True,
     "expected": 322, "actual": 322},
    {"name": "basecalls.zip_sizes", "ok": True},
    {"name": "basecalls.zip_magic", "ok": True, "skipped": True,
     "reason": "deep_validate=false"},
  ],
  "first_failure": None,   # or the failed check dict
  "duration_ms": 87,
}
```

The UI shows `valid: true` runs in the dropdown. Failed runs are listed
*separately* in a collapsed "Incomplete or invalid runs (N)" section with
the first-failure reason, so users can see what's wrong rather than
wonder why their run isn't there. Common reasons (mid-copy, missing
upload marker) get plain-English explanations.

### Cost & caching

Validation cost on dev data, per run folder:

| Phase | Time |
|---|---|
| Top-level marker checks | < 1 ms |
| Parse RunParameters.json | 1–3 ms |
| Build expected set | < 1 ms |
| `os.listdir('BaseCalls')` (322 files) | 5–20 ms on NAS |
| `os.stat` per zip | ~50–100 ms total on NAS |
| Magic byte read (deep, optional) | 1–3 s on NAS, ~322 reads |

So default validation is ~100 ms per run on NAS. With 100 runs across
three sequencers that's ~10 s — already slow enough to be a UI problem
on cold load.

**Cache validation results** in `<jobs_dir>/.cache/validation.json`,
keyed by `(absolute path, mtime of RunManifest.json, mtime of
RunAnalysisFilesUploaded.json)`. Cache hit is free; cache miss runs the
checks. The mtime keys mean any post-validation change to those marker
files invalidates the entry automatically.

Cache also stores results for *invalid* runs so we don't re-walk a
half-copied folder on every refresh. TTL = 24 h to handle the case
where a slow rsync eventually completes — after 24 h we re-check.

### Async validation pattern

On `GET /api/v1/runs`:
1. Return cached results immediately (might be empty on first load).
2. In the background, validate any candidate folder not in the cache.
3. UI polls `/api/v1/runs` again after a short delay if its response
   contains `validating: N`. Once `validating: 0`, the list is final.

This avoids a multi-second blocking call on first page load — the form
renders, the dropdown fills in as runs are confirmed valid.

### Manual override

A `?force=true` query param on `/api/v1/runs/<id>` re-runs validation
ignoring the cache. Exposed as a "re-check" button next to each invalid
run, so the user can prod after fixing the underlying issue.

---

## HTTP API

Versioned under `/api/v1/` so Stage 2 can evolve.

| Method | Path | Purpose |
|---|---|---|
| GET | `/api/v1/health` | liveness; reports NAS mount status |
| GET | `/api/v1/config` | current `config.yaml` (sanitised) |
| GET | `/api/v1/masks` | parsed `masks.yaml` |
| GET | `/api/v1/runs` | discovered runs, newest first |
| GET | `/api/v1/runs/<id>` | one run's full metadata |
| POST | `/api/v1/jobs` | create + start a job; body = full param set |
| GET | `/api/v1/jobs` | list jobs (running, done, failed) |
| GET | `/api/v1/jobs/<id>` | job status + per-mask progress |
| GET | `/api/v1/jobs/<id>/log` | SSE stream of combined log |
| GET | `/api/v1/jobs/<id>/report` | rendered `results.html` |
| POST | `/api/v1/jobs/<id>/cancel` | SIGTERM the bash job + its children |

All responses JSON except `/log` (SSE) and `/report` (HTML).

---

## Job lifecycle

1. `POST /api/v1/jobs` validates the payload (see "Validation" below), assigns
   `job_id = <runId>__<utc_iso>__<shortuuid>`, writes
   `<jobs_dir>/<job_id>/params.json`, and forks a worker.
2. Worker invokes `aviti_test_mask.sh` with the resolved flags, redirecting
   stdout+stderr to `<jobs_dir>/<job_id>/run.log` and writing a sentinel
   `<jobs_dir>/<job_id>/state` (`queued|running|integrating|done|failed|cancelled`).
3. On `aviti_test_mask.sh` exit:
   - exit 0 → set state `integrating`, invoke `integrate_mask_results.sh`,
     then `report_render.py` → `<jobs_dir>/<job_id>/results.html`, then
     `done`.
   - exit ≠ 0 → set state `failed`, capture the per-mask
     success/failure table from the bash script's output, still attempt
     `integrate_mask_results.sh` on whatever succeeded (partial report
     better than no report).
4. The UI polls `/api/v1/jobs/<id>` every 2 s and tails `/log` via SSE.

## Concurrent users — multiple colleagues, same host

Several lab members may hit the UI at once. The system must accept
simultaneous submissions, run what it can in parallel, queue the rest,
and never let one user's job starve another's.

### Resource budget on chicken

48 cores, 251 GiB RAM. A single mask container uses `THREADS` cores
(default 8) and a few GB RAM. One `aviti_test_mask.sh` invocation
parallelises masks with `-j MAX_JOBS` (default 4) → ~32 cores per job
suite, ~16 GB peak.

Hosting 3 simultaneous suites at the defaults already exceeds 48 cores.
**Don't oversubscribe — queue instead.** Defaults:

| Setting | Default | Reason |
|---|---|---|
| `max_concurrent_jobs` | 3 | up to 3 "users" running simultaneously |
| `max_jobs_per_user` | 1 | fair-share so one user can't claim all 3 slots |
| `max_inner_jobs` | 3 | -j ceiling per submission |
| **`max_global_containers`** | **3** | **hard global cap on simultaneous bases2fastq containers (the binding limit)** |
| `webui.threads` ceiling | 8 | per-mask thread cap |

**The two knobs are inclusive — global container cap is 3, period.**
A single global semaphore of size `max_global_containers` gates every
bases2fastq launch. The other two knobs just shape how those 3 slots
get distributed:

| Scenario | Slot allocation |
|---|---|
| 1 user, 1 submission | up to 3 inner masks run in parallel (3 slots used by one job) |
| 2 users simultaneously | first job gets 2 slots, second gets 1 — or 1+1 with one slot idle, whichever the queue lands on |
| 3 users simultaneously | each gets 1 slot; all three run one mask at a time, masks beyond the first are queued per-job |
| 4+ users | first 3 run as above; the 4th onward sits in the user-level queue |

Worst-case NAS read load: **3 concurrent bases2fastq readers, ever.**
Worst-case CPU: 3 × 8 = 24 threads (half of chicken's 48), leaving
plenty of headroom for other lab work on the host.

### Submission queue

The Flask app keeps a single in-memory queue + a worker pool of size
`max_concurrent_jobs`. **State is on disk** (per the existing
job-lifecycle section) so the queue can be rebuilt from
`<results_root>/*/state` files on restart — see "Crash recovery"
below.

`POST /api/v1/jobs` always returns 202 (accepted), never blocks:

```json
{
  "job_id": "20260522_AV224503_5279_1__2026-05-26T09-14-02Z__a1b2c3",
  "state": "queued",
  "queue_position": 2,
  "estimated_start": "in ~14 minutes"
}
```

The UI subscribes to `/api/v1/jobs/<id>` and updates the position as
slots free up. When the worker pool has a free slot, the oldest queued
job whose owner has no active job moves to `running`.

### Identifying "the user"

Stage 1 has no login. Identify the submitter by the form field
`submitter` (free-text name/email, required, persisted in a browser
cookie so it pre-fills). Stage 2 can swap this for a real auth header
without changing the queue logic — the queue only needs *some* opaque
string per submitter.

Combined with `max_jobs_per_user`, this is enough to keep one
enthusiastic colleague from filling all 3 slots with their own runs.

### File-system isolation

Per-session folders (already designed) plus distinct `job_id`s mean
two simultaneous jobs never collide on disk. The Docker invocation in
`run_mask_qc()` mounts `-v "$outdir:/output"` where `$outdir` is
inside the session folder — already collision-free.

One sharp edge: `--cache-input` stages to `/dev/shm`. With 3 concurrent
jobs each staging a ~300 GB BaseCalls tree, tmpfs blows up. So:

- **Stage each cache to a per-job subdir** of `/dev/shm` (e.g.
  `/dev/shm/aviti_cache/<job_id>`), not a shared path.
- **Pre-flight check**: before accepting `cache_input=true`, compute
  estimated stage size and require `free(/dev/shm) ≥ 1.2 × size +
  Σ(estimated sizes of other running --cache-input jobs)`. If not, the
  job either queues until shm has room, or runs without caching (user
  picks via a `on_cache_full: queue|fallback_no_cache` config knob).
- **Always clean up cache on job end**, regardless of success — even
  on SIGKILL via the trap in `aviti_test_mask.sh`. Verify this is
  already wired; if not, add it as a prerequisite.

### Docker daemon contention

Three simultaneous `bases2fastq` containers pulling the same image,
hitting the same Docker socket — Docker handles this fine, but:

- Pull the image **once at server startup**, not at job time. Already
  prepped on chicken; add to UI server startup as belt-and-braces.
- Set per-container `--cpus` and `--memory` from `mem_limit_per_job` —
  already in place; document the importance under multi-user load.

### Run-folder lock (don't double-process the same run)

If two users pick the same run *and* the same params, accept the
second submission but warn ("Job X by Alice is already running on this
run with identical params — your job will produce duplicate output").
Hard-block only if the *output folder* would collide — and it won't,
because session folders are unique by uuid.

### Crash recovery

On `Flask` start (or systemd restart), `job_manager.rehydrate()`:

1. Scan `<results_root>/*/state` files.
2. For each:
   - `done|failed|cancelled` → leave alone (visible in history)
   - `queued` → put back on the queue (in original order, by folder mtime)
   - `running|integrating` with a live PID → re-attach by tailing the
     `run.log`; assume the subprocess survived (it's detached)
   - `running|integrating` with a dead PID → mark `failed` with a
     clear "server restarted mid-run" message; the user can re-submit

### Surfacing concurrency in the UI

- A persistent header bar shows "N jobs running · M queued" pulled
  from `/api/v1/jobs?state=running,queued`.
- The job-status view shows queue position when queued, ETA when
  running.
- A simple admin page `/jobs/all` (no auth in Stage 1) lists every
  job with submitter, state, started, ETA — so colleagues can see
  whose work is currently using the cores.

### Why not `max_concurrent_jobs: 1` with internal `-j` parallelism

Tempting because `aviti_test_mask.sh` already parallelises masks
internally. But single-job-at-a-time means a 2-hour run by Alice
blocks Bob's 10-minute smoke test entirely. Three slots with reduced
inner parallelism keeps wall-time predictable for everyone.

---

## Job database (SQLite DAO)

A SQLite database is the canonical record of every submission, its
parameters, state transitions, and final outcome. The per-session
folder stays authoritative for the *bulky* artifacts (logs,
`qc_runs/`, the rendered HTML report) but the DB is what the UI reads
to list jobs, compute stats, and drive monitoring.

### Why SQLite (not just files)

- Reading 30 session folders + parsing `state` + `params.json` on
  every UI request scales badly past a few weeks of history. SQLite
  serves the same listing in microseconds.
- Filterable monitoring views ("all failed jobs in the last week by
  user X") become one indexed query rather than a directory walk.
- Survives process restart without rehydration logic — the DB is the
  rehydration source.
- Already a familiar pattern in the user's other webapps (cf.
  `webapp_template` — session auth, action log, SQLite WAL).

### Location

`<webui.jobs_dir>/jobs.db` — alongside the session folders. WAL mode
enabled at startup (`PRAGMA journal_mode=WAL`) so the writer doesn't
block readers, which matters when the UI polls during a long-running
job.

### Schema (v1)

```sql
CREATE TABLE IF NOT EXISTS jobs (
  job_id              TEXT PRIMARY KEY,        -- <runId>__<utc_iso>__<shortuuid>
  submitter           TEXT NOT NULL,           -- from users.yaml selection
  run_id              TEXT NOT NULL,           -- the AVITI run folder name
  run_path            TEXT NOT NULL,           -- absolute path used at submission
  params_json         TEXT NOT NULL,           -- full submitted form, JSON-encoded
  masks_source        TEXT NOT NULL,           -- builtin | uploaded | typed
  masks_json          TEXT NOT NULL,           -- normalised list of masks, JSON
  tiles_spec          TEXT,                    -- the resolved tile spec (empty = default)
  state               TEXT NOT NULL,           -- queued|running|integrating|done|failed|cancelled
  queue_position      INTEGER,                 -- nullable; null once running
  cache_input         INTEGER NOT NULL,        -- 0/1
  threads             INTEGER NOT NULL,
  max_jobs            INTEGER NOT NULL,
  docker_image        TEXT NOT NULL,
  mem_limit_per_job   TEXT,                    -- e.g. "16g", nullable
  submitted_at        TEXT NOT NULL,           -- ISO-8601 UTC
  started_at          TEXT,
  finished_at         TEXT,
  duration_seconds    INTEGER,                 -- computed on completion
  exit_code           INTEGER,                 -- of aviti_test_mask.sh
  mask_count          INTEGER NOT NULL,        -- how many masks the job ran
  masks_succeeded     INTEGER DEFAULT 0,
  masks_failed        INTEGER DEFAULT 0,
  best_score          REAL,                    -- top score from mask_integration_summary.csv
  best_mask           TEXT,                    -- the mask that achieved best_score
  error_message       TEXT,                    -- one-line summary on failure
  cancelled_by        TEXT                     -- submitter / "admin" / null
);

CREATE INDEX IF NOT EXISTS ix_jobs_state ON jobs(state);
CREATE INDEX IF NOT EXISTS ix_jobs_submitter ON jobs(submitter);
CREATE INDEX IF NOT EXISTS ix_jobs_submitted_at ON jobs(submitted_at);

-- Per-mask outcome detail (for the monitoring view, not for the queue)
CREATE TABLE IF NOT EXISTS mask_results (
  job_id     TEXT NOT NULL REFERENCES jobs(job_id) ON DELETE CASCADE,
  mask       TEXT NOT NULL,
  status     TEXT NOT NULL,                   -- ok | failed | skipped
  q30_pct    REAL,
  assigned_pct REAL,
  score      REAL,
  source     TEXT,                            -- csv | html | json (which integrator parser)
  error_msg  TEXT,                            -- e.g. "OOM (exit 137)" when failed
  PRIMARY KEY (job_id, mask)
);

CREATE TABLE IF NOT EXISTS schema_version (
  version INTEGER PRIMARY KEY
);
INSERT OR IGNORE INTO schema_version VALUES (1);
```

### Lifecycle integration

| Step | DB write |
|---|---|
| `POST /api/v1/jobs` accepted | `INSERT` row with `state='queued'`, full params |
| Worker dequeues | `UPDATE state='running', started_at=now()` |
| `aviti_test_mask.sh` exit | `UPDATE state='integrating'` (or `'failed'` if non-zero) |
| Each mask finishes (parsed from log) | `INSERT` into `mask_results` |
| `integrate_mask_results.sh` exit | `UPDATE best_score, best_mask, state='done'` |
| Cancel request | `UPDATE state='cancelled', cancelled_by=...` |
| Purge sweep deletes session | `DELETE FROM jobs WHERE job_id=?` (cascade kills mask_results) |

If a write fails (disk full, locked), the failure is logged but the
job process continues — file-system state is still authoritative for
recovery. The DB is a "fast index," not the irreplaceable record.

### DAO module

`webui/db.py` — single module, no ORM. Reasons:
- The schema is small and stable.
- We want to keep the dependency surface tiny for the Docker image.
- An ORM would obscure the WAL/lock behavior we care about.

```python
# Skeleton (illustrative — not the actual code)
class JobsDAO:
    def __init__(self, path: Path):
        self.path = path
        self._init_schema()
    def insert(self, job: JobRecord) -> None: ...
    def update_state(self, job_id: str, state: str, **fields) -> None: ...
    def get(self, job_id: str) -> JobRecord | None: ...
    def list(self, *, state=None, submitter=None, since=None, limit=100) -> list[JobRecord]: ...
    def add_mask_result(self, job_id: str, mask: str, **fields) -> None: ...
    def delete(self, job_id: str) -> None: ...
    def stats(self, *, since=None) -> dict: ...    # for the monitoring view
```

All methods accept a sqlite3 connection or use a per-call connection
(`with sqlite3.connect(...) as conn`). No long-lived connections —
SQLite + WAL handles concurrent processes fine if every writer
commits quickly.

### Queue management page (`/queue`)

A dedicated page for inspecting and curating *pending* work — jobs in
`queued` state, plus any `running` jobs surfaced for cancellation.
Linked from the top nav with a live badge showing queued count.

**Table columns:**

1. **Position** — queue order, 1, 2, 3, … (only for `queued` state).
2. **Submitted** — `submitted_at` local time.
3. **User** — `submitter`.
4. **Run** — `run_id`.
5. **Masks** — count + source (builtin / uploaded / typed).
6. **Tiles** — resolved spec.
7. **Cache** — yes/no (relevant since `--cache-input` jobs queue for
   `/dev/shm`).
8. **State** — `queued | running | stopping`.
9. **Actions** — per row:
   - **Edit** — opens the original submission form pre-filled; saving
     replaces the queued row in place (only allowed while `queued`).
   - **Delete** — removes the queued job before it starts (immediate
     for `queued`; for `running` it triggers the graceful-drain cancel
     path and the state transitions to `stopping`).
   - **Move up / Move down** — reorder within the queue (drag handle
     also supported). Submitter and admin only.

**Bulk actions toolbar** (above the table):

- **Clear queue (queued only)** — a destructive button styled with a
  red border + warning icon. Click triggers a **two-step modal**:
  1. First modal: "This will cancel **N queued jobs** owned by
     **M users**. Running jobs will not be touched. Type `CLEAR`
     below to confirm." Text input must equal the literal string
     `CLEAR` to enable the action button.
  2. After typing, a second confirm button appears. Clicking it issues
     `POST /api/v1/queue/clear?confirm=CLEAR` and the modal shows a
     progress spinner until all rows are deleted.
- **Cancel my queued jobs** — same flow but scoped to the current
  user; single confirmation (no `CLEAR` typing).

**Permissions (v1, no auth):**

- A user may edit/delete their own queued jobs without confirmation.
- The "Clear queue" button is visible to everyone in v1 but its
  modal is the gating mechanism. Once auth lands, gate the button to
  the admin role.

**API:**

- `GET /api/v1/queue` — rows in queue order, includes `running` jobs
  at the bottom for cancel actions.
- `PATCH /api/v1/jobs/<id>` — update queued job params (server
  re-validates everything).
- `DELETE /api/v1/jobs/<id>` — cancel queued or trigger drain on
  running.
- `POST /api/v1/queue/reorder` — body: `{job_id, new_position}`.
- `POST /api/v1/queue/clear?confirm=CLEAR` — body must echo the
  confirmation token; server rejects 400 otherwise. Returns
  `{deleted: N, skipped_running: M}`.

**State after Clear queue:**

- All previously-queued rows in `jobs` get `state='cancelled'`,
  `cancelled_by='<submitter or "admin">'`, `error_message='cleared
  via /queue'`.
- Session folders for those jobs are removed immediately (they had no
  output yet — nothing to archive).
- DB row stays (with `state='cancelled'`) for the audit trail; it
  shows up in `/history` filtered by cancelled.

**Edge case — clearing while a worker is dequeueing:** the worker
takes a SQLite row-level lock when transitioning a row from
`queued → running`. The clear operation transitions all queued rows
in a single `UPDATE … WHERE state='queued'` transaction — if the
worker is mid-transition on row X, one of the two transactions wins
SQLite's serial order, and the other sees the updated state and skips
its action. No race, no orphaned containers.

---

### History / DB browse page (`/history`)

The primary "what has this tool ever done" view — distinct from the
analytics-oriented `/monitor` page below. Reachable from a top-nav link
on every page.

**Default query:** every row in `jobs`, sorted by `submitted_at DESC`
(newest first), paginated 50 per page.

**Columns shown** (left to right):

1. **Submitted** — `submitted_at`, formatted as `YYYY-MM-DD HH:MM` in
   local time; hover for full UTC ISO.
2. **User** — `submitter`.
3. **Run** — `run_id`, abbreviated to `…AV224503_5279_1` style if long,
   full path in tooltip.
4. **Masks** — `mask_count` total + `masks_succeeded / masks_failed`
   inline (e.g. `5 ✓ / 1 ✗`).
5. **Tiles** — `tiles_spec` or `default (single)` if empty.
6. **State** — coloured pill: queued (grey), running (blue, pulsing),
   integrating (blue), done (green), failed (red), cancelled (amber).
7. **Best mask** — `best_mask`, with `best_score` in subscript.
8. **Duration** — `duration_seconds`, formatted as `Hh Mm` or `Mm Ss`.
9. **Actions** — links: `view` (→ session/results.html if `done`,
   otherwise live job view), `download CSV`, `delete`
   (confirms; gated to own jobs unless admin).

**Filters** (above the table, all combinable):

- Text search (matches `submitter`, `run_id`, `best_mask`,
  `error_message`).
- State multi-select chips: `queued`, `running`, `integrating`,
  `done`, `failed`, `cancelled`. Default: all on.
- User dropdown (populated from distinct `submitter` values in DB).
- Date range (last 24h, last 7d, last 30d, custom).
- Sequencer (`AV*` prefix of `run_id`).

Filters are GET query params (`?state=failed&user=splaisan&since=7d`)
so URLs are shareable and bookmarkable.

**Sort:** clicking a column header toggles sort. Default
`submitted_at DESC`. State is preserved in URL params.

**Empty state:** "No jobs match these filters" with a "clear filters"
link.

**Pagination:** numbered + prev/next, with `?page=N&per=50` in URL.

**Backend:** `GET /api/v1/jobs?state=…&submitter=…&since=…&limit=…&offset=…&order=…`
returns `{rows: [...], total: N, page: M}`. Each row is a flat dict
ready to render.

**Performance:** indices on `state`, `submitter`, `submitted_at` make
all of the above sub-millisecond up to ~100k rows. Far beyond what the
lab will accumulate.

**Refresh:** auto-refresh button (off by default); manual reload
otherwise. No SSE here — the page is read-only.

**Export:** "Download as CSV" button respects current filters,
streams `jobs.csv`.

---

### Monitoring view

A new page `/monitor` (Stage 1: localhost only; Stage 2: gated by the
deferred auth admin role) presents:

- **Live state** — running, queued, finished today / this week.
- **Throughput** — jobs per day, average duration, success rate.
- **Per-user activity** — submissions and pass/fail counts.
- **Top failure reasons** — grouped by `error_message`.
- **Best masks** — frequency of which `best_mask` wins across runs.
- **Resource usage** — average wall time per mask, queue wait time.

Powered by `JobsDAO.stats(...)`. Reads only — no actions. Useful for
the lab to see how the tool is used and to spot patterns
(e.g., "this mask always wins for instrument X, so set it as
default").

### Migration / schema evolution

`schema_version` table starts at 1. Future migrations add the next
version's `CREATE`/`ALTER` statements in `db.py.MIGRATIONS` and bump
the version on startup. No Alembic — straight SQL inside a
transaction is sufficient at this scale.

### Backup

A nightly `cp jobs.db jobs.db.bak.$(date +%F)` cron is enough — the
DB is rebuildable in part from session folders if lost, and any data
loss only affects monitoring history.

---

## Session layout & auto-purge

### One folder per UI session

Every UI submission gets its own session folder under
`webui.results_root` (the local mounted results directory). The
`job_id` (`<runId>__<utc_iso>__<shortuuid>`) doubles as the folder name
so it is sortable by submission time and unique even when the same
user re-submits the same run.

```
<results_root>/
  20260522_AV224503_5279_1__2026-05-26T09-14-02Z__a1b2c3/
    params.json                # the submitted form values
    state                      # queued | running | integrating | done | failed | cancelled
    run.log                    # full stdout+stderr of aviti_test_mask.sh
    qc_runs/                   # written by aviti_test_mask.sh
      mask_0_R1.Y12N._R2.Y12N./
        ...
      mask_1_.../
    mask_integration_summary.csv   # from integrate_mask_results.sh
    results.html                   # rendered final report
    .purge_lock                    # touched while files are written; blocks purge
```

`<jobs_dir>` from the earlier section *is the same path as*
`<results_root>` for the local stage — there's no reason to separate
job metadata from results in a single-user deployment. Treat them as
aliases. (Stage 2 could split them if multi-tenant, but that's out of
scope.)

### Why per-session, not per-run

Reusing the same folder for repeat submissions of the same run would
overwrite previous results — and the whole point of this tool is to
compare runs across parameter choices. Per-session folders keep every
attempt for later inspection.

### Purge policy

Run a purge sweep on **every** invocation of the UI (any HTTP request
to `/`, `/api/v1/runs`, or `/api/v1/jobs`). The sweep is cheap when
there is nothing to delete:

1. List subfolders of `<results_root>` matching the session-id pattern
   (`<runId>__<iso>__<uuid>`). Non-matching dirs are left alone — a
   user could have unrelated content there.
2. For each session folder:
   - Skip if `.purge_lock` is present (a job is actively writing).
   - Skip if `state ∈ {queued, running, integrating}` (active or
     about-to-be-active).
   - Read `mtime` of the `state` file (last status update) — that is
     the session's "age". Falls back to folder mtime if `state` is
     missing.
   - Mark for deletion if `now - mtime > retain_jobs_days`.
3. Sort marked-for-deletion folders newest-first. **Keep the newest
   `retain_jobs_min_keep` regardless of age** — so a long-idle host
   doesn't wake up to an empty results page.
4. Delete the remainder with `shutil.rmtree(..., ignore_errors=False)`.
   Any deletion error is logged but does not bubble up to the user —
   one stuck folder must not block all UI requests.

The sweep is wrapped in a single-process lock
(`<results_root>/.purge.lock`, `fcntl.flock`) so concurrent UI
requests don't race. If the lock is held, the second request silently
skips its sweep — once is enough.

### Why "on each request" rather than a background timer

- No daemon thread, no scheduler dependency, no `cron`. The UI is
  process-stateful enough as it is.
- Sweep cost is one `os.listdir` + one `os.stat` per session folder.
  At 30 days retention with one or two jobs per day, that's ~60 stats
  — sub-millisecond.
- Worst case: the UI is never opened, nothing gets purged. That's
  fine — the storage problem only exists if someone is actually using
  the tool.

### Manual controls

- `POST /api/v1/jobs/<id>/delete` — explicit delete of one session,
  regardless of age. Returns 409 if the session is active.
- `POST /api/v1/admin/purge?days=N` — force a sweep with a custom
  retention. Useful for one-off clean-ups; unguarded in Stage 1
  (localhost only), put behind basic-auth in Stage 2.

### Hard safety rails

- Refuse to operate if `webui.results_root` resolves to `/`, `$HOME`,
  or any path with fewer than 3 segments. A misconfigured root must
  never let the sweeper walk the wrong tree.
- Refuse to delete anything outside `realpath(results_root)` — guards
  against symlink escapes (a session folder containing a symlink to
  `/etc` should not delete `/etc`).
- `shutil.rmtree` with `onerror=` that logs but does not retry, to
  avoid loops on permission-denied edge cases.
- Dry-run mode: starting the server with `--dry-run-purge` logs what
  *would* be deleted on each sweep without touching anything. Use it
  on first deploy.

### Disk-pressure escape hatch

If `shutil.disk_usage(results_root).free < 5 GB` at the start of a
sweep, drop `retain_jobs_days` to `max(1, retain_jobs_days // 4)`
*for this sweep only* and continue. Surface a banner on the UI
("Storage pressure — purged aggressively"). Avoids the failure mode
where the retention setting is too generous for the available disk.

---

## Validation — the error-robust core

Every field the UI exposes must round-trip through the same validator on
both sides (browser JS + server Python). The server is authoritative;
client-side validation is UX only.

Validators per field (server-side, in `job_manager.validate_params`):

| Field | Rule |
|---|---|
| `run_id` | must exist in current `scan_nas_for_runs()` result |
| `input_dir` | resolves to a real path with `RunManifest.json` inside |
| `output_base` | inside `webui.results_root`; created if missing; writable |
| `threads` | 1–`os.cpu_count()` |
| `max_jobs` | 1–`os.cpu_count() // threads` (warn if oversubscribed) |
| `cache_input` | bool; if true, RAM stage size estimated ≤ free `/dev/shm` |
| `mem_limit_per_job` | parsed by humanfriendly; ≤ free RAM ÷ `max_jobs` |
| `docker_image` | regex `^[a-z0-9./_-]+(:[a-z0-9._-]+)?$`; pull-test optional |
| `tiles` | parsed by `resolve_tiles_spec()` (from plan_tile_selection) |
| `masks` | non-empty subset of `masks.yaml` entries |

Reject the whole payload with a structured error response (HTTP 400 + JSON
listing every field that failed). The form highlights each.

Resource pre-flight (mirrors `check_resources()` in the bash script): run
this *before* spawning the job and refuse with a clear message rather than
letting the bash script half-start and warn.

---

## The form (`static/index.html`)

Sections, top to bottom:

1. **Run picker** — searchable dropdown populated from `/api/v1/runs`.
   Shows instrument, run name, lanes, cycles, start date. Selection
   auto-fills `input_dir`.
2. **Tile selection** — radio group: `default (single tile)`,
   `all`, `lane:N` (with lane number dropdown), `spread:N`, `random:N`,
   `raw pattern`. Each shows a one-line explanation pulled from the
   plan_tile_selection grammar.
3. **Masks** — three mutually-exclusive input modes (radio-selected):
   - **(default) Pick from `masks.yaml`** — checkbox list from
     `/api/v1/masks`, all checked by default.
   - **Upload a custom mask list** — file upload (`.yaml` or `.txt`,
     ≤ 64 KB). Server validates each line with the same regex used
     for built-in masks (`^R1:[YN0-9*]+-R2:[YN0-9*]+$`) and rejects
     the whole file on any failure with a per-line error list.
   - **Type a single mask** — text input, same regex validation,
     applied as a one-mask suite (useful for quick smoke tests of a
     specific mask).

   Backend: `POST /api/v1/jobs` accepts a `masks_source` field
   (`builtin | uploaded | typed`) plus the corresponding payload
   (list of selected built-in names, uploaded file content, or the
   single string). Server normalises all three to the same internal
   list before invoking `aviti_test_mask.sh`. The script gains a
   `--masks-inline` flag taking a temp file path; the UI writes the
   normalised list to `<session>/masks.yaml` and passes
   `--masks-file <session>/masks.yaml`.
4. **Execution params** — collapsible advanced section with `threads`,
   `max_jobs`, `cache_input`, `mem_limit_per_job`, `docker_image`. Pre-filled
   from `/api/v1/config`. Each shows allowable range / pattern.
5. **Output** — `output_base` (defaulted to `<results_root>/<runId>_<timestamp>`).
6. **Launch button** + a live "resource estimate" line ("This will use ~32
   threads and ~64 GB at peak — host has 48 threads / 251 GB free").

On submit, the page transitions to a job-status view (still index.html, just
hides the form). On `done`, "View report" links to `/jobs/<id>/report`.

---

## `results.html` — what to render

Inputs available after a successful job:
- `mask_integration_summary.csv` from `integrate_mask_results.sh`
- per-mask `qc_runs/mask_*/` output directories (linked as raw artifacts)
- `params.json` (the submitted form values)
- `run.log` (full script output)

Layout:

1. **Header** — run name, instrument, mask count, total wall time,
   lanes tested + per-lane project labels (e.g. `L1: P12345 · L2: P67890`).
2. **Submitted parameters** — collapsed by default; includes the
   user-supplied `lane → project` map.
3. **Per-lane ranked tables** — one table per lane that was tested.
   The lab loads different projects on different lanes, so per-lane is
   the primary view. Each table titled with the lane's project label
   (`Lane 1 · P12345`) and sorted by `Score` (mask, %Assigned, Q30%,
   score, source, status). An additional "All lanes aggregate" table
   appears at the bottom for legacy comparisons.
4. **Per-mask cards** — one per mask, linking to the raw QC report.
5. **Log tail** — last 200 lines of `run.log`.
6. **Download** — links to per-lane CSVs + the aggregate CSV + a zip
   of `qc_runs/`.

Renderer is a single Jinja2 template (`results.html.j2`) so the same
code runs in Stage 1 and Stage 2.

### Per-lane project labels — schema impact

The submission form gains a small **Lanes → Project** table: one row
per lane in `RunParameters.Tiles`, with a free-text "project" field
the user fills in (typically a project number like `P12345` or a
short name). Defaults blank; not required, but strongly encouraged.

Persistence:
- The `jobs` table gains `lane_projects_json TEXT` (e.g.
  `{"1": "P12345", "2": "P67890"}`), defaulting to `'{}'`.
- The `mask_results` table gains a `project TEXT` column populated by
  the integrator from `lane_projects_json[lane]` — denormalised so
  the History/Monitor pages can filter/group by project without a
  join into JSON.

This lets users answer "which mask wins for project P12345 across all
its runs" with one indexed query.

---

## Error handling — what can go wrong, how each is surfaced

| Failure | Surface as | Recovery |
|---|---|---|
| NAS not mounted | red banner on form; `/api/v1/runs` returns `[]` + a `mount_status` field | user remounts; refresh |
| Selected run vanished mid-form | 400 on POST with `run_id: not found` | re-fetch runs |
| `RunManifest.json` malformed | run hidden from list with a console warning | upstream fix |
| Docker daemon down | 503 on `POST /jobs`, with `cause: docker_unreachable` | start docker; retry |
| `docker pull` fails | job state `failed`, log shows pull error, no partial outputs | check network |
| One mask fails (OOM, bad input) | job continues; per-mask card shows ✗ + last log lines; integrate runs on the rest | reduce `max_jobs` or set `mem_limit_per_job` |
| All masks fail | state `failed`; no report generated | params + log shown |
| Job cancelled | SIGTERM, then SIGKILL after 30 s; partial cleanup | restart |
| Disk full mid-run | bash script's existing failure path; UI shows "disk full" if log matches | free space |
| UI server crashes mid-job | job keeps running (it's a detached subprocess); on restart, scan `jobs_dir` to rehydrate state | none needed |

Crash-resistance principle: job state is **on disk**, not in memory. The
server can be killed and restarted without losing track of running jobs —
restart logic in `job_manager.rehydrate()` reads every `state` file and
re-attaches via PID.

---

## Stage 2 — Docker packaging

`webui/Dockerfile`:

```dockerfile
FROM python:3.11-slim
RUN apt-get update && apt-get install -y --no-install-recommends docker-cli && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt
COPY . .
EXPOSE 8765
CMD ["python", "-m", "flask", "--app", "app.py", "run", "--host", "0.0.0.0", "--port", "8765"]
```

`docker-compose.yml`:

```yaml
services:
  webui:
    build: ./webui
    ports: ["8765:8765"]
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock     # spawn sibling containers
      - /mnt/nas/sequencing:/mnt/nas/sequencing:ro    # NAS read-only
      - /data/analyses/aviti_test_mask:/data/analyses/aviti_test_mask  # results
      - ./config.yaml:/app/config.yaml:ro
      - ./masks.yaml:/app/masks.yaml:ro
      - ../:/repo:ro                                  # so it can call the shell scripts
    environment:
      - AVITI_REPO=/repo
```

The bash scripts run *inside* the webui container but `docker run …
bases2fastq` calls hit the host daemon through the mounted socket — that's
how the work containers see the host's NAS mount and CPU/RAM.

Security caveat: mounting `docker.sock` gives the container root-equivalent
on the host. Document this; in production, restrict the container to a
single host and don't expose its port off-LAN.

---

## Test plan

1. **Discovery unit tests** — `discovery.py` against a fixture tree with
   good runs, malformed runs, missing markers, dead symlinks. No NAS
   needed.
2. **Validator unit tests** — every field, valid + invalid.
3. **Job manager integration test** — fake `aviti_test_mask.sh` that sleeps
   + exits 0/1; verify state transitions, log capture, rehydrate after kill.
4. **End-to-end on chicken** — one real run from
   `/data/analyses/aviti_test_mask/20260522_AV224503_5279_1`, all masks,
   `spread:8`. Confirms Flask + shell + Docker chain works.
5. **Docker e2e** — same run via the containerized webui, NAS mounted
   read-only.

---

## Deferred TODOs (planned, not in v1)

### Auth / login layer in front of the UI

**Status:** deferred to a later iteration. Implement before exposing
the UI beyond a trusted LAN, or sooner if access needs to be
restricted within the LAN.

**Why deferred:** v1 ships to a trusted lab network behind chicken's
firewall; building auth alongside the rest of the UI would delay the
first usable version without changing what colleagues can do. The
multi-user concurrency design above already collects a `submitter`
field per submission, which gives non-binding accountability — enough
for v1.

**Scope of the future work:**

- Pick a mechanism — most likely options:
  - **Reverse-proxy basic auth** (nginx/Caddy in front of the Flask
    app, htpasswd file). Easiest; no Python code changes.
  - **Keycloak / VIB SSO** if institutional SSO is available — best
    long-term, more setup.
  - **Flask-Login + local user table** as a middle ground if SSO is
    not available and basic-auth is too crude.
- Replace the free-text `submitter` field with the authenticated
  user's identity (header, session, or token) — the concurrency
  queue's per-user accounting already uses an opaque string and
  will need no changes beyond the source of that string.
- Add an admin role gated for: `POST /api/v1/admin/purge`,
  `/jobs/all` visibility of other users' jobs, ability to cancel
  someone else's job.
- Add CSRF protection (already-needed once cookies carry auth) —
  Flask-WTF or a manual token. Currently the form is unauthenticated
  so CSRF is moot.
- Audit log: append-only file recording `(timestamp, user, action,
  job_id)` for submissions, cancellations, deletes.

**Acceptance criteria for the auth iteration:**

1. No unauthenticated request can reach `POST /api/v1/jobs` or any
   `/api/v1/admin/*` endpoint.
2. The submitter shown on a job is provably the authenticated user
   (cannot be spoofed by editing a form field).
3. A user can only see/cancel/delete their own jobs unless they have
   the admin role.
4. Existing session folders / job history remain accessible after
   the auth migration (no data loss).

This deferral is the single most important "remember to do this"
item in the plan — surface it on every project-status review until
done.

---

## Out of scope (for now)

- Run *queueing* across users beyond fair-share — `max_jobs_per_user`
  is simple LIFO-per-user, not a priority scheduler.
- Editing `masks.yaml` from the UI — read-only for now; users edit
  the file on disk. Avoids a class of validation bugs.
- WebSocket-based log streaming — SSE is enough and simpler.
- Mobile layout — desktop-only.

---

## Sequencing of work

Recommended PR order so each merge is independently useful:

1. `config.yaml` extension (NAS + webui keys) — backwards-compatible,
   ships alone.
2. `discovery.py` + CLI wrapper `bin/aviti_list_runs` — usable standalone,
   no UI yet.
3. `plan_tile_selection.md` implementation (separate plan) — UI needs it.
4. Flask app skeleton (`/health`, `/runs`, `/masks`, `/config`) + static
   form that just displays values. No launch yet.
5. `job_manager.py` + `POST /jobs` + log SSE. Form can now launch and tail.
6. `report_render.py` + `/report`. End-to-end flow on Stage 1.
7. Dockerfile + compose. Stage 2.

Each step ends with the system in a runnable state.

---

## Open questions

1. **NAS mount path** — what's the real path on chicken? `config.yaml` ships
   with a placeholder until confirmed.
2. **Sequencer subfolder layout** — is it `aviti_1/`, `aviti_N/`, or
   something else? Need a `ls /mnt/nas/sequencing` from chicken once mounted.
3. **Run-folder marker** — `RunManifest.json` is the assumption; confirm
   against actual run folders. If the marker differs per instrument, scan
   multiple candidates.
4. **Where does the UI run** — chicken localhost (port-forwarded over SSH,
   as we already do for 18080), or a different host? Stage 1 assumes
   chicken; we already have the `LocalForward` infrastructure.
5. **Auth** — confirm "trusted LAN, no auth" is acceptable for the
   initial Docker deploy. Otherwise add basic-auth before Stage 2 ships.
