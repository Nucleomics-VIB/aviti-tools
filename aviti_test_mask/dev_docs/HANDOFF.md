# Handoff — 2026-05-27 (mid-day, refresh 2)

State at session checkpoint. Pick up here next session.

## What's running

Nothing. Server stopped, no docker containers, no bash orphans, no
queued/running jobs.

```bash
docker ps --filter 'label=aviti_job_id'   # empty
ps -ef | grep aviti_test_mask.sh          # empty
lsof -ti :8765                             # empty
```

## Repo state

Branch `develop`, **5 commits ahead of `origin/develop`** (not yet
pushed). Latest commit: **`48ae7a6` — `refactor(webui): extract
DockerClient from job_worker`**.

Today's commits since yesterday's handoff (newest first):

- `48ae7a6` — extract DockerClient (4 inline docker calls → 1 module);
  worker 484 → 442 LOC
- `8714828` — extract submit_job service; pages.submit_post 80→30 LOC
- `2e89358` — explicit job state machine (job_lifecycle.py) + DAO
  enforcement + HTTP 409 errorhandler
- `b65d6b3` — order fix: `--exclude-tile` before `--include-tile` in
  the worker→script CLI
- `42840ba` — extract pipeline_invocation seam (python↔bash contract);
  fixes tile-restriction bug by always pairing `--include-tile` with
  `--exclude-tile 'L.R..C..S.'`

## Tests

**128 passing** (`cd webui && pytest tests/ -q`), up from 49 at the
start of the day.

- `test_discovery.py` (22) — scan, validation, tile resolution
- `test_db.py` (7) — DAO CRUD
- `test_masks_loader.py` (3) — masks YAML loader
- `test_persist_mask_results.py` (2) — integrator CSV persistence
- `test_results_endpoints.py` (15) — results API
- `test_pipeline_invocation.py` (27) — python↔bash contract
- `test_job_lifecycle.py` (20) — state machine + DAO rejection
- `test_job_submission.py` (9) — submit service
- `test_docker_client.py` (20) — docker CLI façade
- `test_lifecycle_integration.py` (3) — end-to-end JobWorker drive
  against stub bash scripts (happy path, script failure, preflight)

## Architecture pass — outcome

Driven by `/improve-codebase-architecture` review. Of six candidates:

- ✅ **Candidate 2** (PipelineInvocation seam) — `42840ba` + `b65d6b3`
- ✅ **Candidate 3** (Explicit JobLifecycle FSM) — `2e89358`
- ✅ **Candidate 5** (Extract submit_job service) — `8714828`
- ⚠️  **Candidate 1** (Split JobWorker) — *partial.* DockerClient
  extracted (`48ae7a6`). Worker still 442 LOC; remaining concerns
  (SlotPool, ScriptRunner, Reattacher, IntegratorRunner) could each
  become their own module but the payoff is now diminishing — the
  worst coupling is gone.
- ❌ **Candidate 4** (Fuse discovery/ into RunDescriptor) — *skipped
  after re-read.* The 4 discovery modules serve distinct request
  lifecycles (scan on every page, validation lazily, metadata
  once-then-cached, tiles at submit), so they don't actually re-parse
  the same JSON in one request. Consolidation would force callers to
  pay for unwanted work. Honest deletion-test says no.
- 🤔 **Candidate 6** (Drop stored queue_position) — *speculative.*
  Leave alone unless the drift bites.

Tile-restriction bug from yesterday's handoff is **fixed**
(`42840ba` + `b65d6b3`). Pairing rule enforced by
`pipeline_invocation.build_script_command()` and pinned by a test
asserting `exc_idx < inc_idx`.

## Next session — architecture (optional)

**1a. Continue JobWorker decomposition** if the file still feels heavy:
- SlotPool — `_active` dict + `_lock` + max_global_containers /
  max_jobs_per_user accounting (~30 LOC, used by `_try_launch_next`
  and the reattach path).
- ScriptRunner — `_launch` + Popen lifecycle (~70 LOC).
- Reattacher — `_reap_stale_on_startup` + `_reattach_thread` (~60 LOC).
- IntegratorRunner — `_run_integrator` + `_persist_mask_results` (~80
  LOC).

Verdict to revisit: the worker is no longer a god object. Each
remaining method does one thing. Further splitting probably crosses
into "moving code for the sake of moving code" — only do it if the
file *reads* hard, not because LOC is over some threshold.

## Other pending todos (carried from yesterday)

1. ✅ **Lifecycle integration test** — done in
   `tests/test_lifecycle_integration.py` (3 tests, +3 = 128 total).
   Drives `JobWorker` end-to-end against stub bash scripts: happy
   path (queued → running → integrating → done with `best_mask`
   bubbling up from integrator CSV), script-failure path (rc=7 →
   failed with the error line surfaced), preflight failure
   (`run_path` missing → queued → failed, no script ever spawned).
   `POLL_INTERVAL_SECONDS` is monkeypatched to 0.05 s so all three
   complete in under a second. Catches the slot-leak class of bug
   (legal transitions in the wrong order) that the DAO FSM can't see.
2. **Auto-purge** — `retain_jobs_days` config exists but no scheduled
   cleanup runs. Sessions accumulate on disk.
3. **Auth layer** — *designed; ready to build.* See "Feature
   designs → Auth" below. Build BEFORE dockerization stage 2 (item 5).
4. **Per-lane project routing on submit** — DB tracks
   `lane_projects_json`, but the submit form doesn't let you assign
   projects per-lane.
5. **Dockerization stage 2** — `7a739e2` shipped definition files;
   still need to `docker compose build` and smoke-test the image.
   **Recommendation: build + smoke-test NOW (throwaway image), but
   do NOT deploy to Portainer until Auth (item 3) lands.** Building
   now finds bugs in the Dockerfile, conda-on-Linux, DooD socket,
   bind-mount path-identity, APP_UID/GID, `tini`, and healthcheck
   while the rest of the code is calm (FSM/services just refactored,
   128 tests green). Smoke test = `docker compose up`, hit
   `/api/v1/health`, list runs from `/`, queue a no-op job. Develop
   Auth in the local dev loop (`./dev_server.sh --restart` cycles in
   seconds vs minutes per `docker compose build`). Rebuild and
   deploy to Portainer only once Auth is in.
6. **Push to origin** — 5 commits are local-only.
7. **DB backup loop** — *designed; ready to build after Auth.* See
   "Feature designs → DB backup loop" below.
8. **Total runtime in job results** — *designed.* See "Feature
   designs → Runtime tracking" below. Coordinate with the
   JobLifecycle FSM (`2e89358`) — its state transitions are where
   timestamps want to land.
9. **`dev_server.sh`** — *untracked.* Mac-dev launcher with
   colima-mount fixes, `--restart` (kills webui on port 8765 +
   `pgrep -f aviti_test_mask.sh` workers + `docker ps --filter
   label=aviti_job_id` containers + clears pycache), `--wipe`
   (destroys DB and results, prompted). Verified still suited after
   today's refactor — it operates at OS level (labels, script names,
   port, paths) which are unchanged. Commit when convenient.

## Feature designs (carried)

Detailed designs for the items above. Status fields here track build
state; question history lives in git log.

### Auth

- **Pattern source:** copy verbatim from FreezerManager
  (`/Users/u0002316/Documents/GitHub/Nucleomics-VIB/WebTools/dev_wt_FreezerManager/`):
  `app/auth.py`, `app/routes_blueprints/auth_routes.py`,
  `app/email.py`, USERS schema in `scripts/init_database.py`,
  templates `login/forgot_password/reset_password/change_password/users.html`.
- **Schema:** USERS table with `CHECK(Role IN ('admin','user'))`,
  `MustChangePassword`, `reset_token`, `reset_token_expires`. Two
  roles only.
- **Seed source:** existing `webui/config/users.yaml` (versioned,
  no secrets). New idempotent script (TBD location — coordinate with
  architecture) seeds DB on container start; default password
  `changeme`; first login forces change.
- **Secrets:** `SECRET_KEY` in gitignored file → Portainer env var.
  No additional site-wide bearer token (user confirmed: per-user
  bcrypt IS the access control).
- **Email:** Flask-Mail + Gmail SMTP (per-app password). Creds via
  `MAIL_USERNAME` / `MAIL_PASSWORD` env vars (gitignored
  `scripts/run_with_mail.sh` in dev, Portainer in prod). SMTP host /
  port / TLS in `webui_config.yaml`. **Graceful degradation:** login
  hides "Forgot?" when `MAIL_USERNAME` unset; same code in dev and
  prod.
- **Route gating:** every page `@login_required` (admin pages
  `@admin_required`). Anonymous only: `/login`, `/forgot-password`,
  `/reset-password/<token>`, `/api/v1/health`, static. `/results/<id>`
  is gated — sharing means adding the recipient to `users.yaml`.
- **Per-user concurrency:** enforce `max_jobs_per_user` in the
  `submit_job` service (`8714828`) — count user's jobs in
  (queued ∪ running), reject with **409** when over (reuses the FSM
  errorhandler from `2e89358`). Admins bypass.
- **Live config reload:** admin-only `/admin/reload-config` (or
  SIGHUP) re-reads `webui_config.yaml`. Rationale: Mac dev is too
  weak to benchmark; prod tuning needs queue-observe-adjust-requeue
  without losing running jobs.
- **Open architectural question (flag for next architecture pass):**
  user's intuition is a *total thread budget* (e.g. 24) shared
  across users. Current config models per-container limits
  (`max_global_containers × max_inner_jobs × threads`). Decide
  whether to evolve to a global-budget model or keep the factored
  form.

### DB backup loop

- **Module:** one deep `backup` module (location TBD — likely
  alongside the services already extracted). Interface:
  `start_backup_loop(config) -> None` called once at app startup.
  No `BackupStrategy` abstraction (one adapter = hypothetical seam).
- **Trigger:** background thread inside the Flask process,
  **mtime-based.** On start, finds newest `jobs-*.db` in
  `backup_dir`, sleeps until `mtime + interval`, runs, loops.
  Filesystem IS the schedule state → restarts self-heal. No
  APScheduler, no in-image cron (would break `tini → exec Flask`),
  no sidecar.
- **Method:** `sqlite3.Connection.backup()` — online-safe with
  active writers; produces a guaranteed-valid file.
- **Destination:** separate bind mount. Add to `docker-compose.yml`:
  `${BACKUP_ROOT:-/data/backups}:/data/backups`. **Requirement:**
  `BACKUP_ROOT` MUST be a different host path than `RESULTS_ROOT`
  (otherwise `--wipe` / `rm -rf` takes both). Different physical
  disk is nice-to-have, not required (off-host DR is out of scope).
- **Filenames:** `jobs-YYYYMMDDTHHMMSSZ.db` (UTC, basic ISO-8601 →
  `ls` sorts chronologically) + `jobs-...db.sha256` sidecar.
- **Retention:** keep last `backup_retain_count` files; default 8
  (~2 months weekly). Prune after each successful write.
- **Restore:** manual operator task (no UI button). Stop container →
  copy last good `jobs-*.db` over `jobs.db` → restart.
- **Config additions:** *already in `webui/config/webui_config.yaml`
  as of the config-slot commit* — keys `backup_enabled`,
  `backup_interval_days`, `backup_retain_count`, `backup_dir`. Mac
  dev points at `../../backups` (gitignored); Ubuntu prod overrides
  to e.g. `/data/backups` on a different bind mount. Docker
  deployment will need a third bind-mount in `docker-compose.yml`
  mapping `${BACKUP_ROOT:-/data/backups}:/data/backups`.
- **Bonus:** when SMTP is configured, send a success/failure email
  to admins after each backup (FreezerManager has
  `send_backup_confirmation_email` ready to copy).

### Runtime tracking

- **Goal:** record real runtimes so operator can tune concurrency on
  the prod host (Mac dev too weak to benchmark).
- **Three clocks, all stored as int seconds:**
  - `wall_clock_s` — submit → done (queue + compute)
  - `worker_runtime_s` — bash script start → bash script exit
  - `container_runtime_s` — bases2fastq container start → exit
- **Schema:** add columns to JOBS table. Capture at state-machine
  transitions (`2e89358`): submitted_at, started_at, finished_at as
  TIMESTAMP; the `_s` fields can be derived in the DAO or stored
  alongside (decide with architecture agent based on what the FSM
  already records).
- **UI:** render `H:MM:SS` on Results page, History, Monitor.
- **Coordinate:** the JobLifecycle FSM is the natural seam for
  timestamping — do NOT scatter `time.time()` calls across the
  worker. Add one method to the FSM that stamps the current state
  on transition.

## Mac dev environment

- conda env: `aviti_test_mask_webui`
  (`/opt/miniconda3/envs/aviti_test_mask_webui/bin/python`)
- NAS share: `/Volumes/lvs/GBW-0047_NUC_Transfers/0003_Runs/Aviti`
  (must be mounted; webui banner warns when not)
- Results dir: `<project>/results/`
- Colima mounts (must persist — set via `colima start --mount`):
  - `/Volumes/lvs:r`
  - `/Users/u0002316/Documents/GitHub/Nucleomics-VIB/aviti-tools/aviti_test_mask/results:w`
- Concurrency: `max_global_containers=1`, `max_inner_jobs=1`,
  `threads=4` (Mac-dev profile in `webui/config/webui_config.yaml`)

## Restart procedure

```bash
cd /Users/u0002316/Documents/GitHub/Nucleomics-VIB/aviti-tools/aviti_test_mask/webui
/opt/miniconda3/envs/aviti_test_mask_webui/bin/python app.py
# UI at http://127.0.0.1:8765
```

## Leftover session folder

`results/20260522_AV224503_5267_2__2026-05-26T19-08-49Z__406d7d` —
incomplete (got to mask 1 mid-tile-processing then the container was
stopped). Safe to delete or leave for inspection.

## Architecture review HTML report

Last generated: `/var/folders/9t/3znyqdv97hd88564_qzqt39d78gyqt/T/architecture-review-20260527-121301.html`
(temp; macOS may purge it). Re-run `/improve-codebase-architecture`
when the next set of friction points emerges.
