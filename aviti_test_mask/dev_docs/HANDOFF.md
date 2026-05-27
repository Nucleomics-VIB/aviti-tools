# Handoff — 2026-05-27 (afternoon, refresh 3)

State at session checkpoint. Pick up here next session.

## What's running

Nothing. Smoke-test container `aviti_smoke` stopped + removed, dev
server not running, no bash orphans, no queued/running jobs.

```bash
docker ps --filter 'label=aviti_job_id'   # empty
docker ps --filter name=aviti             # empty
ps -ef | grep aviti_test_mask.sh          # empty
lsof -ti :8765                             # empty
```

The image `aviti_test_mask_webui:latest` (684 MB, arm64) is left in
the local Docker — see Docker smoke-test below. Safe to `docker rmi`.

## Repo state

Branch `develop`, up to date with `origin/develop`. Working tree has
**one uncommitted change**: `.gitignore` adds `docker/config/` (the
operator-staged config dir the Docker README tells you to create —
not a build artefact). Commit when convenient.

Today's commits since yesterday's handoff (newest first), all already
pushed:

- `e4b0237` — reserve backup config slot in webui_config.yaml
- `265db1b` — designs for auth, backup, runtime; build-vs-deploy
- `efb244a` — `dev_server.sh` mac-dev launcher
- `cbe049a` — lifecycle integration test
- `24a6bcc` — handoff refresh
- `48ae7a6` — extract DockerClient (worker 484 → 442 LOC)
- `8714828` — extract submit_job service; pages.submit_post 80 → 30 LOC
- `2e89358` — explicit job state machine + DAO enforcement + HTTP 409
- `b65d6b3` — order fix: `--exclude-tile` before `--include-tile`
- `42840ba` — extract pipeline_invocation seam; fixes tile-restriction

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
5. **Dockerization stage 2** — *throwaway smoke test done
   2026-05-27.* Image builds (684 MB arm64, all 12 steps clean), app
   boots, `/api/v1/health` returns HTTP 200, `/` renders the Submit
   page (20 KB HTML, correct title, "No runs"). Confirms the
   Dockerfile, conda-env-on-Ubuntu, entrypoint, tini, and Flask boot
   are all sound. **Findings worth keeping:**
   - **Use `docker build` (buildkit), not `docker compose build`
     (legacy builder), on Mac.** Legacy builder hangs at Step 9
     (`useradd`) when `APP_UID` is huge (Mac AD-LDAP `id -u` returns
     ~1.3M, which makes `useradd` write a multi-GB sparse
     `lastlog`/`faillog` that breaks the legacy builder's overlayfs
     commit). Buildkit handles it. Non-issue on Ubuntu where APP_UID
     is small.
   - **`APP_UID=1000` is mandatory at build time on Mac.** Same
     sparse-log problem hits container *creation*, not just build —
     image baked with the giant UID can't be extracted into the
     overlay. On Ubuntu set APP_UID to the host aviti user's UID.
   - **`docker/config/` must live in a Colima-mounted host path.**
     Default `./config` next to `docker-compose.yml` is at a project
     path Colima doesn't mount, so the bind-mount silently shows up
     empty inside the container → Flask dies with
     `FileNotFoundError: /app/webui/config/webui_config.yaml`.
     Non-issue on Ubuntu. For Mac smoke, set `CONFIG_DIR` to
     something under `~/...` or `/Volumes/lvs`.
   - **SQLite WAL on sshfs is the known Mac/Colima pathology.**
     Bind-mounted `db_path` failed with `unable to open database
     file` even after `chmod 777`. Won't be the issue on Ubuntu
     where the results dir is native ext4. For Mac smoke, point
     `db_path` at a container-local path (`/tmp/aviti/results/…`,
     deep enough to pass the `results_root` shallow-path guard in
     `config_loader`).
   - **Two `degraded` markers in `/health` are environmental, not
     bugs.** `nas_check` fails when `/data/nas` isn't bound;
     `docker.ok` fails when `DOCKER_GID` doesn't match the Colima
     socket's gid. Both resolve naturally on Ubuntu.

   **Still to verify on prod Ubuntu (smoke can't catch on Mac):**
   docker-out-of-docker actually launching a bases2fastq sibling
   container; path identity for `/data/results/<job>` resolving
   correctly on the host daemon; SQLite WAL persistence across
   restart on bind-mounted ext4; healthcheck flipping to `healthy`.

   **Local image left for inspection** — `docker rmi
   aviti_test_mask_webui:latest` when done.
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

**Canonical spec:**
`~/.claude/skills/webapp_template/AUTH_MODULE.md` — the user updated
the `webapp_template` skill on 2026-05-27 to include this section,
distilled from FreezerManager + ProjectManager + PacBioPricing. It
supersedes the bullet list that lived here before. **Read it first;
the items below only record where aviti will *depart* from it.**

The spec already prescribes everything the earlier handoff listed
(USERS schema, two-role CHECK, `MustChangePassword`, reset-token
columns, Flask-Mail + Gmail app-password, `@login_required` /
`@admin_required`, anti-enumeration on `/forgot-password`, route
gating, customisation checklist) *plus* several things the handoff
missed and we should adopt:

- **Flask-Session filesystem backend** (8h lifetime, HttpOnly,
  SameSite=Lax, Secure when behind HTTPS) — handoff didn't specify
  session machinery.
- **Stable `SECRET_KEY` fallback chain**: env → config →
  `data/.secret_key` file → `secrets.token_hex(32)`. Dev-server
  reloads don't log everyone out.
- **`_cleanup_old_sessions()` helper** invoked on each login —
  prevents `flask_session/` growing unbounded. Load-bearing.
- **`secrets.token_urlsafe(32)` reset token, 1-hour expiry,
  always-success on `/forgot-password`** — anti-enumeration.
- **Factor `app/email.py` out of `app/auth.py`** — auth code calls
  named senders (`send_password_reset_email`, etc.); SMTP details
  live in one place; swapping providers is a config change. Also
  documents the constraint we already hit: VIB/KU Leuven SMTP
  gateway won't authenticate a containerised app → masquerade
  through Gmail (matches what the handoff planned).
- **Cookie `Secure` flag** controllable via env / config — set when
  behind HTTPS in Portainer.

**Where aviti will *depart* from the spec (challenge points):**

- **Don't introduce a separate `config.yaml` with `default_users`
  inside it.** Spec assumes one big config; aviti already splits
  `webui/config/webui_config.yaml` (operator-tunable runtime) from
  `webui/config/users.yaml` (identity list, gitignore-safe). Keep
  the split — feed the seed loop from `users.yaml`, not from a
  field inside `webui_config.yaml`.
- **Don't add `scripts/init_database.py` and don't add a separate
  `ensure_schema_current()` module.** Aviti already initialises
  the schema on `JobsDAO(path).__init__` via `_init_schema()` (see
  `webui/services/db.py:186-200`) with `CREATE TABLE IF NOT
  EXISTS` + a `schema_version` row. **Fold the USERS table into
  `SCHEMA_SQL` and the additive reset-token migration into
  `_init_schema()`.** Fewer moving parts than the spec's
  "two-script" pattern; same idempotent guarantee.
- **`flask_session/` in `/tmp` (tmpfs), not a bind mount.** Compose
  already declares `tmpfs: /tmp:size=128m`. Sessions become
  ephemeral on container restart — fine for an internal tool,
  zero new bind mount, zero growth risk, no Mac-specific path.
  Skill defaults to `BASE_DIR / flask_session` which would
  silently land in the image's writable layers — worse.
- **Per-user concurrency stays in `submit_job` service** (`8714828`)
  — count user's (queued ∪ running) jobs, reject with HTTP 409 via
  the FSM errorhandler from `2e89358`. Admins bypass. (Spec doesn't
  cover this; it's aviti-specific.)
- **No "live config reload" admin route in this pass.** Out of
  scope for auth itself; treat as a separate item if prod tuning
  proves it necessary.

**Still open (flag for next architecture pass, post-auth):** total
thread-budget vs per-container limits. Current config models
`max_global_containers × max_inner_jobs × threads`; user's
intuition is a *total* budget (e.g. 24 threads shared). Decide
after Auth lands and we have real prod load numbers.

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

## Skills the next session should know about

- **`webapp_template`** (updated 2026-05-27) — now bundles
  `AUTH_MODULE.md` with the FreezerManager-derived session-auth
  spec. Read it before building Auth (item 3). Departure points
  listed inline in the Auth section above.
- **`portainer_docker_deployment`** — step-by-step `export →
  upload → deploy` workflow for shipping the locally-built image
  to a Portainer host without a registry. Relevant for stage 3 of
  Dockerization, after Auth lands.
