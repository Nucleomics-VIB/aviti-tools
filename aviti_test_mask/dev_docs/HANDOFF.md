# Handoff — 2026-05-26 end of day

State at session end. Pick up here next session.

## What's running

Nothing. Server stopped, no docker containers, no bash orphans, no
queued/running jobs.

```bash
docker ps --filter 'label=aviti_job_id'   # empty
ps -ef | grep aviti_test_mask.sh          # empty
lsof -ti :8765                             # empty
```

## Repo state

Branch `develop`, all work pushed to `origin/develop`. Latest commit:
**`87d5961` — `fix(webui): anchor --include-tile patterns …`**.

Today's commits (newest first):

- `87d5961` — anchor `--include-tile` patterns *(this turned out to be the wrong fix — see "Open issue" below)*
- `7a739e2` — docker/ definition files (Dockerfile, compose, entrypoint, .dockerignore, prod config, README) — no build/run
- `62a415d` — rich per-mask Results page (Chart.js, per-mask cards, embedded reports)
- `6249685` — reattach reserves concurrency slot (was the cause of two concurrent jobs)
- `bb35cdb` — Monitor page
- `a9aa6a7` — clarify "default" tile mode label
- `72ec663` — default tile mode honours form lane filter
- `d9b031b` — show actual tiles in queue + 📑 Full Report icon
- `d2243d2` — runs-per-page default 15
- `3e8c86c` — worker reattach to surviving containers on restart
- `596bb88` — History + Results + Settings pages + integrator CSV → mask_results

## Tests

49 passing (`cd webui && python -m pytest tests/ -q`).

## Open issue (highest priority for tomorrow)

**bases2fastq `--include-tile` does NOT restrict by itself.** Element's
docs ([Sequencing Optional Arguments](https://docs.elembio.io/docs/bases2fastq/optional-arguments/),
[Example Commands](https://docs.elembio.io/docs/bases2fastq/example-commands/))
explicitly say: *"To include specific tiles, you must exclude all
tiles with the `--exclude-tile` argument"*.

So the correct invocation is:

```bash
bases2fastq /input /output --qc-only --filter-mask R1:Y18N*-R2:Y18N* \
  --exclude-tile 'L.R..C..S.' \
  --include-tile L1R09C01S1
```

Today we tried anchored regex (`^L1R09C01S1$`) which did not help —
neighbour columns C02, C03 kept being processed. The user will test
the `--exclude-tile + --include-tile` invocation directly against
bases2fastq tomorrow before we wire it.

When confirmed:

1. **`scripts/aviti_test_mask.sh`** — when `--include-tile` is provided,
   also pass `--exclude-tile 'L.R..C..S.'`. Add a new CLI flag
   `--exclude-tile` for symmetry or just hardcode the catch-all.
2. **`webui/services/discovery/tiles.py`** — drop the `^…$` anchors
   from `_anchored()`. They're irrelevant once `--exclude-tile` is in
   place; with anchors the regex matches no tile and the fallback set
   kicks in.
3. **`webui/tests/test_discovery.py`** — remove the `^…$` assertions
   added in `87d5961`. Expect plain `"L1R09C01S1"` etc.

## Other pending todos

1. **Lifecycle integration test** — boot Flask, queue a fake job
   (with a stub script that just writes a few `✅` lines and exits),
   verify state transitions queue → running → done. This is the
   class of test that catches operational bugs unit tests miss (e.g.
   the reattach slot leak fixed in `6249685` would have been
   detected).
2. **Auto-purge** — `retain_jobs_days` config exists but no scheduled
   cleanup runs. Sessions accumulate on disk.
3. **Auth layer** — deferred per `plan_webui.md`. Anyone with the URL
   can submit.
4. **Per-lane project routing on submit** — DB tracks
   `lane_projects_json`, but the submit form doesn't let you assign
   projects per-lane.
5. **Dockerization** — Stage 2: actually `docker compose build` and
   smoke-test the image. Definition files are ready (`7a739e2`).

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
