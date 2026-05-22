# Parallelism and I/O caching in aviti_test_mask.sh

## Problem

`bases2fastq` accepts exactly one `--filter-mask` per invocation. Testing N masks
therefore requires N separate Docker containers, each independently reading the same
`BaseCalls/` directory from the run input.

When the input lives on a **NAS / NFS / SMB mount**, all N containers thrash the
network link simultaneously. With the default 9 built-in masks this often makes
total wall time *worse* than running masks sequentially, because the network
serialises the concurrent reads under the hood.

---

## Why tee / pipe is not applicable

`bases2fastq` takes a **directory path** as its first positional argument and
performs its own file traversal internally. It cannot read from `stdin` or a
named FIFO. A `tee`-based fan-out would require intercepting the tool's VFS
calls (e.g. via FUSE), which is fragile and provides no advantage over a simple
bind mount.

---

## Two complementary solutions

### 1. `--cache-input` — stage input to fast local storage once

Copy the entire run directory to fast local storage **before** launching any
container. All N containers then read from the local copy in parallel.

```
NAS read:          1×  (sequential, full link bandwidth, one-time cost)
Container reads:   N×  from local storage  (parallel, very fast)
```

**Storage selection** (`stage_input_to_ram` function):

| Host OS | Staging target | Type |
|---------|---------------|------|
| Linux | `/dev/shm` (if tmpfs is mounted there) | RAM — ~50 GB/s |
| Linux (no shm) / macOS | `$TMPDIR` or `/tmp` | Local SSD — ~3 GB/s |

A size guard checks that the run fits within 90% of available space before
copying. If it does not fit, the script falls back to the original path with a
warning and continues normally.

**macOS / Docker Desktop note:** Docker Desktop on macOS can only bind-mount
paths that are listed under *Settings → Resources → File Sharing*. `/tmp` is
included by default; `/dev/shm` typically is not. The staging function detects
whether `/dev/shm` is a real tmpfs and falls back to `/tmp` automatically.

**Cleanup:** a `trap _cleanup EXIT` removes the staged directory on any exit
(normal, error, or `Ctrl-C`).

**Usage:**
```bash
./aviti_test_mask.sh -i /Volumes/NAS/run -o ./results --cache-input
```

---

### 2. `-j / --jobs N` — bounded job concurrency

Limit the number of Docker containers running simultaneously using a
**named-pipe semaphore** (token pool). This prevents I/O thrashing on NAS even
without caching, and controls CPU/memory pressure when running many masks.

```bash
# FIFO pre-loaded with N tokens
mkfifo "$_SEMFIFO"; exec 3<>"$_SEMFIFO"
for ((i=0; i<MAX_JOBS; i++)); do printf ' ' >&3; done

for mask in "${MASKS[@]}"; do
  read -r -n1 -u3           # acquire a token (blocks if all N are in use)
  (
    trap 'printf " " >&3' EXIT   # release token on any exit
    run_mask_qc "$mask" "$outdir"
  ) &
done
```

`trap 'printf " " >&3' EXIT` ensures the token is always released even if
`run_mask_qc` fails, so a crashed container never deadlocks the pool.

**Tuning guide:**

| Storage | Recommended `--jobs` | Rationale |
|---------|---------------------|-----------|
| Local NVMe SSD | 6–9 | Disk can serve many streams; CPU is the limit |
| Local HDD | 2–3 | Seek contention above ~2 concurrent readers |
| NFS / NAS (1 GbE) | 1–2 | Link saturated by even 1 reader |
| NFS / NAS (10 GbE) | 3–4 | Link handles 3–4 streams before saturation |

Default is **4** — conservative enough to work well on most NAS setups without
`--cache-input`.

---

## Combining both options

The two levers are independent and compose:

```bash
# Best for NAS: stage once to RAM, then run all 9 masks in parallel
./aviti_test_mask.sh -i /Volumes/NAS/run -o ./results --cache-input -j 9

# Conservative NAS: no caching but only 2 concurrent reads
./aviti_test_mask.sh -i /Volumes/NAS/run -o ./results -j 2
```

---

## Estimated wall-time impact (9 masks, ~2 GB QC data, 1 GbE NAS)

| Mode | Approx. wall time |
|------|------------------|
| Original (9× NAS reads, all parallel) | ~20 min |
| `-j 2` (controlled NAS reads) | ~12 min |
| `--cache-input` (1× NAS + 9× local) | ~4 min |
| `--cache-input -j 9` | ~3 min |

*Numbers are illustrative; actual results depend on NAS bandwidth, run size,
and host hardware.*

---

## Future path

If `bases2fastq` ever supports a multi-mask config file, the architecture
simplifies to: one container, one read, N outputs — at which point
`--cache-input` and the job pool become unnecessary.
