# test run on a random run taken from our NAS

I tried to create a test tool to evaluate several masks and find the one giving the best compromize between read number and quality

Note that I have no clue if the results are correct, I leave this to you experts

The Docker calls in `aviti_test_mask.sh` run with your host UID/GID (`--user $(id -u):$(id -g)`), so generated files in the output directory are owned by your current user instead of `root`.

## Using a NAS / LAN mount as input

When `-i` points to a network-mounted path (NFS, SMB/CIFS — e.g. `/Volumes/NAS/...`), Docker Desktop on macOS must be told to share that path with its VM, otherwise the bind mount is silently empty and `bases2fastq` will fail.

**One-time setup in Docker Desktop:**

1. Open *Settings → Resources → File Sharing*
2. Add the mount point (e.g. `/Volumes/NAS`) or the full run path
3. Click *Apply & Restart*

The script detects network mounts automatically and runs a quick verification before starting the QC jobs. If Docker cannot read the input it exits immediately with a clear error message.

Current script behavior:

- Runs `bases2fastq` QC on the full run (no single-tile mode).
- Uses `BaseCalls` content as-is (no pre-unzip step in the script).
- Supports `-p/--threads` to set `bases2fastq -p` (default: `24`).

## run QC with several arbitrary masks

Note that more can be added to the array on top of the script very easily

```bash
./aviti_test_mask.sh -i $PWD/20250107_AV224503_4917_1 -o $PWD/test2
```

With explicit thread count:

```bash
./aviti_test_mask.sh -i $PWD/20250107_AV224503_4917_1 -o $PWD/test2 -p 24
```

## result integration

```bash
./integrate_mask_results.sh -o test2
```

Results of this test run

```text
## MASK DIAGNOSTIC
Mask                                                %Assigned       Q30%      Score        Src
R1_Y18N_-R2_Y18N_                                      98.131     97.408  95.587444        log
R1_Y15N_-R2_Y15N_                                      98.096     97.089  95.240425        log
R1_Y12N_-R2_Y12N_                                      98.041     96.693  94.798784        log
R1_Y10N_-R2_Y10N_                                      97.979     96.309  94.362595        log
R1_Y15N_-R2_N_                                         97.991     96.220  94.286940        log
R1_N_-R2_N_                                            95.899     90.688  86.968885        log

🎯 RECOMMEND: R1_Y18N_-R2_Y18N_ (Score: 95.587444)

## RUNSTATS JSON
Mask                                                   RunPF%    RunQ30%
R1_Y18N_-R2_Y18N_                                      98.131     97.408
R1_Y15N_-R2_Y15N_                                      98.096     97.089
R1_Y12N_-R2_Y12N_                                      98.041     96.693
R1_Y10N_-R2_Y10N_                                      97.979     96.309
R1_Y15N_-R2_N_                                         97.991     96.220
R1_N_-R2_N_                                            95.899     90.688
Saved summary: /data/analyses/aviti_test_mask/test2/mask_integration_summary.csv
```
