## test run on a random run taken from our NAS

### run QC with several arbitrary masks (more can be added) 
./aviti_test_mask.sh -i $PWD/20250107_AV224503_4917_1 -o $PWD/test2

### result integration
./integrate_mask_results.sh -o test2


```
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