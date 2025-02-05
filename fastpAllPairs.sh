#!/bin/bash

# script: fastpAllPairs.sh
# using a manual list of all read#1 in sample order
# apply fastp to R1 and R2 files from each lane and each run in the current folder
# run in the parent folder containing the run merged run folders
# save the filtered pair in a folder named by the sample for future merging
#
# SP@NC; version 1.0;  2025-01-31

# run conda or die
myenv="/home/luna.kuleuven.be/u0002316/conda_envs/fastp"
source /opt/miniconda3/etc/profile.d/conda.sh
conda activate ${myenv} || \
  ( echo "# the conda environment ${myenv} was not found on this machine" ;
    echo "# please read the top part of the script!" \
    && exit 1 )

# Store the sorted unique read#1 list in a heredoc
read -r -d '' r1_list << EOF
4920_01_A1_20487881F4509f06A1_S5_L001_R1_001.fastq.gz
4920_01_A1_20487881F4509f06A1_S5_L002_R1_001.fastq.gz
4920_02_A2_20487881F4517f05A2_S6_L001_R1_001.fastq.gz
4920_02_A2_20487881F4517f05A2_S6_L002_R1_001.fastq.gz
4920_03_S1_A1_20487881F4509f10S1A1_S7_L001_R1_001.fastq.gz
4920_03_S1_A1_20487881F4509f10S1A1_S7_L002_R1_001.fastq.gz
4920_04_S2_A1_20487881F4509f08S2A1_S8_L001_R1_001.fastq.gz
4920_04_S2_A1_20487881F4509f08S2A1_S8_L002_R1_001.fastq.gz
4920_05_S3_A2_20487881F4517f09S3A2_S9_L001_R1_001.fastq.gz
4920_05_S3_A2_20487881F4517f09S3A2_S9_L002_R1_001.fastq.gz
4920_06_S4_A2_20487881F4517f02S4A2_S10_L001_R1_001.fastq.gz
4920_06_S4_A2_20487881F4517f02S4A2_S10_L002_R1_001.fastq.gz
4920_07_R1_20487881F2516f04R1_S11_L001_R1_001.fastq.gz
4920_07_R1_20487881F2516f04R1_S11_L002_R1_001.fastq.gz
4920_08_R3_20487881F2513f04R3_S12_L001_R1_001.fastq.gz
4920_08_R3_20487881F2513f04R3_S12_L002_R1_001.fastq.gz
4920_09_R4_20487881F2509f06R4_S13_L001_R1_001.fastq.gz
4920_09_R4_20487881F2509f06R4_S13_L002_R1_001.fastq.gz
4920_10_R5_20487881F3521f05R5_S14_L001_R1_001.fastq.gz
4920_10_R5_20487881F3521f05R5_S14_L002_R1_001.fastq.gz
4920_11_R6_20487881F2507f06R6_S15_L001_R1_001.fastq.gz
4920_11_R6_20487881F2507f06R6_S15_L002_R1_001.fastq.gz
4920_12_R7_20487881F4521f02R7_S16_L001_R1_001.fastq.gz
4920_12_R7_20487881F4521f02R7_S16_L002_R1_001.fastq.gz
4920_13_R8_20487881F1517f07R8_S17_L001_R1_001.fastq.gz
4920_13_R8_20487881F1517f07R8_S17_L002_R1_001.fastq.gz
4920_14_R9_20487881F1521f11R9_S18_L001_R1_001.fastq.gz
4920_14_R9_20487881F1521f11R9_S18_L002_R1_001.fastq.gz
4920_15_R10_20487881F3502f05R10_S19_L001_R1_001.fastq.gz
4920_15_R10_20487881F3502f05R10_S19_L002_R1_001.fastq.gz
EOF

# folder to save all-runs-merged R1 and R2 files files
outfolder=fastp_out
mkdir -p ${outfolder}

# Set number of threads
nth=4

echo -e "# $(date)\n# running fastp on all read pairs in path" > ${outfolder}/run_log.txt

# Process each R1 prefix
while read r1; do
if [[ -n "$r1" ]]; then  # Check if the line is non-empty
    echo "# Finding R1 files for prefix: $r1"

    # Find all matching R1 files in the current path and sort them (by run)
    r1_files=$(find . -name "${r1}" | sort -u)

    # run fastp on each pair
    for read1 in ${r1_files}; do

      # extract run number from folder name
      runid=$(dirname ${read1}| sed -E 's/.*_([0-9]+)-RawData.*/\1/' | sed 's/^[0-9]$/0&/')

      # deduce read2
      read2=$(echo "${read1}" | sed 's/_R1_001.fastq.gz/_R2_001.fastq.gz/')

      # create subfolder for that sample
      smplout=$(basename ${read1%_L00?_R1_001.fastq.gz})
      mkdir -p ${outfolder}/${smplout}

      # define prefix (removing the "_merged_R1" suffix)
      prefix="${runid}_$(basename ${read1%_R1_001.fastq.gz}_fastp)"

      # Run fastp
      echo -e "\n# running fastp on ${read1} and ${read2}" | tee -a ${outfolder}/run_log.txt

      cmd="time fastp \
        -i ${read1} -o ${outfolder}/${smplout}/${prefix}_R1.fq.gz \
        -I ${read2} -O ${outfolder}/${smplout}/${prefix}_R2.fq.gz \
        -w ${nth} \
        -R ${prefix} \
        -j ${outfolder}/${prefix}_report.json \
        -h ${outfolder}/${prefix}_report.html"

      #echo "# ${cmd}"
      eval ${cmd} 2>&1 | tee -a ${outfolder}/run_log.txt

      echo ".. done for ${read1}"
      echo

    done
  fi
done <<< "${r1_list}"

conda deactivate
exit 0