#!/bin/bash

# Script: aviti2fastp.sh
# Description: This script processes Aviti paired FASTQ files using fastp
# the argument is the common suffix of all R1 files (eg. _R1_001.fastq.gz)
# the code processes all pairs based on the files found with ls and the suffix
# filtered paired read are written to a subfolder 'filtered_reads'
# REM: orphan single reads are not saved using this script but can be added (see manpage) 
# Version: 1.0
# Date: 2025-01-24
# Author: SP@NC (+AI)

# Check if an argument is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <read_file_pattern>"
    echo "Example: $0 '_R1_001.fastq.gz'"
    exit 1
fi

# Set the read file pattern from the argument
read_pattern=$1

# Activate conda environment
myenv=fastp
source /etc/profile.d/conda.sh
conda activate ${myenv} || \
  ( echo "# the conda environment ${myenv} was not found on this machine" ;
    echo "# please read the top part of the script!" \
    && exit 1 )

# Create output folder
mkdir -p filtered_reads

# Set number of threads
nth=4

# Process files for each pair
for r1 in $(ls *${read_pattern}); do
    # Extract minimal sample name string
    pfx=${r1%${read_pattern}}
    pfx=${pfx#*_}
    pfx=${pfx%_S[0-9]*}

    # Debug (uncomment if needed)
    # echo ${r1} ${pfx} ${lane}

    # Run fastp
	echo "running fastp on ${r1} and ${r1/R1/R2}:"
    cmd="time fastp -i ${r1} -o filtered_reads/${pfx}_fastp_R1.fq.gz \
      -I ${r1/R1/R2} -O filtered_reads/${pfx}_fastp_R2.fq.gz \
      -w ${nth} \
      -R ${pfx}_report \
      -j filtered_reads/${pfx}_report.json \
      -h filtered_reads/${pfx}_report.html"

	#echo "# ${cmd}"
	eval ${cmd} 2>&1 | tee -a filtered_reads/run_log.txt
done

echo "Processing complete." 2>&1 | tee -a filtered_reads/run_log.txt
