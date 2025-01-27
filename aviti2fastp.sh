#!/bin/bash

# Script: aviti2fastp.sh
# Description: This script processes Aviti paired FASTQ files using fastp
# The first argument is the common suffix of all R1 files (eg. _R1_001.fastq.gz)
# The second argument is a prefix to add to the output files
# The code processes all pairs based on the files found with ls and the suffix
# Filtered paired reads are written to a subfolder 'filtered_reads'
# REM: orphan single reads are not saved using this script but can be added (see manpage) 
# Version: 1.1
# Date: 2025-01-27
# Author: SP@NC (+AI)

# Check if two arguments are provided
if [ $# -ne 2 ]; then
    echo "Usage: $0 <read_file_pattern> <output_prefix>"
    echo "Example: $0 '_R1_001.fastq.gz' 'experiment1'"
    exit 1
fi

# Set the read file pattern and output prefix from the arguments
read_pattern=$1
output_prefix=$2

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
    
    # remove project# from front
    pfx=${pfx#*_}

    # Run fastp
    echo -e "\n# running fastp on ${r1} and ${r1/${read_pattern}/${read_pattern/_R1_/_R2_}}" | tee -a filtered_reads/run_log.txt
    
    cmd="time fastp -i ${r1} -o filtered_reads/${output_prefix}_${pfx}_fastp_R1.fq.gz \
      -I ${r1/${read_pattern}/${read_pattern/_R1_/_R2_}} -O filtered_reads/${output_prefix}_${pfx}_fastp_R2.fq.gz \
      -w ${nth} \
      -R ${output_prefix}_${pfx}_report \
      -j filtered_reads/${output_prefix}_${pfx}_report.json \
      -h filtered_reads/${output_prefix}_${pfx}_report.html"

    #echo "# ${cmd}"
    eval ${cmd} 2>&1 | tee -a filtered_reads/run_log.txt
done

echo "Processing complete." 2>&1 | tee -a filtered_reads/run_log.txt
