#!/bin/bash

# Script: merge2lanes.sh
# Description: This script merges Aviti paired FASTQ files from 2 lanes
# Input reads are found in a subfolder specified by -i option
# Merged reads are written to a new subfolder 'merged_reads' by default
# Version: 1.6
# Date: 2025-01-27
# Author: SP@NC (+AI)

set -e

# Default values
input_dir=""
output_dir="merged_reads"
parallel_jobs=1

# Function to display usage
usage() {
    echo "Usage: ${0} -i input_dir [-o output_dir] [-j parallel_jobs]"
    echo "  -i input_dir    : Input directory (required)"
    echo "  -o output_dir   : Output directory (default: merged_reads)"
    echo "  -j parallel_jobs: Number of parallel jobs (default: 1)"
    exit 1
}

# Parse command line options
while getopts "i:o:j:h" opt; do
    case ${opt} in
        i )
            input_dir=${OPTARG}
            ;;
        o )
            output_dir=${OPTARG}
            ;;
        j )
            parallel_jobs=${OPTARG}
            ;;
        h )
            usage
            ;;
        \? )
            usage
            ;;
    esac
done

# Check if input directory is provided
if [ -z "${input_dir}" ]; then
    echo "Error: Input directory (-i) is required."
    usage
fi

# Create output directory if it doesn't exist
mkdir -p "${output_dir}"

# Find all unique sample numbers
samples=$(find "${input_dir}" -name "*fastp_R1.fq.gz" \
  | sed -E 's/.*_S([0-9]+)_.*/\1/' | sort -u)

# Function to process a single sample
process_sample() {
    local sample=${1}
    echo "Processing sample _S${sample}_"
    
    # Get the prefix from the first input file
    local prefix=$(find "${input_dir}" -name "*_S${sample}_*fastp_R1.fq.gz" \
      | head -n 1 | sed -E 's/.*\/(.*?)_S.*/\1/')
    
    # Merge R1 files
    find "${input_dir}" -name "*_S${sample}_*fastp_R1.fq.gz" | sort \
      | xargs zcat | gzip -c > "${output_dir}/${prefix}_S${sample}_merged_R1.fq.gz"
    
    # Merge R2 files
    find "${input_dir}" -name "*_S${sample}_*fastp_R2.fq.gz" | sort \
      | xargs zcat | gzip -c > "${output_dir}/${prefix}_S${sample}_merged_R2.fq.gz"
    
    echo -e "# sample _S${sample}_ lanes were merged"
}

# Export the function and necessary variables so they're available to parallel processes
export -f process_sample
export input_dir
export output_dir

# Process samples in parallel using GNU Parallel, with the specified number of jobs
echo "${samples}" | parallel -j "${parallel_jobs}" process_sample

echo "All samples processed successfully"
