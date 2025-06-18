#!/bin/bash

# =============================================================================
# Script: get_aviti_thumbnails.sh
# 
# Purpose: Collects and renames Aviti sequencer thumbnail images from run 
#          directories into a single location for easy viewing and comparison.
#
# Usage: ./get_aviti_thumbnails.sh
#
# Author: SP@NC (nucleomics@vib.be)
# Version: 1.0
# Date: June 18, 2025
# 
# The script will:
# 1. Find all PNG thumbnail files in the Aviti runs directory
# 2. Extract device ID and experiment information from the path
# 3. Copy files to a destination folder with standardized naming
# =============================================================================

# Source directory containing Aviti run data
INPUT_DIR="/mnt/nuc-transfer/0003_Runs/Aviti"

# Output directory where renamed files will be saved
OUTPUT_DIR="$PWD/aviti_thumbnails"

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Display start message
echo "=== Starting Aviti thumbnail collection ==="
echo "Searching for thumbnails in: $INPUT_DIR"
echo "Output directory: $OUTPUT_DIR"

# Counter for processed files
count=0

# Find all PNG files and process them
find "$INPUT_DIR" -name "*.png" | while read -r file; do
    # Extract the device name from the directory structure
    # (remove "A" prefix from "AV224503")
    device=$(basename "$(dirname "$(dirname "$file")")") 
    device=$(echo "$device" | sed 's/^A//')
    
    # Extract the experiment ID by parsing the parent directory name
    parent_dir=$(basename "$(dirname "$file")")
    
    # The parent directory format: YYYYMMDD_AVXXXXXX_EXPERIMENT_INFO
    # Example: 20250107_AV224503_4822_4911_1
    # Extract just the experiment part (4822_4911 or similar)
    experiment=$(echo "$parent_dir" | cut -d'_' -f3-)
    
    # Get the thumbnail filename (ThumbnailLane1.png or ThumbnailLane2.png)
    thumbnail=$(basename "$file")
    
    # Create the new filename with format: DEVICE_EXPERIMENT_THUMBNAIL.png
    new_filename="${device}_${experiment}_${thumbnail}"
    
    # Copy the file to the output directory
    echo "Copying: $thumbnail → $new_filename"
    cp "$file" "$OUTPUT_DIR/$new_filename"
    
    # Increment counter
    ((count++))
done

echo "=== Completed ==="
echo "$count thumbnail files have been copied to $OUTPUT_DIR"
echo "Process finished at $(date)"
