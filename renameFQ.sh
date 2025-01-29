#!/bin/bash

# Script: renameFQ.sh
# Description: Extract and rename fastq filename variables
# Author: SP@NC
# Date: 2025-01-28
# Version: 1.3

# Check if an argument is provided
if [ $# -eq 0 ]; then
    echo "Error: Please provide the path to the input file."
    exit 1
fi

# Extract filename from the provided path
filename=$(basename "$1")

# Split the filename into parts
IFS='_' read -ra parts <<< "${filename}"

# Initialize variables
exp="${parts[0]}"
sid="${parts[1]}"
lab="${parts[2]}"
phe=""
snu=""
lnu=""
rid=""
sfx=""

# Handle the case where lab contains an underscore
if [ ${#parts[@]} -eq 9 ]; then
    lab="${parts[2]}_${parts[3]}"
    phe="${parts[4]}"
    snu="${parts[5]}"
    lnu="${parts[6]}"
    rid="${parts[7]}"
    sfx="${parts[8]}"
else
    phe="${parts[3]}"
    snu="${parts[4]}"
    lnu="${parts[5]}"
    rid="${parts[6]}"
    sfx="${parts[7]}"
fi

# Replace underscore with dash in lab if it contains an underscore
lab="${lab/_/-}"

# Report the variables in the specified order
echo "Extracted variables:"
echo "exp: $exp"
echo "sid: $sid"
echo "lab: $lab"
echo "phe: $phe"
echo "snu: $snu"
echo "lnu: $lnu"
echo "rid: $rid"
echo "sfx: $sfx"

# Set pfx using the extracted variables
pfx="${lab}_${phe}_${rid}.fq.gz"

echo "pfx: $pfx"