#!/bin/bash
# aviti_count_tiles.sh

nthr=48

# lane 1 by default
lane=${1:-1}

gettile (){
bioawk -c fastx '{split($name,arr,":"); print arr[5]}' $1
}

export -f gettile

# parse all fastq.gz and print count of unique tiles
parallel --plus -j${nthr} gettile {} ::: Samples/*/*L00${lane}_R1_001.fastq.gz | \
  sort | uniq -c