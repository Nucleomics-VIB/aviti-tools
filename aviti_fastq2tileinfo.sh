#!/bin/bash
# aviti_fastq2tileinfo.sh

fastq=$1

tilecnt=$(bioawk -c fastx '{split($name,arr,":"); print arr[5]}' ${fastq} \
  | sort | uniq -c | wc -l)

echo "# found ${tilecnt} different tiles in ${fastq}"

bioawk -c fastx 'BEGIN{
  minx=2000; 
  maxx=0; 
  miny=2000; 
  maxy=0};
  {split($name,arr,":");
  # test min and max x&y
  if(arr[6]<minx) minx=arr[6];
  if(arr[7]<miny) miny=arr[7];
  if(arr[6]>maxx) maxx=arr[6];
  if(arr[7]>maxy) maxy=arr[7];
  }
  END{
  print "# x range " minx":"maxx;
  print "# y range " miny":"maxy;
  }' ${fastq}


