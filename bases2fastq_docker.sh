#!/bin/bash

# bases2fastq_docker.sh
# basecall Aviti data from a completed run folder
# run the command from the folder enclosing the run folder !!
# run from docker as uid:gid to avoid outputfolder being owned by root
#
# Stéphane Plaisance - VIB-NC June-06-2018 v1.0
# visit our Git: https://github.com/Nucleomics-VIB

version="1.0, 2023_04_18"

usage='# Usage: bases2fastq_docker.sh
# -r <path to run folder (in the current path)> 
# -o <path to output folder (in the current path)>
# optional -t <threads> (default 8)>
# script version '${version}'
# [-h for this help]'

while getopts "r:o:t:h" opt; do
  case $opt in
    r) runfolder=${OPTARG} ;;
    o) outfolder=${OPTARG} ;;
    t) threads=${OPTARG} ;;
    h) echo "${usage}" >&2; exit 0 ;;
    \?) echo "Invalid option: -${OPTARG}" >&2; exit 1 ;;
    *) echo "this command requires arguments, try -h" >&2; exit 1 ;;
  esac
done

# get the docker (run once)
# docker pull elembio/bases2fastq
# elembio/bases2fastq              latest              ab8ff63330af   3 months ago    1.02GB

# check if a docker image is available (command below returns its ID)
dockerimg="elembio/bases2fastq"
dockertag="latest"
if [[ "$(docker images -q ${dockerimg}:${dockertag} 2> /dev/null)" == "" ]]; then
    echo "image not present on the server"
    exit 1
fi

# check if arguments were provided
if [ -z "${runfolder+x}" ] || [ -z "${outfolder+x}" ]
then
   echo "# please provide mandatory arguments -r and -o!"
   echo "${usage}"
   exit 1
fi

# check run folder exists
if [ ! -d "${runfolder}" ]; then
	echo "${runfolder} folder not found!"
	exit 1
fi

# check thet run folder is complete and contains RunUploaded.json
if [ -z "${runfolder}/RunUploaded.json" ]; then
    echo "# the run folder does not contain 'RunUploaded.json'"
    echo "# it might not be fully transferred, please check"
    exit 1
fi

# default num threads
nthr=${threads:-8}

# or simplified to
docker run \
  --rm \
  --user "$(id -u):$(id -g)" \
  -v $PWD:/workdir \
  elembio/bases2fastq:latest bases2fastq \
    -p ${nthr} \
    --settings 'R1AdapterTrim,FALSE' \
    --settings 'R2AdapterTrim,TRUE' \
    --legacy-fastq \
    /workdir/${runfolder} \
    /workdir/${outfolder}
