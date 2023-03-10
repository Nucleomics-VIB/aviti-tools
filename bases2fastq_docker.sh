#!/bin/bash

# bases2fastq_docker.sh
# basecall Aviti data from a completed run folder
# run the command from the folder enclosing the run folder !!
# run from docker as uid:gid to avoid output folder being owned by root
#
# Stéphane Plaisance - VIB-NC January-17-2023 v1.0
# visit our Git: https://github.com/Nucleomics-VIB

version="1.02, 2023_01_25"

usage='# Usage: bases2fastq_docker.sh
# -i <path to run folder (in the current path)> 
# -o <path to output folder (in the current path)>
# optional -m <path to custom RunManifest.csv (by default RunManifest.csv in the run folder)>
# optional -t <threads> (default 8)>
# optional -l <log level (default DEBUG, INFO for less)>
# optional -x <if NOT set apply --legacy-fastq (default)>
# script version '${version}'
# [-h for this help]'

while getopts "i:o:m:t:l:xh" opt; do
  case $opt in
    i) runfolder=${OPTARG} ;;
    o) outfolder=${OPTARG} ;;
    m) manifest=${OPTARG} ;;
    t) threads=${OPTARG} ;;
    l) loglevel=${OPTARG} ;;
    x) nolegacy=1 ;;
    h) echo "${usage}" >&2; exit 0 ;;
    \?) echo "Invalid option: -${OPTARG}" >&2; exit 1 ;;
    *) echo "this command requires arguments, try -h" >&2; exit 1 ;;
  esac
done

# get the docker (run once)
# docker pull elembio/bases2fastq
# elembio/bases2fastq latest ab8ff63330af 3 months ago 1.02GB

# check if a docker image is available (command below returns its ID)
dockerimg="elembio/bases2fastq"
dockertag="latest"
if [[ "$(docker images -q ${dockerimg}:${dockertag} 2> /dev/null)" == "" ]]; then
    echo "image not present on the server"
    exit 1
fi

# check if required arguments were provided
if [ -z "${runfolder+x}" ] || [ -z "${outfolder+x}" ]
then
   echo "# please provide mandatory arguments -i and -o!"
   echo "${usage}"
   exit 1
fi

# check if run folder exists
if [ ! -d "${runfolder}" ]; then
	echo "${runfolder} folder not found!"
	exit 1
fi

# check if run folder is complete and contains RunUploaded.json
if [ -z "${runfolder}/RunUploaded.json" ]; then
    echo "# the run folder does not contain 'RunUploaded.json'"
    echo "# it might not be fully transferred, please check"
    exit 1
fi

# default or custom num threads
nthr=${threads:-8}

# default or custom manifest
manifest=${manifest:-"${runfolder}/RunManifest.csv"}

# legacy or not
if [[ "${nolegacy}" == 1 ]]; then
  legacy=""
else 
  legacy="--legacy-fastq"
fi

# default or increased logging
# Severity level for logging. i.e. DEBUG, INFO, WARNING, ERROR (default INFO)
deflog="DEBUG"
loglv=${loglevel:-"${deflog}"}

# calling docker
docker run \
  --rm \
  --user "$(id -u):$(id -g)" \
  -v $PWD:/workdir \
  elembio/bases2fastq:latest bases2fastq \
    --num-threads ${nthr} \
    --run-manifest /workdir/${manifest} \
    ${legacy} \
    --log-level ${loglv} \
    /workdir/${runfolder} \
    /workdir/${outfolder}
