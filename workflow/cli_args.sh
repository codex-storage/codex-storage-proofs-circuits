#!/bin/bash

MY_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source ${MY_DIR}/params.sh

CLI_ARGS="--depth=$MAXDEPTH \
 --maxslots=$MAXSLOTS \
 --cellsize=$CELLSIZE \
 --blocksize=$BLOCKSIZE \
 --nsamples=$NSAMPLES \
 --entropy=$ENTROPY \
 --seed=$SEED \
 --nslots=$NSLOTS \
 --ncells=$NCELLS \
 --index=$SLOTINDEX"

if [[ "$1" =~ "--export" ]]
then
  echo "exporting CLI_ARGS"
  export CLI_ARGS
fi
