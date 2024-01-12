#!/bin/bash
#SBATCH -J jetphox
#SBATCH -p long
# eval `alienv -w /software/flo/alice/sw/ --no-refresh load AliPhysics/latest lhapdf/latest`
# export LHAPDF_DATA_PATH=$LHAPDF_DATA_PATH:/cvmfs/sft.cern.ch/lcg/external/lhapdfsets/current:/alf/data/flo/pdfsets

# get current working directory
CURRENTDIR=`pwd`

# load jetphoxenv docker and run exe
docker run -it --rm -v /alf/data/flo/PyPhox/:/home/PyPhox jetphoxenv:latest $CURRENTDIR/$1


# time ./$1
# exit $?
