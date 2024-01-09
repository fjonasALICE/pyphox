#!/bin/bash
#SBATCH -J jetphox
#SBATCH -p long
eval `alienv -w /software/flo/alice/sw/ --no-refresh load AliPhysics/latest lhapdf/latest`
export LHAPDF_DATA_PATH=$LHAPDF_DATA_PATH:/cvmfs/sft.cern.ch/lcg/external/lhapdfsets/current:/alf/data/flo/pdfsets

time ./$1
exit $?
