#!/bin/bash
DATE=`date +%Y_%m_%d`
RANDOMX=12345
NEVENTS=5000000
BORNBOX=2
ISO=2

# eval `$(which alienv) -w /software/flo/alice/sw --no-refresh printenv AliPhysics/latest-master-o2 lhapdf/latest-o2`
# export LHAPDF_DATA_PATH=$LHAPDF_DATA_PATH:/cvmfs/sft.cern.ch/lcg/external/lhapdfsets/current:/alf/data/flo/pdfsets

# # NEVENTS=50000000

# export PERL5LIB=/alf/data/flo/BashPhox/jp1/working

# 1. run docker container jetphoxenv:latest and mount directory  /alf/data/flo/PyPhox/ under /PyPhox
# 2. go into /PyPhox and run ./run_jetphox

RUNOPTIONS = "--prefix Test_${DATE} --pdf1 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember1 0 --pdf2 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember2 0 --scaleis 1.0 --scalefs 1.0 --scalerenorm 1.0 --process dir --higherorder true --cme 5020 --maxrap '0.8' --minrap '-0.8' --isoconeradius 0.4 --isoenergy $ISO --numberofevents $NEVENTS --randomseed $RANDOMX --directcalculation $BORNBOX --jetrapmin '-5.8' --jetrapmax '5.8' --skipcopy false"


# build all files needed (inside docker)
docker run -it --rm -v /alf/data/flo/PyPhox/:/home/PyPhox jetphoxenv:latest run_jetphox build $RUNOPTIONS
# submit job to slurm (outside docker, since docker container does not know sbatch)
./run_jetphox submit $RUNOPTIONS
