#!/bin/bash
#SBATCH -J jetphox_build

DATE=`date +%Y_%m_%d`
RANDOMX=12345
NEVENTS=5000000
BORNBOX=2
ISO=2

# set path to find module
# RUNOPTIONS="--prefix Test_${DATE} --pdf1 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember1 0 --pdf2 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember2 0 --scaleis 1.0 --scalefs 1.0 --scalerenorm 1.0 --process dir --higherorder true --cme 5020 --maxrap 0.8 --minrap -0.8 --isoconeradius 0.4 --isoenergy $ISO --numberofevents $NEVENTS --randomseed $RANDOMX --directcalculation $BORNBOX --jetrapmin -5.8 --jetrapmax 5.8 --skipcopy false"

./run_jetphox submit $1 --skipcopy true

