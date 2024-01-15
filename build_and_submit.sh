#!/bin/bash

RUNOPTIONS="--prefix Test_${DATE} --pdf1 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember1 0 --pdf2 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember2 0 --scaleis 1.0 --scalefs 1.0 --scalerenorm 1.0 --process dir --higherorder true --cme 5020 --maxrap 0.8 --minrap -0.8 --isoconeradius 0.4 --isoenergy $ISO --numberofevents $NEVENTS --randomseed $RANDOMX --directcalculation $BORNBOX --jetrapmin -5.8 --jetrapmax 5.8 --skipcopy false"

buildjobid=$(sbatch build_jetphox_perlmutter.sh $RUNOPTIONS)

# submit subit_jetphox_perlmutter.sh after build_jetphox_perlmutter.sh has finished
sbatch --dependency=afterok:${buildjobid##* } submit_jetphox_perlmutter.sh $RUNOPTIONS