#!/bin/bash
DATE=`date +%Y_%m_%d`
RANDOMX=12345
NEVENTS=5000000
BORNBOX=2
ISO=2

LOCALPATH=${HOME}/pyphox/
SCRATCHPATH=${PSCRATCH}/pyphox/

# copy everything to scratch storage
cp -r $LOCALPATH $SCRATCHPATH
cd $SCRATCHPATH


# do PDF uncertainties for pp
# loop over all PDF members
buildjobdirid=0
for i in {0..101}
do
RUNOPTIONS="--prefix Test_${DATE} --pdf1 NNPDF40_nnlo_as_01180 --pdfmember1 ${i} --pdf2 NNPDF40_nnlo_as_01180 --pdfmember2 ${i} --scaleis 1.0 --scalefs 1.0 --scalerenorm 1.0 --higherorder true --cme 5020 --maxrap 0.8 --minrap -0.8 --isoconeradius 0.4 --isoenergy $ISO --numberofevents $NEVENTS --randomseed $RANDOMX --directcalculation $BORNBOX --jetrapmin -5.8 --jetrapmax 5.8 --skipcopy false"

# build and submit job to slurm
# dir
if [ $i -eq 0 ]
then
buildjobdirid=$(sbatch build_jetphox_perlmutter.sh $RUNOPTIONS --process dir)
else
buildjobdirid=$(sbatch --dependency=afterok:${buildjobdirid##* } build_jetphox_perlmutter.sh $RUNOPTIONS --process dir)
fi

sbatch --dependency=afterok:${buildjobdirid##* } submit_jetphox_perlmutter.sh $RUNOPTIONS --process dir

# frag
buildjobfragid=$(sbatch --dependency=afterok:${buildjobdirid##* } build_jetphox_perlmutter.sh $RUNOPTIONS --process onef)
sbatch --dependency=afterok:${buildjobfragid##* } submit_jetphox_perlmutter.sh $RUNOPTIONS --process onef
done

cd $LOCALPATH



