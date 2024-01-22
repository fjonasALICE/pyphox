#!/bin/bash
#SBATCH -N 1
#SBATCH -c 1
#SBATCH -C cpu
#SBATCH -q shared
#SBATCH -J build
#SBATCH -A alice
#SBATCH -t 00:10:00
#SBATCH --license=scratch
#SBATCH --image=fjonas/jetphoxenv:latest


# Get all arguments and add them all to RUNOPTIONS
RUNOPTIONS=""
for var in "$@"
do
    RUNOPTIONS="$RUNOPTIONS $var"
done
echo "Running build_jetphox_perlmutter.sh with arguments: $RUNOPTIONS"

export PERL5LIB="${PSCRATCH}/pyphox/jptemplate/working:${PERL5LIB}"

# run build job

time shifter -e PERL5LIB=$PERL5LIB --module=none ${PSCRATCH}/pyphox/run_jetphox build $RUNOPTIONS

