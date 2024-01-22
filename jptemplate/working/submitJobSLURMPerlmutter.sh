#!/bin/bash
#SBATCH -N 1
#SBATCH -c 1
#SBATCH -C cpu
#SBATCH -q shared
#SBATCH -J jetphox_run
#SBATCH -A alice
#SBATCH -t 5:00:00
#SBATCH --license=scratch
#SBATCH --image=fjonas/jetphoxenv:latest


# get current working directory
CURRENTDIR=`pwd`

export PERL5LIB="${PSCRATCH}/pyphox/jptemplate/working:${PERL5LIB}"
time shifter -e PERL5LIB=$PERL5LIB --module=none $CURRENTDIR/$1

# time ./$1
# exit $?
