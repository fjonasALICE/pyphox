#!/bin/bash
#SBATCH -N 1
#SBATCH -c 1
#SBATCH -C cpu
#SBATCH -q shared
#SBATCH -J jetphox_run
#SBATCH -A alice
#SBATCH -t 12:00:00
#SBATCH --image=fjonas/jetphoxenv:latest


# get current working directory
CURRENTDIR=`pwd`

export PERL5LIB="/global/u1/f/fjonas/pyphox/jptemplate/working:${PERL5LIB}"
time $CURRENTDIR/$1

# time ./$1
# exit $?
