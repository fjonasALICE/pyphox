#!/bin/bash
#SBATCH -J jetphox_build
#SBATCH --image=fjonas/jetphoxenv:latest


# get current working directory
CURRENTDIR=`pwd`

time $CURRENTDIR/$1

# time ./$1
# exit $?
