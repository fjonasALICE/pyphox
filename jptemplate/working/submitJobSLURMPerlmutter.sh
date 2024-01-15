#!/bin/bash
#SBATCH -N 1
#SBATCH -C cpu
#SBATCH -q regular
#SBATCH -J jetphox_run
#SBATCH -t 12:00:00


# get current working directory
CURRENTDIR=`pwd`

srun -n 1 -c 256 --cpu_bind=cores $CURRENTDIR/$1

# time ./$1
# exit $?
