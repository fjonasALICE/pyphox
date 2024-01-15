#!/bin/bash
#SBATCH -N 1
#SBATCH -C cpu
#SBATCH -q regular
#SBATCH -J jetphox_build
#SBATCH -t 02:00:00

DATE=`date +%Y_%m_%d`
RANDOMX=12345
NEVENTS=5000000
BORNBOX=2
ISO=2


srun -n 1 -c 256 --cpu_bind=cores /global/u1/f/fjonas/pyphox/run_jetphox submitperlmutter $1 --skipcopy true

