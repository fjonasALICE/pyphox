#!/bin/bash
#SBATCH -N 1
#SBATCH -C cpu
#SBATCH -q regular
#SBATCH -J jetphox_build
#SBATCH -t 02:00:00
#SBATCH --image=fjonas/jetphoxenv:latest


# Get all arguments and add them all to RUNOPTIONS
RUNOPTIONS=""
for var in "$@"
do
    RUNOPTIONS="$RUNOPTIONS $var"
done
echo "Running build_jetphox_perlmutter.sh with arguments: $RUNOPTIONS"

# run builld job
srun -n 1 -c 1 --cpu_bind=cores /global/u1/f/fjonas/pyphox/run_jetphox build $RUNOPTIONS

