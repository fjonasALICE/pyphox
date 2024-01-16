#!/bin/bash
#SBATCH -N 1
#SBATCH -c 1
#SBATCH -C cpu
#SBATCH -q shared
#SBATCH -J jetphox_build
#SBATCH -A alice
#SBATCH -t 00:30:00
#SBATCH --image=fjonas/jetphoxenv:latest


# Get all arguments and add them all to RUNOPTIONS
RUNOPTIONS=""
for var in "$@"
do
    RUNOPTIONS="$RUNOPTIONS $var"
done
echo "Running build_jetphox_perlmutter.sh with arguments: $RUNOPTIONS"

export PERL5LIB="/global/u1/f/fjonas/pyphox/jptemplate/working:${PERL5LIB}"

# run builld job

time shifter -e PERL5LIB=$PERL5LIB --module=none /global/u1/f/fjonas/pyphox/run_jetphox build $RUNOPTIONS

