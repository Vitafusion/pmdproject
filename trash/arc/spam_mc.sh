#!/bin/bash

#SBATCH -N 1
#SBATCH --ntasks-per-node=4
#SBATCH -t 1:00:00       
#SBATCH -p normal_q              
#SBATCH -A ascclass

cd $SLURM_SUBMIT_DIR

module purge
module load intel/18.2 mkl R/3.6.2

export R_LIBS="$HOME/cascades/R/3.6.2/intel/18.2/lib:$R_LIBS"
export MKL_NUM_THREADS=$SLURM_NTASKS

echo "$( date ): Starting spam_mc"
R CMD BATCH spam_mc.R
echo "$( date ): Finished spam_mc"

