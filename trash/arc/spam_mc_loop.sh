#!/bin/bash

#SBATCH -N 1
#SBATCH --ntasks-per-node=16
#SBATCH -t 1:00:00       
#SBATCH -p normal_q              
#SBATCH -A ascclass

cd $SLURM_SUBMIT_DIR

module purge
module load intel/18.2 mkl R/3.6.2

#set r libraries
export R_LIBS="$HOME/cascades/R/3.6.2/intel/18.2/lib:$R_LIBS"

#set number of cores used by each r process
export MKL_NUM_THREADS=2

#number of r processes to run
ncopies=8

echo "$( date ): Starting spam_mc"

for i in $( seq 1 $ncopies ); do 
  R CMD BATCH "--args seed=$i reps=5" spam_mc.R spam_mc_${i}.Rout &
done
wait

echo "$( date ): Finished spam_mc"

