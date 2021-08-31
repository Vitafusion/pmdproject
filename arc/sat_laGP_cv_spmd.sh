#!/bin/bash
#SBATCH -t 12:00:00
#SBATCH -p normal_q
#SBATCH -A ascclass
#SBATCH -n 8 --cpus-per-task=8

# Add modules
rver="3.6.1" #R version
module purge
module load intel/18.2 mkl R/$rver openmpi/4.0.1 R-parallel/$rver

#set r libraries
export R_LIBS="$HOME/cascades/R/$rver/intel/18.2/lib:$R_LIBS"

#set num threads
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

#openmpi environment variables
export OMPI_MCA_btl_openib_allow_ib=1 #allow infiniband

#timestamp
echo "$( date ): Starting laGP (spmd version)"

## Run R (spmd mode)
SCRIPT=sat_laGP_cv_spmd.R  
srun --cpu_bind=cores Rscript $SCRIPT

#timestamp
echo "$( date ): Finished laGP (spmd version)"
