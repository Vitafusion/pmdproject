#!/bin/bash
#SBATCH -t 10:00
#SBATCH -p normal_q 
#SBATCH -A ascclass
#SBATCH -N5 --ntasks-per-node=4

## interactive version for testing
## interact -N5 --ntasks-per-node=4 -t 1:00:00 -A ascclass

### Add modules
rver="3.6.1" #R version
module purge
module load intel/18.2 mkl R/$rver openmpi/4.0.1 R-parallel/$rver

# Set r libraries
export R_LIBS="$HOME/cascades/R/$rver/intel/18.2/lib:$R_LIBS"

# Set num threads
export OMP_NUM_THREADS=$SLURM_NTASKS_PER_NODE

# OpenMPI environment variables
export OMPI_MCA_btl_openib_allow_ib=1 #allow infiniband
export OMPI_MCA_rmaps_base_inherit=1  #slaves inherit environment

# Run R
# --map-by creates a process on each node
# --bind-to lets OpenMP run across the allocated cores on the node
SCRIPT=sat_laGP_cv_snow.R  
#mpirun -np 1 Rscript $SCRIPT & #bad mapping
#mpirun -np 1 --map-by ppr:1:node Rscript $SCRIPT & #good mapping, but stuck in 1-core box
mpirun -np 1 --map-by ppr:1:node --bind-to none Rscript $SCRIPT &

## Solution to the MPI busy-wait problem
while [ 1 ]; do
    sleep 1
    PID=$(pgrep -f "R --slave --no-restore --file=$SCRIPT")
    [ -z "$PID" ] || break
done

renice 19 $PID

wait

#exit;
