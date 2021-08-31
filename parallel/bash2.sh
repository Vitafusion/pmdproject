#!/bin/sh
#SBATCH --time=144:00:00
#SBATCH -A bdpa
#SBATCH -p normal_q
#SBATCH -N 1
#SBATCH --mail-user=zhengzhi@vt.edu
#SBATCH --mail-type=FAIL,END
##SBATCH --requeue
cd /home/zhengzhi/pmd_parallel/
#######


module purge
export OMP_NUM_THREADS=1
Rscript --vanilla NormM3Tae.R

