#!/bin/bash

if [ $# -eq 0 ]
  then
    echo "A job id must be provided"
else
  jobid=$1

  NDS=$( scontrol show hostnames "$( squeue -o %R -h -j $jobid )" )

  ##serial version
  #for nd in $NDS; do 
  #  ssh $nd "echo $nd; ps aux | grep jkrometi | egrep 'Rscript|R --slave' | grep -v grep"; 
  #done
  
  #parallel version (parallel can be used for non-R tasks, too!)
  module load parallel
  NDS=$( echo "$NDS" | paste -s -d, - )
  parallel --nonall -S $NDS "hostname; ps aux | grep jkrometi | egrep 'Rscript|R --slave' | grep -v grep"; 
fi
