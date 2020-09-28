#!/bin/bash --login

#SBATCH --nodes=4
#SBATCH --time=0:10:0
#SBATCH --partition=standard
#SBATCH --qos=standard
#SBATCH --exclusive
#SBATCH --tasks-per-node=36
#SBATCH --cpus-per-task=1
#SBATCH --exclusive


echo "Starting job $SLURM_JOB_ID at `date`"

for p in 1 36 72 144
do

srun -n $p ./benchio

done

echo "Finished job $SLURM_JOB_ID at `date`"
