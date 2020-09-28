#!/bin/bash --login

#SBATCH --nodes=2
#SBATCH --time=0:10:0
#SBATCH --partition=standard
#SBATCH --qos=standard
#SBATCH --exclusive
#SBATCH --tasks-per-node=36
#SBATCH --cpus-per-task=1
#SBATCH --exclusive

module load intel-compilers-18
module load hdf5parallel/1.10.6-intel18-mpt222
module load netcdf-parallel/4.7.4-intel18-mpt222

export MPI_TYPE_DEPTH=20

echo "Starting job $SLURM_JOB_ID at `date`"

for p in 1 36 72
do

mpiexec_mpt -ppn 36 -n $p ./benchio

done

echo "Finished job $SLURM_JOB_ID at `date`"
