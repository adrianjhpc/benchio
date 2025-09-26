#!/bin/bash --login

#SBATCH --nodes=2
#SBATCH --time=0:20:0
#SBATCH --partition=standard
#SBATCH --qos=short
#SBATCH --tasks-per-node=128
#SBATCH --cpus-per-task=1
#SBATCH --exclusive

module swap PrgEnv-cray PrgEnv-gnu
module load cray-hdf5-parallelcray-hdf5-parallel
module load cray-netcdf-hdf5parallel

export MPI_TYPE_DEPTH=20

echo "Starting job $SLURM_JOB_ID at `date`"

for p in 1 32 64 128 256
do

srun -n $p ./benchio

done

echo "Finished job $SLURM_JOB_ID at `date`"
