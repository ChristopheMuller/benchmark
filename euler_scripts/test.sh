#!/bin/bash

#SBATCH -A public
#SBATCH -n 32
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=1
#SBATCH --job-name=test
#SBATCH --output=$(pwd)/euler_output/test.out
#SBATCH --mail-type=START,END,FAIL

# Use full paths
apptainer exec --bind $HOME:$HOME --bind $(pwd):$(pwd) env.sif Rscript $(pwd)/run.R
