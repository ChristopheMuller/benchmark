#!/bin/bash

#SBATCH -A public
#SBATCH -n 1
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=16
#SBATCH --job-name=bench
#SBATCH --mem-per-cpu=8G
#SBATCH --output=./output/30bth_out.out
#SBATCH --mail-type=END,FAIL

# Use full paths
export R_USER_CACHE_DIR=$HOME/.R_cache
export RENV_PATHS_CACHE=$HOME/.R_cache/renv/cache
export RENV_PATHS_LIBRARY=$HOME/.R_cache/renv/library

apptainer exec --bind $HOME:$HOME --bind $(pwd):$(pwd) env.sif Rscript $(pwd)/run.R
