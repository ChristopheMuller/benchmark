#!/bin/bash

#SBATCH -A public
#SBATCH -n 25
#SBATCH --time=120:00:00
#SBATCH --cpus-per-task=1
#SBATCH --job-name=bench
#SBATCH --mem-per-cpu=5G
#SBATCH --output=./output/29th_out.out
#SBATCH --mail-type=END,FAIL

# Use full paths
export R_USER_CACHE_DIR=$HOME/.R_cache
export RENV_PATHS_CACHE=$HOME/.R_cache/renv/cache
export RENV_PATHS_LIBRARY=$HOME/.R_cache/renv/library

apptainer exec --bind $HOME:$HOME --bind $(pwd):$(pwd) env.sif Rscript $(pwd)/run.R
