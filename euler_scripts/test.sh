#!/bin/bash

#SBATCH -A public
#SBATCH -n 32
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=1
#SBATCH --job-name=test
#SBATCH --output=./euler_output/test.out
#SBATCH --mail-type=START,END,FAIL

apptainer exec --bind $HOME:$HOME env.sif "Rscript run.R"