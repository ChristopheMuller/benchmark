#!/bin/bash
# submit_jobs.sh
# Run this after ampute.R has finished and job_ids.txt exists.

mkdir -p logs logs_error

while IFS= read -r id; do
  echo "Submitting: $id"
  oarsub -n "impute_${id}" \
         -l /nodes=1/core=1,walltime=10:00:00 -q production \
         --stdout "logs/${id}.txt" \
         --stderr "logs_error/${id}.txt" \
         "Xvfb :99 -screen 0 1024x768x16 & export DISPLAY=:99 && module load apptainer && apptainer exec env.sif Rscript run_one.R ${id}"
done < job_ids.txt