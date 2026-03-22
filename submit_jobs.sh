#!/bin/bash
# submit_jobs.sh
# Run this after ampute.R has finished and job_ids.txt exists.

while IFS= read -r id; do
  echo "Submitting: $id"
  oarsub -n "impute_${id}" \
         -l /nodes=1/core=1,walltime=2:00:00 \
         "Xvfb :99 -screen 0 1024x768x16 & export DISPLAY=:99 && apptainer exec env.sif Rscript run_one.R ${id}"
done < job_ids.txt