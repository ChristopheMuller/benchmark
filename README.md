# Imputation Benchmark Pipeline

A parallelised simulation pipeline for benchmarking missing data imputation methods. Jobs are submitted individually to an OAR cluster, giving full transparency over what is running and where.

---

## Overview

```
ampute.R         — phase 1: amputate datasets, generate job list
run_one.R        — phase 2: one imputation + scoring job (called per job)
submit_jobs.sh   — submits all jobs to OAR
setup.R          — single source of truth for all parameters
```

---

## Step 1 — Place datasets in the right folders

| Folder | What goes here |
|---|---|
| `./data/datasets/complete/` | Complete datasets (will be amputed) |
| `./data/datasets/incomplete/` | Datasets that already have missing values (passed through as-is) |
| `./data/datasets/categorical/` | Complete datasets with categorical variables (will be amputed) |
| `./data/datasets/incomplete_categorical/` | Categorical datasets already with missing values |

All datasets should be `.RDS` files. The filename (without `.RDS`) becomes the dataset ID throughout the pipeline.

Also make sure these two files exist:
- `./data/functions.RDS` — table of imputation methods with a `Function name` column
- `./data/categorical_funs.RDS` — table of imputation methods valid for categorical data

---

## Step 2 — Configure the simulation in `setup.R`

Open `setup.R` and edit the parameters to define what the simulation will do:

```r
amputation_mechanisms = c("mcar", "mar"),   # missingness mechanisms to simulate
amputation_reps       = 2,                  # number of repeated amputations per setting
missing_ratios        = c(0.1, 0.2, 0.3),  # proportion of values to remove
```

To restrict which imputation methods are benchmarked, add a `filter()` after loading `imputation_methods`:

```r
imputation_methods <- readRDS(path_to_methods) %>%
  rename(imputation_fun = `Function name`) %>%
  mutate(method = str_remove(imputation_fun, "impute_")) %>%
  filter(method %in% c("mice", "missForest"))  # leave this out to run all methods
```

---

## Step 3 — Run amputation

```bash
Rscript ampute.R
```

This will:
1. Call `save_params()` from `setup.R` to build the full parameter grid and save it to `./data/params.RDS`
2. Amputate all complete datasets according to the configured mechanisms, ratios, and reps, saving results to `./results/amputed/`
3. Copy incomplete datasets through unchanged
4. Write `job_ids.txt` — one imputation job ID per line

This script is safe to re-run: datasets that already exist in `./results/amputed/` are skipped.

> ⚠️ Do not change parameters in `setup.R` after this step. `params.RDS` and `job_ids.txt` are regenerated on every run of `ampute.R`, so changing parameters mid-pipeline will make the job list inconsistent with the amputed data on disk.

---

## Step 4 — Submit imputation jobs

```bash
bash submit_jobs.sh
```

This reads `job_ids.txt` and submits one OAR job per line via `oarsub`. Each job calls:

```bash
Rscript run_one.R <imputed_id>
```

Each job independently:
- Reads its amputed dataset from `./results/amputed/`
- Runs the assigned imputation method
- Saves the imputed dataset to `./results/imputed/`
- Computes scores and saves them to `./results/scores/<imputed_id>.RDS`

You can adjust walltime and resource requirements at the top of `submit_jobs.sh`.

---


## Output files

| Path | Contents |
|---|---|
| `./data/params.RDS` | Full parameter grid (one row per imputation job) |
| `./results/amputed/<amputed_id>.RDS` | Amputed datasets |
| `./results/imputed/<imputed_id>.RDS` | Imputed datasets |
| `./results/scores/<imputed_id>.RDS` | Per-job scores |
| `./results/imputation_summary.RDS` | Final combined results |