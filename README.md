# Benchmarking Missing Data Imputation Methods

## Overview

This repository contains code for the research project accompanying the article:

> Grzesiak, K., Muller, C., Josse, J., & Näf, J. (2025).  
> *"Do we Need Dozens of Methods for Real World Missing Value Imputation?"*  
> arXiv preprint arXiv:2511.04833

This repository provides a full benchmarking pipeline for evaluating missing data imputation methods on numerical and mixed-type datasets.

The pipeline includes:

- Artificial missingness generation (amputation),
- Imputation using custom R or Python functions,
- Evaluation and aggregation of performance metrics.

If you use this repository for benchmarking, comparison, or published research, please cite the above paper.

## ⚠️ Start Here: `_targets.R`

All simulation settings and pipeline logic are controlled in `_targets.R`.

Before running anything, **carefully review and edit `_targets.R`**.

This file defines:

- Datasets used in the benchmark,
- Amputation mechanisms,
- Missingness ratios,
- Number of replicates,
- Selected imputation methods,
- Evaluation settings,
- Paths and workflow structure.

The benchmark is built using the `{targets}` pipeline framework.  
All major components of the experiment are orchestrated from `_targets.R`.

After adjusting parameters, run:

```sh
Rscript run.R
```

or 

```r
targets::tar_make_clustermq(workers = 10)
```
You can specify the number of workers yourself.


## 1. Prerequisites

### 1.1 R Environment

We recommend using **renv** for reproducibility:

```r
# Restore the environment (installs packages specified in renv.lock)
renv::restore()
```

Make sure you are using the correct R version (see `renv.lock` if needed).


###  1.2 Optional Python Integration

If you do not plan to use Python-based imputation methods, you can skip this section.

To enable Python methods:

1. Create a virtual environment in `./.venv` using the provided scripts:

```r
# Windows
system("./python/windows_setup.sh")  

# Linux/macOS
system("./python/linux_setup.sh")
```

This creates a virtual environment in `./.venv`.

2. Activate the environment in R:

```r
reticulate::use_virtualenv("./.venv", required = TRUE)
```
3. Python functions can be loaded with:

```r
source("python/python_imputation_functions.R")
```

## 2. Preparing Custom Imputation Methods

Custom methods should be placed in

```
./my_methods/
```

#### Rules

1. Each method must be in a separate file.

2. The file name must match the method name.

3. The main imputation function must start with `impute_`.


**Example**: 

- **Method name:** `nice_imputation`, 

- **File:** `nice_imputation.R`, 

- **Function:** `impute_nice_imputation()`.

Additional helper functions may be included in the file.
Only the `impute_` function will be registered as the imputation method.

4. Validate methods:

```r
validate_my_methods()          # checks naming and presence of impute_ functions
collect_my_methods()           # lists all methods found
```

## 3. Dataset Structure

Datasets are organized as follows:

```
data/datasets/
├── complete/                  # complete numerical datasets
├── categorical/               # complete categorical datasets
├── incomplete/                # incomplete numerical datasets
└── incomplete_categorical/    # incomplete categorical datasets
```

You can inspect dataset properties with:

```r
readRDS("./data/datasets/sets_dim.RDS")
```

#### Note on Categorical Data

- If your method does not support categorical variables, remove such datasets in `_targets.R` before running the benchmark.

- If your method requires preprocessing (e.g., one-hot encoding, ordinal encoding), handle this inside your `impute_` function.

## 4. Benchmark Parameters

All core parameters are defined and passed through `_targets.R`.

Key configurable components:

### 1. Amputation:

- Missingness mechanisms (e.g., MCAR, MAR, MNAR),

- Missing ratios,

- Number of replicates.

### 2. Imputation:

- Timeout (in seconds),

- Maximum number of retry attempts in case of failure.

Some parameter objects are stored in:

``` 
data/params.RDS 
```

## 5. Running the Benchmark

After editing `_targets.R`, run:

```sh
Rscript run.R
```
or 

```r
targets::tar_make_clustermq(workers = 10)
```

Depending on the configuration, the benchmark may take a significant amount of time.

## 6. Output

```
./results/
├── amputed/                      # amputed datasets
├── imputed/                      # imputed datasets
├── amputation_summary.RDS
└── imputation_summary.RDS
```

## 7. Reproducibility Notes

- Always restore the `renv` environment before running.

- Do not manually modify files inside `results/` during pipeline execution.

- All experimental logic should be modified via `_targets.R`, not individual scripts.

## Citation

If you use this repository in academic work, please cite:

Grzesiak, K., Muller, C., Josse, J., & Näf, J. (2025).
Do we Need Dozens of Methods for Real World Missing Value Imputation?
arXiv preprint arXiv:2511.04833.

```bibtex
@article{grzesiak2025imputation,
  title   = {Do we Need Dozens of Methods for Real World Missing Value Imputation?},
  author  = {Grzesiak, Krystyna and Muller, C. and Josse, J. and Näf, J.},
  journal = {arXiv preprint arXiv:2511.04833},
  year    = {2025}
}
```
