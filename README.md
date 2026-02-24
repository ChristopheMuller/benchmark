
# Benchmarking Missing Data Imputation Methods


## Overview

This repository contains code for the research project accompanying the article:

> Grzesiak, K., Muller, C., Josse, J., & Näf, J. (2025).  
> "Do we Need Dozens of Methods for Real World Missing Value Imputation?"  
> *arXiv preprint arXiv:2511.04833*


This repository contains the full pipeline for benchmarking a wide range of missing data imputation methods on both numerical and mixed-type datasets, as presented in our study. If you use this code for benchmarking, comparison, or as part of published research, please cite the above paper.

The code runs a full benchmark pipeline for missing value imputation on multiple datasets, including:

- Amputation (artificial missingness),
- Imputation using custom R or Python functions,
- Evaluation of imputation performance.


## 1. Prerequisites

### 1.1 R Environment

We recommend using **renv** for reproducibility:

```r
# Restore the environment (installs packages specified in renv.lock)
renv::restore()
```


###  1.2 Optional Python Integration

If you don’t want to use Python, you can skip this section. If you want to use Python-based imputation methods:

1. Create a virtual environment in ./.venv using the provided scripts:

```r
# Windows
system("./python/windows_setup.sh")  

# Linux/macOS
system("./python/linux_setup.sh")
```

2. Activate the environment in R:

```r
reticulate::use_virtualenv("./.venv", required = TRUE)
```
3. Python functions can be loaded with:

```r
source("python/python_imputation_functions.R")
```

## 2. Preparing Custom Imputation Methods

1. Place each custom method in a separate file inside ./my_methods/.

- The file name should match the method name.

- The imputation function must start with `impute_`. Example: **Method name:** `nice_imputation`, **File:** `nice_imputation.R`, **Function:** `impute_nice_imputation()`.

2. Files may contain additional helper functions.
Only the `impute_` function will be used as the imputation method.

3. Validate methods:

```r
validate_my_methods()          # checks naming and presence of impute_ functions
collect_my_methods()           # lists all methods found
```

## 3. Dataset Structure

Datasets are organized as:

- `data/datasets/complete/` — complete numerical datasets

- `data/datasets/categorical/` — complete categorical datasets

- `data/datasets/incomplete/` — incomplete numerical datasets

- `data/datasets/incomplete_categorical/` — incomplete categorical datasets

You can inspect dataset properties with:

```r
readRDS("./data/datasets/sets_dim.RDS")
```

#### Note on Categorical Data

- If your method does not support categorical variables, remove such datasets before running the benchmark.

- If your method requires additional preprocessing of categorical columns (e.g., one-hot encoding, ordinal encoding), handle it inside the imputation function.

## 4. Benchmark Parameters

1. Amputation:

- Mechanisms
- Missing ratios
- Replicates

2. Imputation:

- Timeout [in seconds],
- Max attempts on failure,

Parameters are saved in data/params.RDS.

## 5. Running the Benchmark

Edit the file `run.R` according to your needs before running the benchmark. 
Then run 

```sh
Rscript run.R
```


## 6. Output

- Amputed datasets: `./results/amputed/`,
- Imputed datasets: `./results/imputed/`,
- Summary files: `./results/amputation_summary.RDS` and `./results/imputation_summary.RDS`.




