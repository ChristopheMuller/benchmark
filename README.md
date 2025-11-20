
# Benchmarking Missing Data Imputation Methods

This repository contains the full pipeline for benchmarking a wide range of missing data imputation methods on both numerical and mixed-type datasets, as presented in our study.

### Repository Structure

``` graphql
.
├── data/                   # Raw and preprocessed datasets & simulation parameters
├── R/                      # Core R scripts for simulation, evaluation and imputation
├── python/                 # Core python scripts for imputation
├── renv/                   # R environment snapshot (created with renv)
├── results/                # Summary files, additional analysis
├── output/                 # Timings, logs
├── figures/                # Final figures used in the manuscript
├── README.md               # This file
└── renv.lock               # Exact package versions used

```


### Overview

We evaluate 74 imputation methods under various missingness mechanisms (MCAR, MAR, MNAR), missingness ratios, and data types. The pipeline is modular, reproducible, and includes:

- Support for numerical and mixed-type datasets,

- Evaluation using multiple metrics (e.g., standardized Energy distance),

- Recording of runtime and stability (success/failure),

- Aggregation and visualization of results.

### Installation

1. Clone the repository:

``` bash
git clone https://github.com/ChristopheMuller/benchmark
cd benchmark
```

2. Open R and restore the environment using `renv`:

```r
install.packages("renv")
renv::restore()
```
3, ...

### Citation

If you use this code or findings in your own work, please cite:

Grzesiak, K., Muller, C., Josse, J., & Näf, J. (2025). Do we Need Dozens of Methods for Real World Missing Value Imputation?. arXiv preprint arXiv:2511.04833.

