
# ------------------------------------------------------------------------------
# This code is part of the research project accompanying the article:
# 
# Grzesiak, K., Muller, C., Josse, J., & Näf, J. (2025).
# "Do we Need Dozens of Methods for Real World Missing Value Imputation?"
# arXiv preprint arXiv:2511.04833.
#
# If you use this code for benchmarking, comparison, or as part of published
# research, please cite the above paper.
# ------------------------------------------------------------------------------

# options(warn = 0)  # Re-enable warnings

library(targets)
options(clustermq.scheduler = "multiprocess")
library(tarchetypes)
library(purrr)
library(dplyr)
library(stringr)
library(energy)
library(reticulate)
library(tidyr)
library(clustermq)
library(parallel)

# for vis
library(ggplot2)
library(patchwork)

################################################## Your first steps? ###########

# If you're just starting, please, run the line below to install all the 
# dependencies of the benchmark.
#
# renv::restore()
#
# Note: It's just some packages installation - you don't have to do this step 
# every time.


################################################################################
################################## BENCHMARK FUNCTIONS - DO NOT CHANGE  ########
################################################################################

tar_source() # loading source functions for benchmark

################################################################################
#############################################  YOUR CUSTOM IMPUTATIONS  ########
################################################################################

# -------------------------------------------------------------- PYTHON? -------

# Python integration (optional)
#
# If you don't want to use Python, you can ignore this section.
#
# To use Python functions, first set up a virtual environment in "./.venv".
# Depending on your system, run:
#   system("./python/windows_setup.sh")  # on Windows
#   system("./python/linux_setup.sh")    # on Linux/macOS
#
# After setting up the virtual environment, activate it with:
# reticulate::use_virtualenv("./venv", required = TRUE)
# reticulate::use_
#
# reticulate::py_config() # check if python path is correct
#
# Check here if pip works for venv. Run the following lines
# reticulate::py_run_string("import sys; print(sys.executable)")
# reticulate::py_run_string("import pip; print(pip.__version__)")

# To check project environment run:
# check_project_environment()

# ------------------------------------------------------------- IMPUTATION -----
# 
# If you would like to test custom imputation methods, place each method in a 
# separate file inside the ./my_methods/ directory. The file name should match 
# the method name.
#
# The imputation function must be named using the prefix "impute_".
# For example, for a method called "nice_imputation":
#   - the file should be named "nice_imputation.R"
#   - the imputation function should be named "impute_nice_imputation"
#
# Files may contain additional helper functions with arbitrary names.
# However, only the function whose name starts with "impute_" will be used 
# as the imputation method.
#
# To validate custom imputation methods in ./my_methods/. please run:
validate_my_methods()
#
# Methods found :
collect_my_methods() # check if all the methods you'd like to run are here


# PREPARE ENVIRONMENT ----------------------------------------------------------

# The following function will source all the imputation dependencies in a separate
# R session for stability. Do not change it unless you're using python

load_imputations_env <- function() {
  
  # source benchmark functions
  targets::tar_source()
  # source R methods
  source_my_methods()
  
  # source python methods
  # source("python/python_imputation_functions.R") # uncomment for python
  
}

# ============ PYTHON (OPTIONAL) ===============================================

# If you're using python, place the imputation functions in the files:
#
# - python_imputation_functions.R  # R wrapper (following the guidline above)
# - python_imputation_functions.py # python imputation

# Place all functions in these two files. Make sure that the R wrappers 
# containing the imputation methods follow the specified conventions (as shown 
# in the examples) and that their names start with "impute_".

# If everything is correct, you should see all the python methods after running:
# collect_python_methods()

# If everything is correct, change the object below to TRUE :)
use_python_imputations <- FALSE

################################################################################
#################################################  PARAMETERS  #################
################################################################################

## You can play with the benchmark setup below

# -------------------------------------------------------------- AMPUTATION ----

amputation_mechanisms <- c("mcar", "mar")   # missingness mechanisms

missing_ratios <- c(0.1, 0.2, 0.3)          # proportion of values to ampute

amputation_reps <- 2                        # replicates for amputation

# -------------------------------------------------------------- IMPUTATION ----

timeout_thresh <- 36000                     # timeout value [in seconds]

# How many attempts does the imputation method get in case of a failure
n_attempts <- 2                             # number of attempts in a single run

# -------------------------------------------------------------- EVALUATION ----

# COMPLETE NUMERICAL DATASETS
# 
# Available scores:
# "mae" – mean absolute error
# "rmse" – root mean squared error
# "nrmse" – normalized RMSE
# "energy" – energy distance
# "energy_std" – standardized energy distance 
# "feature_wise_wasserstein" – per-feature Wasserstein distance
# "KLD" – Kullback–Leibler divergence
# "entropic_wasserstein" – entropy-regularized Wasserstein distance
# "sliced_wasserstein" – projection-based Wasserstein distance
# 
# Scores are provided as a character vector, e.g.:

scores <- c("mae", "energy")

# !!! In case of no scores selected, "energy" will be calculated by default.

# NOTE:
# Some scores (especially Wasserstein-based and KLD) can be computationally 
# expensive. Choose carefully depending on dataset size and simulation budget.

# COMPLETE CATEGORICAL DATASETS
# For complete categorical datasets we automatically compute "energy" and 
# "energy_std".
# NOTE:
# For categorical variables, energy score is calculated based on one-hot encoding.


# INCOMPLETE DATASETS:
# For incomplete datasets, we compute energy-I-Score. For more details check the 
# reference:
#
#           Näf, J., Grzesiak, K., and Scornet, E. (2025a)
#     How to rank imputation methods? arXiv preprint arXiv:2507.11297 
#            (https://doi.org/10.48550/arXiv.2507.11297)


# -------------------------------------------------------------- DATASETS ------

# The datasets are stored in the "data/datasets/" directory.
# Below we list all datasets available for each case.
# You may redefine the vectors depending on the experiment.

# ------------------------------
# COMPLETE – NUMERICAL
# - airfoil_self_noise.RDS, 
# - allergens.RDS, 
# - concrete.RDS,
# - enb.RDS, 
# - fat.RDS, 
# - scm1d.RDS, 
# - scm20d.RDS,
# - windspeed.RDS, 
# - yeast.RDS
#
# Example selection:
complete_numerical <- c("airfoil_self_noise.RDS", "fat.RDS")

# ---------------------------------

# COMPLETE – CATEGORICAL / MIXED:
# - choccake.RDS, 
# - diamond.RDS, 
# - electricity.RDS,
# - eye_movement.RDS, 
# - german.RDS, 
# - nels88.RDS,
# - PimaIndiansDiabetes.RDS, 
# - worldcup.RDS
#
# Example selection:
complete_categorical <- c("choccake.RDS", "german.RDS")

# -------------------------------

# INCOMPLETE – NUMERICAL:
# - diabetes.RDS, 
# - globwarm.RDS, 
# - oceanbuoys.RDS,
# - popmis.RDS, 
# - pulplignin.RDS
#
# Example selection:
incomplete_numerical <- c("diabetes.RDS", "popmis.RDS")

# ----------------------------

# INCOMPLETE – CATEGORICAL / MIXED:
# - boys.RDS, 
# - colic_again.RDS, 
# - debt.RDS,
# - housevotes84.RDS, 
# - selfreport.RDS, soybean.RDS,
# - tbc.RDS, 
# - vnf.RDS, 
# - walking.RDS
#
# Example selection:
incomplete_categorical <- c("boys.RDS")

# ------------------

# To see the dimensions of all datasets, run:
#   readRDS("./data/datasets/sets_dim.RDS")

# NOTE ON CATEGORICAL DATA
#
# If your imputation method does not support categorical variables,
# please remove datasets containing categorical features before running
# the benchmark.
#
# If your method requires additional preprocessing of categorical columns
# (e.g., one-hot encoding, ordinal encoding, or other transformations),
# make sure to perform this preprocessing inside your imputation function.
#
# Any required data transformations should be handled internally by the
# method implementation.

#======================= That's it! Enjoy your imputation! =====================

# To run the benchmark you can run the ---run.R--- script. Don't hesitate to 
# change the number of workers depending on your machine!

################################################################################
######################## SIMULATION STARTS HERE ################################
##################### DO NOT CHANGE THE CODE BELOW ############################# 
################################################################################

# set random seed
set.seed(56135)

# set paths
path_to_amputed <- "./results/amputed/"
path_to_imputed <- "./results/imputed/"
path_to_results <- "./results/"

# PREPARE DATASETS -------------------------------------------------------------

if(!exists("complete_numerical")) complete_numerical <- character(0) 
if(!exists("complete_categorical")) complete_categorical <- character(0) 
if(!exists("incomplete_numerical")) incomplete_numerical <- character(0) 
if(!exists("incomplete_categorical")) incomplete_categorical <- character(0) 

if(length(complete_numerical) > 0)
  complete_numerical <- paste0("./data/datasets/complete/", 
                               complete_numerical)

if(length(complete_categorical) > 0)
  complete_categorical <- paste0("./data/datasets/categorical/", 
                                 complete_categorical)

if(length(incomplete_numerical) > 0)
  incomplete_numerical <- paste0("./data/datasets/incomplete/", 
                                 incomplete_numerical)

if(length(incomplete_categorical) > 0)
  incomplete_categorical <- paste0("./data/datasets/incomplete_categorical/", 
                                   incomplete_categorical)

# CHECK SCORES -----------------------------------------------------------------

if(!exists("scores") || length(scores) == 0) scores <- "energy"

allowed_scores <- c("mae", "rmse", "nrmse", "energy", "energy_std",
                    "feature_wise_wasserstein", "KLD", "entropic_wasserstein", 
                    "sliced_wasserstein")

invalid_scores <- setdiff(scores, allowed_scores)
valid_scores <- setdiff(scores, invalid_scores)

if (length(invalid_scores) > 0)  {
  warning(sprintf("Invalid score(s): %s\nAllowed scores are: %s",
                  paste(invalid_scores, collapse = ", "),
                  paste(allowed_scores, collapse = ", ")), call. = FALSE)
}

if(length(valid_scores) == 0) scores <- "energy"


# PREPARE IMPUTATIONS ----------------------------------------------------------

# Source all custom imputation method files
source_my_methods()

# collect custom imputation methods
imputation_methods <- collect_my_methods()

# check python methods:
if(use_python_imputations) {
  source_python_methods()
  imputation_methods <- rbind(imputation_methods, collect_python_methods())
} else {
  message("No python usage in this benchmark!")
}


if(length(c(incomplete_numerical, incomplete_categorical)) > 0) {
  imputation_methods <- check_mi(imputation_methods)
} else {
  imputation_methods <- imputation_methods %>% mutate(MI = NA)
}


# PREPARE PARAMETERS -----------------------------------------------------------

# prepare simulation parameters
params <- create_params(
  complete_numerical = complete_numerical,
  complete_categorical = complete_categorical,
  incomplete_numerical = incomplete_numerical,
  incomplete_categorical = incomplete_categorical,
  path_to_amputed = path_to_amputed,
  path_to_imputed = path_to_imputed,
  amputation_mechanisms = amputation_mechanisms,
  amputation_reps = amputation_reps,
  missing_ratios = missing_ratios,
  imputation_methods = imputation_methods
)

saveRDS(params, "./data/params.RDS")

amputation_params <- params %>% 
  select(amputed_id, mechanism, ratio, filepath_original, filepath_amputed) %>% 
  unique()

imputation_params <- params %>% 
  select(imputed_id, amputed_id, filepath_amputed, imputation_fun, 
         filepath_imputed, MI, filepath_original, case) %>% 
  unique()

#################################################  AMPUTATION  #################

amputed_datasets <- tar_map(
  values = amputation_params,
  names = any_of("amputed_id"),
  tar_target(amputed_dat, 
             ampute_dataset(filepath = filepath_original,
                            mechanism = mechanism,
                            ratio = ratio), 
             cue = tar_cue_skip(1 > 0)),
  tar_target(save_amputed_dat,
             saveRDS(amputed_dat, filepath_amputed))
)

#################################################  IMPUTATION  #################

imputed_datasets <- tar_map(
  values = imputation_params,
  names = any_of("imputed_id"),
  tar_target(
    imputed_dat, {
      missing_data <- amputed_all[[paste0("amputed_dat_", amputed_id)]]
      impute(
        dataset_id = imputed_id,
        missing_data_set = missing_data,
        imputing_function = imputation_fun,
        timeout_thresh = timeout_thresh,
        n_attempts = n_attempts,
        case = case,
        load_imputations_env = load_imputations_env()
      )
    }
  ),
  tar_target(save_imputed_dat,
             saveRDS(imputed_dat[["imputed"]], filepath_imputed)
  ),
  tar_target(
    obtained_scores, {
      missing_data <- readRDS(filepath_amputed)
      calculate_scores(imputed = imputed_dat, 
                       amputed = missing_data,
                       imputation_fun = get(imputation_fun),
                       multiple = MI,
                       imputed_id = imputed_id, 
                       timeout_thresh = timeout_thresh,
                       filepath_original = filepath_original,
                       case = case,
                       scores = scores)
    }
  )
)

#################################################  TARGETS  ####################


list(
  # AMPUTATION
  amputed_datasets,
  tar_combine(amputed_all,
              amputed_datasets[["amputed_dat"]],
              command = list(!!!.x)),
  tar_target(amputation_summary,
             summarize_amputation(amputed_all, params)),
  tar_target(save_amputation_summary, {
    saveRDS(amputation_summary, 
            paste0(path_to_results, "amputation_summary.RDS"))
  }),
  
  # IMPUTATION
  imputed_datasets,
  tar_combine(all_scores,
              imputed_datasets[["obtained_scores"]],
              command = bind_rows(list(!!!.x))),
  
  tar_target(imputation_summary, {
    summarize_imputations(all_scores, params)
  }),
  
  tar_target(save_imputation_summary, {
    saveRDS(imputation_summary, 
            paste0(path_to_results, "imputation_summary.RDS"))
  })
  
  # ANALYSIS
  # nice code here
)
