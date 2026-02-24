
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
#   reticulate::use_virtualenv("./.venv", required = TRUE)


# ------------------------------------------------------------ ENVIRONMENT -----

# Set up the environment for imputation
#
# Load required R packages and any other dependencies needed for your imputation 
# methods as an expression named "load_imputations_env".
# 
# Example: loading the 'mice' package

load_imputations_env <- {
  
  library(mice)  # example
  
  # If you want to use Python-based imputation functions, run the following line:
  # source("python/python_imputation_functions.R")
  
}


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
#
# To validate custom imputation methods in ./my_methods/. please run:
validate_my_methods()
#
# Methods found :
collect_my_methods() # check if all the methods you'd like to run are here


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




# -------------------------------------------------------------- DATASETS ------

# The datasets are stored in the "data/datasets/" directory.
# You can inspect their properties and update the following vectors as needed.

complete_numerical <- c("airfoil_self_noise.RDS", "allergens.RDS", "concrete.RDS", 
                        "enb.RDS", "fat.RDS", "scm1d.RDS", "scm20d.RDS", 
                        "windspeed.RDS", "yeast.RDS")

complete_categorical <- c("choccake.RDS", "diamond.RDS", "electricity.RDS", 
                          "eye_movement.RDS", "german.RDS", "nels88.RDS", 
                          "PimaIndiansDiabetes.RDS", "worldcup.RDS")

incomplete_numerical <- c("diabetes.RDS", "globwarm.RDS", "oceanbuoys.RDS", 
                          "popmis.RDS", "pulplignin.RDS")

incomplete_categorical <- c("boys.RDS", "colic_again.RDS", "debt.RDS", 
                            "housevotes84.RDS", "selfreport.RDS", "soybean.RDS", 
                            "tbc.RDS", "vnf.RDS", "walking.RDS")

# To see the dimensions of all datasets, run:
#   readRDS("./data/datasets/sets_dim.RDS")

# ------------------------------------------------------------------------------
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
# ------------------------------------------------------------------------------

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

complete_numerical <- paste0("./data/datasets/complete/", 
                             complete_numerical)
complete_categorical <- paste0("./data/datasets/categorical/", 
                               complete_categorical)
incomplete_numerical <- paste0("./data/datasets/incomplete/", 
                               incomplete_numerical)
incomplete_categorical <- paste0("./data/datasets/incomplete_categorical/", 
                                 incomplete_categorical)

# PREPARE IMPUTATIONS ----------------------------------------------------------

# Source all custom imputation method files
source_my_methods()

# collect custom imputation methods
imputation_methods <- collect_my_methods()

if(length(c(incomplete_numerical, incomplete_categorical)) > 0) {
  imputation_methods <- check_mi(imputation_methods)
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
        load_imputations_env = load_imputations_env
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
                       case = case)
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
