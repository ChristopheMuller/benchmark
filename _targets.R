
options(warn = -1)  # Turn off warnings
# options(warn = 0)  # Re-enable warnings

library(targets)
library(tarchetypes)
library(purrr)
library(dplyr)
library(imputomics)
library(miceDRF)
library(ImputeRobust)
library(stringr)
library(energy)
library(reticulate)
library(tidyr)
library(mice)

# for vis
library(ggplot2)
library(patchwork)

# Source custom functions
tar_source()

options(clustermq.scheduler = "multiprocess")

tar_option_set(
  resources = list(
    RETICULATE_PYTHON = "./.venv/bin/python"
  )
)

set.seed(56135)

# set paths
path_to_amputed <- "./results/amputed/"
path_to_complete_datasets <- "./data/datasets/complete/"
path_to_incomplete_datasets <- "./data/datasets/incomplete/"
path_to_imputed <- "./results/imputed/"

path_to_methods <- "./data/functions.RDS"

# amputation setup:
# amputation_mechanisms <- c("classic_mar", "dist_shift", "classic_mcar")
amputation_mechanisms <- c("classic_mar")
missing_ratios <- c(0.3)
amputation_reps <- 1

# imputation methods
# imputation_funs <- readRDS(path_to_methods)[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 25)]
imputation_funs <- readRDS(path_to_methods)

imputation_methods <- data.frame(method = str_remove(imputation_funs, "impute_"),
                                 imputation_fun = imputation_funs) %>% 
  filter(method != "mice_gamlss")

# parameters:
params <- create_params(path_to_complete_datasets = path_to_complete_datasets,
                        path_to_incomplete_datasets = path_to_incomplete_datasets,
                        path_to_amputed = path_to_amputed,
                        path_to_imputed = path_to_imputed,
                        amputation_mechanisms = amputation_mechanisms,
                        amputation_reps = amputation_reps,
                        missing_ratios = missing_ratios,
                        imputation_methods = imputation_methods)


saveRDS(params, "./data/params.RDS")

amputation_params <- params %>% 
  select(amputed_id, mechanism, ratio, filepath_original, filepath_amputed) %>% 
  unique()



imputation_params <- params %>% 
  select(imputed_id, amputed_id, imputation_fun, filepath_imputed) %>% 
  unique()

# define static branches

amputed_datasets <- tar_map(
  values = amputation_params,
  names = any_of("amputed_id"),
  tar_target(amputed_dat, 
             ampute_dataset(filepath = filepath_original,
                            mechanism = mechanism,
                            ratio = ratio)),
  tar_target(save_amputed_dat,
             saveRDS(amputed_dat, filepath_amputed))
)


imputed_datasets <- tar_map(
  values = imputation_params,
  names = any_of("imputed_id"),
  tar_target(
    imputed_dat, 
    impute(
      dataset_id = imputed_id,
      missing_data_set = amputed_all[[paste0("amputed_dat_", amputed_id)]], 
      imputing_function = get(imputation_fun),
      timeout = 600, # time in seconds
      n_attempts = 3
    )
  ),
  tar_target(save_imputed_dat,
             saveRDS(imputed_dat[["imputed"]], filepath_imputed)
  )
)


list(
  # AMPUTATION
  amputed_datasets,
  tar_combine(amputed_all,
              amputed_datasets[["amputed_dat"]],
              command = list(!!!.x)),
  tar_target(amputation_summary,
             summarize_amputation(amputed_all, params)),
  
  # IMPUTATION
  imputed_datasets,
  tar_combine(imputed_all,
              imputed_datasets[["imputed_dat"]],
              command = list(!!!.x)),

  tar_target(imputation_summary,
             summarize_imputations(imputed_all, params))
  
  # ANALYSIS
  # nice code here
)
