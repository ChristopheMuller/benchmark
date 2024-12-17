
# options(warn = 0)  # Re-enable warnings

# print current time
print(Sys.time())

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
library(glmnet)
library(clustermq)

# for vis
library(ggplot2)
library(patchwork)

# Source custom functions
tar_source()

options(clustermq.scheduler = "multiprocess")

reticulate::use_virtualenv("./.venv", required = TRUE)


set.seed(56135)

# set paths
path_to_amputed <- "./results/amputed/"
path_to_complete_datasets <- "./data/datasets/complete/"
path_to_incomplete_datasets <- "./data/datasets/incomplete/"
path_to_imputed <- "./results/imputed/"

path_to_methods <- "./data/functions.RDS"

# amputation setup:
amputation_mechanisms <- c("mar", "mcar")
missing_ratios <- c(0.1, 0.3, 0.5)
amputation_reps <- 5

# imputation methods
imputation_methods <- readRDS(path_to_methods) %>% 
  rename(imputation_fun = `Function name`) %>% 
  mutate(method = str_remove(imputation_fun, "impute_")) %>% 
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
  select(imputed_id, amputed_id, imputation_fun, filepath_imputed, MI, 
         filepath_original, case) %>% 
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
    imputed_dat, {
      source("python/python_imputation_functions.R")
      impute(
        dataset_id = imputed_id,
        missing_data_set = amputed_all[[paste0("amputed_dat_", amputed_id)]], 
        imputing_function = get(imputation_fun),
        timeout = 600, # time in seconds
        n_attempts = 3
      )
    }
  ),
  tar_target(save_imputed_dat,
             saveRDS(imputed_dat[["imputed"]], filepath_imputed)
  ),
  tar_target(
    obtained_scores, {
      calculate_scores(imputed = imputed_dat, 
                       amputed = amputed_all[[paste0("amputed_dat_", amputed_id)]],
                       imputation_fun = get(imputation_fun),
                       multiple = MI,
                       imputed_id = imputed_id, 
                       timeout = 600,
                       filepath_original = filepath_original,
                       case = case)
    }
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
  tar_combine(all_scores,
              imputed_datasets[["obtained_scores"]],
              command = bind_rows(list(!!!.x))),
  
  tar_target(imputation_summary, {
    summarize_imputations(all_scores, params)
  })
  # ANALYSIS
  # nice code here
)
