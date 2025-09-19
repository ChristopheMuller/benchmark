
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

#imputations
library(imputomics)
library(miceDRF)
library(ImputeRobust)
library(mice)
library(glmnet)
library(missForest)
library(MetabImpute)
library(SuperImputer)

# for vis
library(ggplot2)
library(patchwork)

#dummy solution
select <- dplyr::select

# Source custom functions
tar_source()

# options(clustermq.scheduler = "multiprocess")

reticulate::use_virtualenv("./.venv", required = TRUE)

set.seed(56135)

#################################################  PARAMETERS  #################

# timeout value [in seconds]
timeout_thresh <- 36000

# number of attempts in a single run
n_attempts <- 2

# set paths
path_to_amputed <- "./results/amputed/"
path_to_complete_datasets <- "./data/datasets/complete/"
path_to_incomplete_datasets <- "./data/datasets/incomplete/"
path_to_categorical_datasets <- "./data/datasets/categorical/"
path_to_incomplete_categorical_datasets <- "./data/datasets/incomplete_categorical/"
path_to_imputed <- "./results/imputed/"

path_to_results <- "./results/"

path_to_methods <- "./data/functions.RDS"
path_to_cat_methods <- "./data/categorical_funs.RDS"

# amputation setup:
amputation_mechanisms <- c("mcar", "mar")
missing_ratios <- c(0.1, 0.2, 0.3)
amputation_reps <- 2

# imputation methods
imputation_methods <- readRDS(path_to_methods) %>% 
  rename(imputation_fun = `Function name`) %>% 
  mutate(method = str_remove(imputation_fun, "impute_")) %>%
  filter(! (method %in% c("mice_cart50", "mice_cart100", "supersuperimputer", "superimputer", "engressimpute", "engression")))

imputation_categorical <- readRDS(path_to_cat_methods) 

# parameters:
params <- create_params(
  path_to_complete_datasets = path_to_complete_datasets,
  path_to_incomplete_datasets = path_to_incomplete_datasets,
  path_to_categorical_datasets = path_to_categorical_datasets,
  path_to_incomplete_categorical_datasets = path_to_incomplete_categorical_datasets,
  path_to_amputed = path_to_amputed,
  path_to_imputed = path_to_imputed,
  amputation_mechanisms = amputation_mechanisms,
  amputation_reps = amputation_reps,
  missing_ratios = missing_ratios,
  imputation_methods = imputation_methods,
  imputation_categorical = imputation_categorical
)


print(paste0("total dim params: ", dim(params)))
print(table(params$set_id))
print(table(params$mechanism))
print(table(params$rep))
print(table(params$ratio))
print(table(params$method))

print("ATTENTION: ALWAYS SCORE, NEVER IMPUTE !!!")

# saveRDS(params, "./data/params.RDS")

amputation_params <- params %>% 
  select(amputed_id, mechanism, ratio, filepath_original, filepath_amputed) %>% 
  unique()

imputation_params <- params %>% 
  select(imputed_id, amputed_id, filepath_amputed, imputation_fun, 
         filepath_imputed, MI, filepath_original, case, var_type) %>% 
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
    missing_data <- readRDS(filepath_amputed)
      impute(
        dataset_id = imputed_id,
        missing_data_set = missing_data,
        imputing_function = imputation_fun,
        timeout_thresh = timeout_thresh,
        n_attempts = n_attempts,
        var_type = var_type,
        case = case
      )
    },
    # cue = tar_cue(depend = FALSE),
    cue = tar_cue(mode = "never")
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
                       case = case, var_type = var_type)
    },
    # cue = tar_cue(depend = FALSE),
    cue = tar_cue(mode = "always"),
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
