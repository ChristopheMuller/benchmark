#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1)
this_imputed_id <- args[1]

# --- libraries ---
library(dplyr)
library(stringr)

# imputation libraries (same as _targets.R)
library(imputomics); library(miceDRF); library(ImputeRobust)
library(mice); library(glmnet); library(missForest); library(MetabImpute)
library(reticulate)
reticulate::use_virtualenv("./.venv", required = TRUE)

# source all custom functions (same as tar_source())
invisible(lapply(list.files("./R", full.names = TRUE, pattern = "\\.R$"), source))

set.seed(56135)

# --- config ---
timeout_thresh <- 36000
n_attempts     <- 2

# --- params ---
# source("setup.R")
# save_params()
params <- readRDS("./data/params.RDS")

job <- params %>%
  filter(imputed_id == this_imputed_id)

stopifnot(nrow(job) == 1)

# --- read amputed data ---
amputed_dat <- readRDS(job$filepath_amputed)

# --- impute ---
imputed_dat <- impute(
  dataset_id        = job$imputed_id,
  missing_data_set  = amputed_dat,
  imputing_function = job$imputation_fun,
  timeout_thresh    = timeout_thresh,
  n_attempts        = n_attempts,
  var_type          = job$var_type,
  case              = job$case
)

# --- save imputed ---
saveRDS(imputed_dat[["imputed"]], job$filepath_imputed)

# --- score ---
scores <- calculate_scores(
  imputed           = imputed_dat,
  amputed           = amputed_dat,
  imputation_fun    = get(job$imputation_fun),
  multiple          = job$MI,
  imputed_id        = job$imputed_id,
  timeout_thresh    = timeout_thresh,
  filepath_original = job$filepath_original,
  case              = job$case,
  var_type          = job$var_type
)

# --- save scores ---
dir.create("./results/scores", showWarnings = FALSE, recursive = TRUE)
saveRDS(scores, paste0("./results/scores/", this_imputed_id, ".RDS"))

cat("Done:", this_imputed_id, "\n")