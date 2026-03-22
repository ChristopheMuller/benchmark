#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(yaml)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
})

source("R/checkpoint.R")
source("R/ampute.R")
source("R/impute.R")
source("R/score.R")

# ── 1. Parse config ────────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)
config_path <- if (length(args) >= 1) args[1] else "configs/default.yaml"
cfg <- yaml::read_yaml(config_path)

message("=== Pipeline starting ===")
message("Config: ", config_path)
message("Datasets:   ", paste(basename(cfg$datasets), collapse = ", "))
message("Methods:    ", paste(cfg$methods, collapse = ", "))
message("Mechanisms: ", paste(cfg$mechanisms, collapse = ", "))
message("Ratios:     ", paste(cfg$ratios, collapse = ", "))
message("Reps:       ", cfg$reps)

# ── 2. Build parameter grid ────────────────────────────────────────────────────

amputation_grid <- expand.grid(
  filepath_original = cfg$datasets,
  mechanism         = cfg$mechanisms,
  ratio             = cfg$ratios,
  rep               = seq_len(cfg$reps),
  stringsAsFactors  = FALSE
) %>%
  mutate(
    set_id     = tools::file_path_sans_ext(basename(filepath_original)),
    amputed_id = paste(set_id, mechanism, ratio, rep, sep = "_"),
    filepath_amputed = file.path(cfg$paths$amputed,
                                 paste0(amputed_id, ".rds"))
  )

imputation_grid <- amputation_grid %>%
  tidyr::crossing(method = cfg$methods) %>%
  mutate(
    imputed_id       = paste(amputed_id, method, sep = "_"),
    imputation_fun   = paste0("impute_", method),
    filepath_imputed = file.path(cfg$paths$imputed,
                                 paste0(imputed_id, ".rds")),
    filepath_scores  = file.path(cfg$paths$scores,
                                 paste0(imputed_id, "_scores.rds"))
  )

message(sprintf("\nGrid: %d amputation jobs × %d methods = %d imputation jobs\n",
                nrow(amputation_grid),
                length(cfg$methods),
                nrow(imputation_grid)))

# ── 3. Amputation loop ─────────────────────────────────────────────────────────

message("── AMPUTATION ──────────────────────────────────────────")

amputed_cache <- list()

for (i in seq_len(nrow(amputation_grid))) {
  row <- amputation_grid[i, ]
  message(sprintf("[%d/%d] %s", i, nrow(amputation_grid), row$amputed_id))
  
  result <- with_checkpoint(
    path = row$filepath_amputed,
    expr = ampute_dataset(
      filepath  = row$filepath_original,
      mechanism = row$mechanism,
      ratio     = row$ratio
    )
  )
  
  amputed_cache[[row$amputed_id]] <- result
}

# ── 4. Imputation + scoring loop ───────────────────────────────────────────────

message("\n── IMPUTATION & SCORING ────────────────────────────────")

all_scores <- list()

for (i in seq_len(nrow(imputation_grid))) {
  row <- imputation_grid[i, ]
  message(sprintf("[%d/%d] %s", i, nrow(imputation_grid), row$imputed_id))
  
  # Retrieve amputed data (from cache or disk)
  amputed_dat <- amputed_cache[[row$amputed_id]]
  if (is.null(amputed_dat) && file.exists(row$filepath_amputed)) {
    amputed_dat <- readRDS(row$filepath_amputed)
  }
  if (is.null(amputed_dat)) {
    message("  [SKIP] Amputed data missing, cannot impute.")
    next
  }
  
  # Imputation — checkpointed
  imputed_dat <- with_checkpoint(
    path = row$filepath_imputed,
    expr = impute(
      dataset_id       = row$imputed_id,
      missing_data_set = amputed_dat,
      imputing_function = row$imputation_fun,
      timeout_thresh   = cfg$timeout_thresh,
      n_attempts       = cfg$n_attempts,
      var_type         = row$var_type %||% "continuous",
      case             = row$case %||% "standard"
    )
  )
  
  if (is.null(imputed_dat)) next
  
  # Scoring — checkpointed separately (useful if scoring logic changes)
  scores <- with_checkpoint(
    path = row$filepath_scores,
    expr = calculate_scores(
      imputed          = imputed_dat,
      amputed          = amputed_dat,
      imputation_fun   = get(row$imputation_fun),
      multiple         = isTRUE(row$MI),
      imputed_id       = row$imputed_id,
      timeout_thresh   = cfg$timeout_thresh,
      filepath_original = row$filepath_original,
      case             = row$case %||% "standard",
      var_type         = row$var_type %||% "continuous"
    )
  )
  
  all_scores[[row$imputed_id]] <- scores
}

# ── 5. Combine & save summary ──────────────────────────────────────────────────

message("\n── SUMMARY ─────────────────────────────────────────────")

# Collect ALL score files matching this config's grid (not just this run's cache)
score_files <- imputation_grid$filepath_scores
score_files  <- score_files[file.exists(score_files)]
message(sprintf("Collecting %d score files...", length(score_files)))

combined_scores <- purrr::map_dfr(score_files, readRDS)

out_file <- file.path(
  cfg$paths$results,
  paste0("summary_", tools::file_path_sans_ext(basename(config_path)), ".rds")
)
saveRDS(combined_scores, out_file)
message("Summary saved: ", out_file)

message("\n=== Done ===")