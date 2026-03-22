#!/usr/bin/env Rscript

# --- libraries ---
library(dplyr)
library(stringr)

# source all custom functions
invisible(lapply(list.files("./R", full.names = TRUE, pattern = "\\.R$"), source))

set.seed(56135)

# --- build and save params (must happen before anything else) ---
source("setup.R")
save_params()

# --- load params ---
params <- readRDS("./data/params.RDS")

amputation_params <- params %>%
  select(amputed_id, mechanism, ratio, filepath_original, filepath_amputed, case) %>%
  distinct()

cat("Amputing", nrow(amputation_params), "datasets...\n")

dir.create("./results/amputed", showWarnings = FALSE, recursive = TRUE)

for (i in seq_len(nrow(amputation_params))) {
  row <- amputation_params[i, ]
  
  # incomplete datasets are already missing data — nothing to ampute
  if (row$case %in% c("incomplete", "incomplete_categorical")) {
    cat(sprintf("[%d/%d] Copying as-is (incomplete): %s\n", 
                i, nrow(amputation_params), row$amputed_id))
    dat <- readRDS(row$filepath_original)
    saveRDS(dat, row$filepath_amputed)
    next
  }
  
  # skip if already done
  if (file.exists(row$filepath_amputed)) {
    cat(sprintf("[%d/%d] Already exists, skipping: %s\n", 
                i, nrow(amputation_params), row$amputed_id))
    next
  }
  
  cat(sprintf("[%d/%d] Amputing: %s\n", i, nrow(amputation_params), row$amputed_id))
  
  tryCatch({
    amputed <- ampute_dataset(
      filepath  = row$filepath_original,
      mechanism = row$mechanism,
      ratio     = row$ratio
    )
    saveRDS(amputed, row$filepath_amputed)
  }, error = function(e) {
    cat(sprintf("  ERROR on %s: %s\n", row$amputed_id, conditionMessage(e)))
  })
}

cat("Amputation complete.\n")

# --- write job list for submission ---
writeLines(params$imputed_id, "job_ids.txt")
cat("Written", length(params$imputed_id), "job IDs to job_ids.txt\n")