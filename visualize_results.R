# ==============================================================================
# USER SETTINGS 
# ==============================================================================
PATH_FIGURES    <- "./figures/"       
CASE_TO_PLOT    <- "complete"         
MEASURE_TO_PLOT <- "energy"           
ARRANGE_SUCCESS <- TRUE               
SUCCESS_BREAKS  <- c(0, 1, 40, 80, 99, 100)

if (!dir.exists(PATH_FIGURES)) dir.create(PATH_FIGURES, recursive = TRUE)

# ==============================================================================
# LIBRARIES & DATA LOADING
# ==============================================================================
library(tidyverse)
library(tidytext)
library(patchwork)
source("R/visualization.R") # Loads your plotting functions and color palettes

# Load Benchmark Results
summary_files <- list(
  num_comp   = "results/imputation_summary_complete_numerical.RDS",
  num_incomp = "results/imputation_summary_incomplete_numerical.RDS",
  cat_comp   = "results/imputation_summary_complete_categorical.RDS",
  cat_incomp = "results/imputation_summary_incomplete_categorical.RDS"
)

imputation_summary_benchmark <- map_dfr(summary_files, readRDS)
imputation_summary_benchmark$new <- FALSE

# Load New Results
imputation_summary_new <- readRDS("results/imputation_summary.RDS")
imputation_summary_new$new <- TRUE

# Combine and Filter
imputation_summary <- rbind(imputation_summary_benchmark, imputation_summary_new) %>% 
  filter(set_id %in% unique(imputation_summary_new$set_id)) %>% 
  filter(mechanism %in% unique(imputation_summary_new$mechanism)) %>% 
  filter(ratio %in% unique(imputation_summary_new$ratio))

# ==============================================================================
# GENERATE AND SAVE PLOTS
# ==============================================================================

# 1. Error Analysis Plot
p_errors <- plot_error_analysis(imputation_summary)
ggsave(paste0(PATH_FIGURES, "error_analysis.pdf"), p_errors, width = 12, height = 8)

# 2. Shrek's Heatmap (Aggregated Ranking)
p_heatmap <- plot_ranking_heatmap(imputation_summary, CASE_TO_PLOT, MEASURE_TO_PLOT)
ggsave(paste0(PATH_FIGURES, "heatmap_ranking.pdf"), p_heatmap, width = 10, height = 12)

# 3. Energy vs Time Ranking (Boxplots)
p_final <- plot_energy_time_ranking(imputation_summary, MEASURE_TO_PLOT, SUCCESS_BREAKS)
ggsave(paste0(PATH_FIGURES, "energy_time_ranking.pdf"), p_final, width = 14, height = 10)

# ==============================================================================
cat("Done! Figures saved in:", PATH_FIGURES, "\n")
