# ==============================================================================
# USER SETTINGS 
# ==============================================================================
PATH_FIGURES    <- "./figures/"       
CASE_TO_PLOT    <- "complete"         
MEASURE_TO_PLOT <- "energy_std"            

if (!dir.exists(PATH_FIGURES)) dir.create(PATH_FIGURES, recursive = TRUE)

# ==============================================================================
# LIBRARIES & DATA LOADING
# ==============================================================================
library(tidyverse)
library(tidytext)
library(patchwork)
source("R/visualization.R") # Loads your plotting functions and color palettes


# Load New Results
imputation_summary_new <- readRDS("results/imputation_summary.RDS")  %>% 
  filter(case == CASE_TO_PLOT) %>%
  filter(measure == MEASURE_TO_PLOT)
imputation_summary_new$new <- TRUE

# Load Benchmark Results

imputation_summary_benchmark <- readRDS("results/imputation_summary_benchmark.RDS")
imputation_summary_benchmark$new <- FALSE
imputation_summary_benchmark <- imputation_summary_benchmark %>% 
  filter(measure == MEASURE_TO_PLOT) %>% 
  filter(case %in% unique(imputation_summary_new$case)) %>%
  filter(ratio %in% unique(imputation_summary_new$ratio)) %>%
  filter(mechanism %in% unique(imputation_summary_new$mechanism))


# Combine and Filter
imputation_summary <- rbind(imputation_summary_benchmark, imputation_summary_new) %>% 
  filter(case == CASE_TO_PLOT) %>%
  filter(measure == MEASURE_TO_PLOT) %>% 
  filter(set_id %in% unique(imputation_summary_new$set_id)) %>% 
  filter(mechanism %in% unique(imputation_summary_new$mechanism)) %>% 
  filter(ratio %in% unique(imputation_summary_new$ratio))

# raise error if no data to plot
if (nrow(imputation_summary) == 0) {
  stop("No data available for the specified case and measure. Please check your filters.")
}

# ==============================================================================
# GENERATE AND SAVE PLOTS
# ==============================================================================

# 1. Error Analysis Plot
p_errors <- plot_error_analysis(imputation_summary)
ggsave(paste0(PATH_FIGURES, "error_analysis.pdf"), p_errors, width = 12, height = 8)

# 2. Shrek's Heatmap (Aggregated Ranking)
p_heatmap <- plot_ranking_heatmap(imputation_summary)
ggsave(paste0(PATH_FIGURES, "heatmap_ranking.pdf"), p_heatmap, width = 10, height = 12)

# 3. Energy vs Time Ranking (Boxplots)
p_final <- plot_energy_time_ranking(imputation_summary, c(0, 1, 40, 80, 99, 100))
ggsave(paste0(PATH_FIGURES, "energy_time_ranking.pdf"), p_final, width = 14, height = 10)

# ==============================================================================
cat("Done! Figures saved in:", PATH_FIGURES, "\n")
