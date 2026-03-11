# ==============================================================================
# USER SETTINGS 
# ==============================================================================
PATH_FIGURES           <- "./figures/"       
MEASURE_COMPLETE       <- "energy_std" 
MEASURE_INCOMPLETE_NUM <- "IScore"
MEASURE_INCOMPLETE_CAT <- "IScore_cat"

DATA_TYPE       <- "num" # Options: "num", "mixed", "ALL"
DATA_STATUS     <- "complete" # Options: "complete", "incomplete", "ALL"

if (!dir.exists(PATH_FIGURES)) dir.create(PATH_FIGURES, recursive = TRUE)

# ==============================================================================
# LIBRARIES & CONFIGURATION
# ==============================================================================
library(tidyverse)
library(tidytext)
library(patchwork)
source("R/visualization.R")

# Map user inputs to file suffixes and internal dataset 'case' labels
types_to_load <- if (DATA_TYPE == "ALL") c("num", "mixed") else DATA_TYPE
statuses_to_load <- if (DATA_STATUS == "ALL") c("complete", "incomplete") else DATA_STATUS

target_cases <- c()
for (t in types_to_load) {
  for (s in statuses_to_load) {
    case_label <- paste(t, s, sep = "_")
    target_cases <- c(target_cases, case_label)
  }
}

target_dict <- list(
  "num_complete" = "complete",
  "num_incomplete" = "incomplete",
  "mixed_complete" = "categorical",
  "mixed_incomplete" = "incomplete_categorical"
)

cases_to_plot <- unlist(target_dict[target_cases])


# ==============================================================================
# DATA LOADING & FILTERING
# ==============================================================================

# 1. Load Benchmark Results Dynamically
benchmark_list <- list()
for (t in types_to_load) {
  for (s in statuses_to_load) {
    file_name <- sprintf("results/bench_%s_%s.RDS", t, s)
    if (file.exists(file_name)) {
      benchmark_list[[length(benchmark_list) + 1]] <- readRDS(file_name)
    } else {
      warning(paste("File not found and will be skipped:", file_name))
    }
  }
}

imputation_summary_benchmark <- bind_rows(benchmark_list) %>% 
  mutate(new = FALSE) %>% 
  filter(case %in% cases_to_plot) %>%
  # Apply the case-specific measure filter
  filter(
    (case %in% c("complete", "categorical") & measure == MEASURE_COMPLETE) |
      (case == "incomplete" & measure == MEASURE_INCOMPLETE_NUM) |
      (case == "incomplete_categorical" & measure == MEASURE_INCOMPLETE_CAT)
  )

# 2. Load New Results
imputation_summary_new <- readRDS("results/imputation_summary.RDS")  %>% 
  filter(case %in% cases_to_plot) %>% 
  # Apply the exact same measure filter here
  filter(
    (case %in% c("complete", "categorical") & measure == MEASURE_COMPLETE) |
      (case == "incomplete" & measure == MEASURE_INCOMPLETE_NUM) |
      (case == "incomplete_categorical" & measure == MEASURE_INCOMPLETE_CAT)
  ) %>% 
  mutate(new = TRUE)

# 3. Combine and Apply Set Filters
imputation_summary <- bind_rows(imputation_summary_benchmark, imputation_summary_new) %>% 
  filter(case %in% cases_to_plot) %>%
  filter(set_id %in% unique(imputation_summary_new$set_id)) %>% 
  filter(mechanism %in% unique(imputation_summary_new$mechanism)) %>% 
  filter(ratio %in% unique(imputation_summary_new$ratio))

# 4. Global Method Filtering for Categorical Compatibility
# If we are plotting ANY categorical data, restrict the ENTIRE dataset 
# to only methods that successfully handled the categorical cases to avoid holes in the heatmap.
if (any(c("categorical", "incomplete_categorical") %in% cases_to_plot)) {
  
  methods_work_with_cat <- imputation_summary %>% 
    filter(case %in% c("categorical", "incomplete_categorical")) %>% 
    pull(method) %>% 
    unique()
    
  imputation_summary <- imputation_summary %>%
    filter(method %in% methods_work_with_cat)
}

if (any(c("complete", "incomplete") %in% cases_to_plot)) {
  
  methods_work_with_num <- imputation_summary %>% 
    filter(case %in% c("complete", "incomplete")) %>% 
    pull(method) %>% 
    unique()
  
  imputation_summary <- imputation_summary %>%
    filter(method %in% methods_work_with_num)
}

# Raise error if no data to plot
if (nrow(imputation_summary) == 0) {
  stop("No data available for the specified parameters. Please check your inputs and filters.")
}

# ==============================================================================
# GENERATE AND SAVE PLOTS
# ==============================================================================

# 1. Error Analysis Plot
p_errors <- plot_error_analysis(imputation_summary)
ggsave(paste0(PATH_FIGURES, "error_analysis.pdf"), p_errors, width = 12, height = 8)

# 2. Shrek's Heatmap (Aggregated Ranking)
p_heatmap <- plot_ranking_heatmap(imputation_summary, col_width=0.4)
ggsave(
  filename = paste0(PATH_FIGURES, "heatmap_ranking.pdf"), 
  plot = p_heatmap$plot,
  width = p_heatmap$width,
  height = p_heatmap$height,
  limitsize = FALSE
)

# 3. Energy vs Time Ranking (Boxplots)
p_final <- plot_energy_time_ranking(imputation_summary, c(0, 1, 40, 80, 99, 100), fixed_width=15, row_height=0.2, base_height=1)
ggsave(
  filename = paste0(PATH_FIGURES, "energy_time_ranking.pdf"), 
  plot = p_final$plot,
  width = p_final$width,
  height = p_final$height,
  limitsize = FALSE
)

# ==============================================================================
cat("Done! Figures saved in:", PATH_FIGURES, "\n")
