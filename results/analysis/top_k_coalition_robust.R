library(dplyr)
library(purrr)
library(tidyr)
library(future)
library(furrr)

score_tmp <- "energy_std"
k_values <- 1:5
max_group_size <- 8

n_methods <- length(unique(imputation_summary$method))

t <- imputation_summary %>%
  filter(measure == score_tmp) %>%
  unique() %>%
  group_by(method, set_id, mechanism, ratio) %>%
  reframe(score = mean(score, na.rm = TRUE)) %>%
  mutate(case_id = paste0(set_id, mechanism, ratio)) %>%
  mutate(score = ifelse(is.nan(score), NA, score)) %>%
  group_by(set_id, mechanism, ratio) %>%
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- n_methods
    ranking
  }) %>%
  ungroup() %>%
  group_by(method) %>%
  mutate(mean_ranking = mean(ranking, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(mean_ranking) %>%
  mutate(method = factor(method, levels = unique(method)))

get_covered_cases <- function(df, k) {
  covered_cases_df <- df %>%
    filter(ranking <= k) %>%
    select(method, case_id) %>%
    distinct()
  
  all_methods <- as.character(unique(df$method))
  
  covered_cases_list <- split(covered_cases_df$case_id, as.character(covered_cases_df$method))
  
  present_methods <- names(covered_cases_list)
  missing_methods <- setdiff(all_methods, present_methods)
  
  if (length(missing_methods) > 0) {
    empty_entries <- setNames(replicate(length(missing_methods), character(0)), missing_methods)
    covered_cases_list <- c(covered_cases_list, empty_entries)
  }
  
  return(covered_cases_list[all_methods])
}

compute_metric_fast <- function(method_subset_indices, covered_cases_list, total_cases, method_names) {
  method_subset <- method_names[method_subset_indices]
  
  if (length(method_subset) == 0) {
    return(1.0)
  }
  
  combined_coverage <- reduce(covered_cases_list[method_subset], union)
  num_covered <- length(combined_coverage)
  
  return(1 - (num_covered / total_cases))
}

find_dominated_methods <- function(df) {
  message("Finding dominated methods...")
  
  rankings_data <- df %>% select(method, case_id, ranking)
  
  dominance_check <- rankings_data %>%
    inner_join(rankings_data, by = "case_id", suffix = c("_a", "_b"), relationship = "many-to-many") %>%
    filter(method_a != method_b) %>%
    group_by(method_a, method_b) %>%
    summarise(b_always_better_or_equal = all(ranking_b <= ranking_a), .groups = 'drop')
  
  dominated_by_pairs <- dominance_check %>%
    filter(b_always_better_or_equal)
  
  dominated_methods <- unique(as.character(dominated_by_pairs$method_a))
  
  message(paste0("  Found ", length(dominated_methods), " dominated methods to exclude."))
  return(dominated_methods)
}


all_cases <- unique(t$case_id)
total_num_cases <- length(all_cases)

dominated_methods <- find_dominated_methods(t)

plan(multisession, workers = availableCores() - 1)

results_list <- list()

tic <- proc.time()[3]
for (k in k_values) {
  message(paste0("Processing for k = ", k, "..."))
  
  covered_cases_list_k <- get_covered_cases(t, k)
  
  methods_with_any_coverage <- names(which(sapply(covered_cases_list_k, length) > 0))
  
  candidate_methods <- setdiff(methods_with_any_coverage, dominated_methods)
  
  if(length(candidate_methods) == 0) {
    message("  No candidate methods remain after filtering. Skipping.")
    next
  }
  
  message(paste0("  Testing with ", length(candidate_methods), " candidate methods after pre-filtering."))
  
  
  for (group_size in 1:min(max_group_size, length(candidate_methods))) {
    
    num_combinations <- choose(length(candidate_methods), group_size)
    message(paste0("  Testing ", format(num_combinations, big.mark=","), " combinations of size ", group_size, "..."))
    
    if (num_combinations > 5e7) {
      message("    Too many combinations, skipping to avoid memory issues.")
      next
    }
    
    combinations <- combn(seq_along(candidate_methods), group_size, simplify = FALSE)
    
    metrics <- future_map_dbl(combinations, compute_metric_fast, 
                              covered_cases_list = covered_cases_list_k, 
                              total_cases = total_num_cases,
                              method_names = candidate_methods,
                              .progress = TRUE)
    
    best_metric <- min(metrics)
    best_combo_index <- which.min(metrics)
    best_method_set <- candidate_methods[combinations[[best_combo_index]]]
    
    results_list[[length(results_list) + 1]] <- data.frame(
      k = k,
      group_size = group_size,
      methods = paste(sort(best_method_set), collapse = ", "),
      metric = best_metric,
      num_combinations_tested = num_combinations
    )
    
    if (best_metric == 0) {
      message("  Found a perfect set. No need to test larger groups for this k.")
      break
    }
  }
}
tac <- proc.time()[3]
message(paste0("Total processing time: ", round(tac - tic, 2), " seconds."))

plan(sequential)

final_results <- do.call(rbind, results_list)
final_results <- final_results %>% 
  mutate(score = 1 - metric,
         improvement_score = lag(score, default = 0) - score)
print(final_results)
saveRDS(final_results, "topKcoalition_robust_results.RDS")
