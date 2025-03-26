

library(dplyr)
imputation_summary <- readRDS("~/INRIA/R_scripts/benchmark/results/imputation_summary_M2.RDS")

n_methods <- length(unique(imputation_summary$method))

t <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  # select(-imputation_fun) %>%
  filter(measure == "energy_std") %>% 
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



# Define all methods available
total_methods <- unique(imputation_summary$method)

# Define the k values to test
k_values <- c(1, 2, 3, 4, 5)

# Maximum group size to test
max_group_size <- 10  # Adjust as needed

# Function to compute the proportion of cases where no method is in the top k
compute_metric <- function(method_subset, k) {
  t2 <- t %>% 
    filter(method %in% method_subset) %>% 
    mutate(top_k = ranking <= k) %>%
    group_by(set_id, mechanism, ratio) %>%
    summarise(at_least_one_top_k = any(top_k), .groups = 'drop')
  
  return(sum(!t2$at_least_one_top_k) / nrow(t2))  # Proportion of cases where no method is in top k
}

# Store results
results <- list()

# Forward selection loop
for (k in k_values) {
  print(paste0("Computing for k = ", k, "..."))
  selected_methods <- character()
  best_results <- data.frame(methods = character(), group_size = integer(), k = integer(), metric = numeric())
  
  for (group_size in 1:max_group_size) {
    print(paste0("Selecting method ", group_size, "..."))
    
    best_method <- NULL
    best_metric <- Inf  # We want to minimize the metric
    
    # Try adding each remaining method and find the best one
    for (method in setdiff(total_methods, selected_methods)) {
      candidate_methods <- c(selected_methods, method)
      metric <- compute_metric(candidate_methods, k)
      
      if (metric < best_metric) {
        best_metric <- metric
        best_method <- method
      }
      
      if (best_metric == 0) {
        break  # No need to continue if we already have a perfect score
      }
    }
    
    if (!is.null(best_method)) {
      selected_methods <- c(selected_methods, best_method)
      best_results <- rbind(best_results, data.frame(
        methods = paste(selected_methods, collapse = ", "),
        group_size = group_size,
        k = k,
        metric = best_metric
      ))
    }
    if (best_metric == 0) {
      break  # No need to continue if we already have a perfect score
    }
  }
  
  results[[as.character(k)]] <- best_results
}

# Combine results and save
final_results <- do.call(rbind, results)

