

library(dplyr)

score_tmp <- "energy_std"

n_methods <- length(unique(imputation_summary$method))

t <- imputation_summary %>%
  # filter(!is.na(score)) %>% 
  # select(-imputation_fun) %>%
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



# Define all methods available
total_methods <- unique(imputation_summary$method)

# Define the k values to test
k_values <- 1:5

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




library(ggplot2)
library(dplyr)

p1 <- ggplot(final_results, aes(x = group_size, y = 1 - metric, color = factor(k))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = 1:10*0.1) +
  scale_x_continuous(breaks = 1:max(final_results$group_size)) +
  labs(
    title = "",
    x = "Number of different methods",
    y = "Proportion of cases in top k ranks",
    color = "k"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


library( ggrepel)

final_results %>% 
  mutate(methods = lapply(strsplit(methods, ", "), function(i) rev(i)[1])) %>% 
  # mutate(methods = ifelse(group_size > 1, paste0("+ ", methods), methods)) %>% 
  group_by(k) %>% 
  mutate(pctg_added = {
    c((1 - metric)[1], (1 - metric)[-1] - (1 - metric)[-length(metric)])
  }) %>% 
  ggplot(aes(x = group_size, y = 1 - metric, color = factor(k))) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = 1:10*0.1) +
  scale_x_continuous(breaks = 1:max(final_results$group_size)) +
  labs(
    title = "",
    x = "Number of different methods",
    y = "Proportion of cases in top k ranks",
    color = "k"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  # geom_text(aes( x = group_size, y = 1 - metric, color = factor(k), label = methods),
  #           segment.size  = 0,
  #           segment.color = "grey50",
  #           direction     = "y",
  #           hjust         = 0,
  #           show.legend = FALSE) +
  geom_label_repel(aes(x = group_size, y = 1 - metric, fill = factor(k), label = paste0(methods, "  [+", round(pctg_added*100, 0), "%]")),
             alpha = 0.8, col = "black",
             segment.size  = 0,
             segment.color = "grey50",
             hjust         = 0, show.legend = FALSE,
             nudge_x = 0.02)  +
  coord_cartesian(clip = "off") +
  theme(legend.position = "bottom"
  )
# theme(axis.text.y = element_blank(),
#       axis.title.y = element_blank())



# 
# 
# final_results %>% 
#   mutate(methods = lapply(strsplit(methods, ", "), function(i) rev(i)[1])) %>% 
#   # mutate(methods = ifelse(group_size > 1, paste0("+ ", methods), methods)) %>% 
#   group_by(k) %>% 
#   mutate(pctg_added = {
#     c((1 - metric)[1], (1 - metric)[-1] - (1 - metric)[-length(metric)])
#   }) %>% 
#   ggplot(aes(x = k, y = 1 - metric, group = factor(k))) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 3) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1),
#                      breaks = 1:10*0.1) +
#   scale_x_continuous(breaks = 1:max(final_results$group_size)) +
#   labs(
#     title = "",
#     x = "k",
#     y = "Proportion of cases in top k ranks",
#     color = "k"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   # geom_text(aes( x = group_size, y = 1 - metric, color = factor(k), label = methods),
#   #           segment.size  = 0,
#   #           segment.color = "grey50",
#   #           direction     = "y",
#   #           hjust         = 0,
#   #           show.legend = FALSE) +http://127.0.0.1:31939/graphics/e79ff87e-06d5-41ec-9f9c-a86a38c797b8.png
#   geom_label(aes(x = k, y = 1 - metric, label = paste0(methods, "  [+", round(pctg_added*100, 0), "%]")),
#              alpha = 0.6, col = "black",
#              segment.size  = 0,
#              segment.color = "grey50",
#              hjust         = 0, show.legend = FALSE,
#              nudge_x = 0.05)  +
#   coord_cartesian(clip = "off") +
#   theme(legend.position = "none"
#   ) +
#   scale_x_continuous(expand = expansion(mult = c(0, 0.2)))
# 
# 
# 
