

library(purrr)
library(dplyr)
library(stringr)
# for vis
library(ggplot2)
library(patchwork)
library(fmsb)
library(ggridges)


get_colors_errors <- function() {
  c("computational" = "#7A4E7E", "modification" = "#A3A725", 
    "timeout" = "#1E9CC2", "missings" = "#3D6649", 
    "modification+wrong_levels" = "orangered3", "wrong_levels" = "gold",
    "none" = "#EDF2EF", "missings+wrong_levels" = "#FFB6C1")
}

get_labels_errors <- function() {
  c("computational" = "computational", "modification" = "modification", 
    "timeout" = "timeout", "missings" = "missings",     
    "modification+wrong_levels" = "wrong levels & modification", 
    "wrong_levels" = "wrong levels", "none" = "none" ,
    "missings+wrong_levels" = "wrong levels & missings")
}

get_colors_ranks <- function() {
  c("[1,3]" = "#443B54", "(3,10]"= "#7E7099", "(10,30]" = "#9E94B3", "(30,78]" = "#C9C3D5")
}

get_colors_fractions <- function() {
  c("[0,1]" = "#C9C3D5", "(1,40]"= "#9E94B3", "(40,80]" = "#7E7099", "(80,99]" = "#615577", "(99,100]" = "#443B54")
}

get_colors_datasets <- function() {
  c("enb" = "#E69F00", 
    "oes10" = "#56B4E9", 
    "airfoil_self_noise" = "#009E73", 
    "scm20d" = "#F0E442", 
    "scm1d" = "#0072B2", 
    "concrete" = "#D55E00", 
    "slump" = "#CC79A7", 
    "allergens" = "#999999", 
    "yeast" = "#000000")
}

# ==============================================================================
# PLOTTING FUNCTIONS
# ==============================================================================

#' Plot Error Frequency Analysis
#' @param data The combined imputation summary dataframe
plot_error_analysis <- function(data) {
  p_errors <- data %>% 
    filter(!is.na(measure)) %>% 
    select(-measure, -score) %>% 
    unique() %>% 
    group_by(method, new) %>% 
    mutate(
      n_attempts = n(),
      error = ifelse(is.na(error), "none", error),
      error = factor(error, levels = c("computational", "modification", "timeout", "missings", "modification+wrong_levels", "missings+wrong_levels", "wrong_levels", "none"))
      ) %>% 
    rename(`Type of error` = "error") %>% 
    group_by(method, new, `Type of error`) %>% 
    reframe(error_frac = 100 * n() / n_attempts) %>% 
    unique() %>% 
    group_by(method, new) %>% 
    mutate(joint_error = sum(error_frac[`Type of error` != "none"])) %>%
    ungroup() %>%
    mutate(method_ordered = tidytext::reorder_within(method, joint_error, new)) %>%
    
    ggplot() + 
    geom_col(aes(x = method_ordered, y = error_frac, fill = `Type of error`, alpha = `Type of error`)) +
    tidytext::scale_x_reordered() +
    facet_grid(~ ifelse(new, "New", "Benchmark"), scales = "free_x", space = "free_x") +
    scale_fill_manual(name = "Type of error", values = get_colors_errors(), labels=get_labels_errors()) +
    scale_alpha_manual(
      name = "Type of error", # Must match scale_fill_manual
      labels = get_labels_errors(), # Must match scale_fill_manual
      values = c("computational" = 1, "modification" = 1, "timeout" = 1, 
                 "missings" = 1, "wrong_levels" = 1, "modification+wrong_levels" = 1, 
                 "missings+wrong_levels" = 1, "none" = 0.8)
    ) +
    labs(y = "Imputations [%]", x = "Method", title = "Error Frequency Analysis") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1), 
      legend.position = "top", 
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", color = NA), 
      strip.clip = "off"
    )
  
  return(p_errors)
}

#' Plot Energy vs Time Ranking (Boxplots)
#' @param data The combined imputation summary dataframe
#' @param success_breaks Numeric vector of breaks for success percentages
#' @param base_height Fixed base height for the plot margins/legends
#' @param row_height Height multiplier per method (the beta)
#' @param fixed_width Fixed total width for the plot
plot_energy_time_ranking <- function(data, success_breaks, base_height = 2, row_height = 0.4, fixed_width = 10) {
  
  dat_plt <- data %>% 
    filter(!is.na(measure)) %>% 
    group_by(method, new) %>% 
    mutate(`success [%]` = mean(is.na(error)) * 100) %>% 
    group_by(method, new, set_id, mechanism, ratio) %>% 
    summarise(
      score_mean = mean(score, na.rm = TRUE), 
      time = mean(time, na.rm = TRUE),
      `success [%]` = first(`success [%]`),
      .groups = "drop"
    ) %>% 
    mutate(score_mean = ifelse(is.nan(score_mean), NA, score_mean)) %>% 
    group_by(set_id, mechanism, ratio) %>% 
    mutate(
      n_successful = sum(!is.na(score_mean)),
      ranking = {
        r <- rep(NA, length(score_mean))
        v <- !is.na(score_mean)
        r[v] <- rank(score_mean[v])
        r[!v] <- n_successful[!v] + 1
        r
      }
    ) %>% 
    ungroup() %>% 
    group_by(method, new) %>% 
    reframe(
      mean_ranking = mean(ranking, na.rm = TRUE),
      time = mean(time, na.rm = TRUE), 
      `success [%]` = cut(first(`success [%]`), success_breaks, include.lowest = TRUE),
      ranking = ranking
    ) %>% 
    arrange(mean_ranking) %>% 
    mutate(method = factor(method, levels = unique(method)))
  
  # --- DYNAMIC SIZE CALCULATION ---
  n_methods <- length(unique(dat_plt$method))
  calc_height <- base_height + (row_height * n_methods)
  
  # Extract labels for text formatting
  m_labels <- dat_plt %>% select(method, new) %>% unique() %>% arrange(method)
  
  min_time_ms <- min(dat_plt$time, na.rm = TRUE) * 1000
  
  p_time <- dat_plt %>% 
    group_by(method, new, `success [%]`) %>% 
    summarise(time = mean(time), .groups="drop") %>%
    ggplot(aes(x = method, y = time * 1000, fill = `success [%]`)) +
    geom_col(aes(color = new), width = 0.8, size = 0.6) +
    scale_color_manual(values = c("TRUE" = "blue", "FALSE" = NA), guide = "none") +
    scale_fill_manual(values = get_colors_fractions()) +
    scale_y_continuous(
      "Time", 
      trans = c("log10", "reverse"),
      breaks = c(min_time_ms / 1000, 1, 10, 60, 600, 1800, 3600, 10800) * 1000, 
      labels = c(paste0(round(min_time_ms), "ms"), "1s", "10s", "1min", "10min", "30min", "1h", "3h")
    ) +
    coord_flip() + 
    theme_bw() + 
    theme(
      axis.title.y = element_blank(), 
      axis.text.y = element_blank(),  
      axis.ticks.y = element_blank(), 
      legend.position = "none",
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(color = "black", linetype = "dashed")
    )
  
  p_box <- dat_plt %>% 
    ggplot(aes(x = method, y = ranking)) +
    geom_boxplot(aes(fill = new), alpha = 0.7) +
    scale_fill_manual(values = c("TRUE" = "royalblue1", "FALSE" = "gray90"), guide = "none") +
    geom_point(aes(y = mean_ranking, col = "a"), size = 2) +
    scale_color_manual(name = "", values = c("a" = "firebrick"), labels = c("a" = "Avg Rank")) +
    coord_flip() + 
    theme_bw() + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_text(face = ifelse(m_labels$new, "bold", "plain"))
    )
  
  p_final <- p_time + p_box + patchwork::plot_layout(guides = "collect", widths = c(1, 1.5)) & theme(legend.position = 'bottom')
  
  # Return as a list
  return(list(
    plot = p_final,
    width = fixed_width,
    height = calc_height
  ))
}

#' Plot Aggregated Ranking Heatmap
#' @param data The combined imputation summary dataframe
#' @param base_width Fixed base width for margins/y-axis text
#' @param col_width Width multiplier per column (case_id)
#' @param base_height Fixed base height for margins/legends
#' @param row_height Height multiplier per row (method)
plot_ranking_heatmap <- function(data, base_width = 3, col_width = 0.5, base_height = 2, row_height = 0.4) {
  
  dat_prepared <- data %>% 
    select(-any_of(c("time", "attempts", "error", "imputation_fun"))) %>% 
    unique() %>%
    mutate(score = ifelse(is.nan(score), NA, score)) %>%
    group_by(set_id, mechanism, ratio, method, new) %>%
    summarise(score_mean = mean(score, na.rm = TRUE), .groups = "drop") %>%
    mutate(score_mean = ifelse(is.nan(score_mean), NA, score_mean)) %>% 
    group_by(set_id, mechanism, ratio) %>%
    mutate(
      n_successful = sum(!is.na(score_mean)), 
      ranking = {
        r <- rep(NA, length(score_mean))
        valid <- !is.na(score_mean)
        r[valid] <- rank(score_mean[valid])
        r[!valid] <- n_successful[!valid] + 1 
        r
      }
    ) %>%
    ungroup() %>%
    mutate(case_id = paste(set_id, mechanism, ratio, sep = "_")) %>%
    group_by(method, new) %>%
    mutate(mean_ranking = mean(ranking, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(new), mean_ranking) %>%
    mutate(method = factor(method, levels = rev(unique(method))))
  
  # --- DYNAMIC SIZE CALCULATION ---
  n_cols <- length(unique(dat_prepared$case_id))
  n_rows <- length(unique(dat_prepared$method))
  
  calc_width <- base_width + (col_width * n_cols)
  calc_height <- base_height + (row_height * n_rows)
  
  p_heatmap <- dat_prepared %>%
    ggplot() +
    geom_tile(aes(x = case_id, y = method, fill = ranking), colour = "black") +
    geom_text(aes(x = case_id, y = method, label = round(ranking, 1), 
                  fontface = ifelse(new, "bold", "plain")), size = 3) +
    facet_grid(ifelse(new, "New", "Benchmark") ~ ., scales = "free_y", space = "free_y") +
    scale_fill_gradient(low = "darkgreen", high = "white", name = "Rank") +
    labs(title = paste("Ranking Heatmap")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, size = 8, hjust = 1, vjust = 0.5),
      strip.text.y = element_text(face = "bold", size = 10, angle = 0),
      strip.background = element_rect(fill = "grey95", color = "black")
    )
  
  # Return as a list
  return(list(
    plot = p_heatmap,
    width = calc_width,
    height = calc_height
  ))
}