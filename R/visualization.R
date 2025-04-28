

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
    "timeout" = "#1E9CC2", "missings" = "#3D6649", "none" = "#EDF2EF")
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



plot_errors <- function(imputation_summary) {
  
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-measure, -score) %>% 
    unique() %>% 
    group_by(method) %>% 
    mutate(n_attempts = n()) %>% 
    mutate(error = ifelse(is.na(error), "none", error)) %>% 
    mutate(error = factor(error, levels = c("computational",  "modification", 
                                            "timeout", "missings", "none"))) %>% 
    mutate(method = factor(method, levels = sort(unique(imputation_summary$method), decreasing = TRUE))) %>% 
    rename(`Type of error` = "error") %>% 
    group_by(method, `Type of error`) %>% 
    reframe(error_frac = 100*n()/n_attempts) %>% 
    unique() %>% 
    ggplot() + 
    geom_col(aes(x = method, y = error_frac, fill = `Type of error`, 
                 alpha = `Type of error`)) +
    ylim(0, 100) +
    ylab("imputations [%]" ) +
    coord_flip() +
    scale_fill_manual(name = "Type of error", values = get_colors_errors()) +
    scale_alpha_manual(values = c("computational" = 1, "modification" = 1, 
                                  "timeout" = 1, "missings" = 1, "none" = 0.8)) +
    theme_minimal() 
  
}


plot_errors_datasets <- function(imputation_summary) {
  
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-measure, -score, -imputation_fun) %>% 
    unique() %>% 
    group_by(method) %>% 
    mutate(overall_errors = sum(!is.na(error))) %>% 
    ungroup() %>% 
    group_by(method, set_id) %>% 
    mutate(n_attempts = n()) %>% 
    mutate(error = ifelse(is.na(error), "none", error)) %>% 
    mutate(error = factor(error, levels = c("computational",  "modification", 
                                            "timeout", "missings", "none"))) %>% 
    # mutate(method = factor(method, levels = sort(unique(imputation_summary$method), decreasing = TRUE))) %>% 
    rename(`Type of error` = "error") %>% 
    group_by(method, `Type of error`, set_id) %>% 
    reframe(error_frac = 100*n()/n_attempts, overall_errors = overall_errors) %>% 
    unique() %>% 
    ggplot() + 
    geom_col(aes(x = reorder(method, overall_errors), y = error_frac, fill = `Type of error`, 
                 alpha = `Type of error`)) +
    ylim(0, 100) +
    ylab("imputations [%]" ) +
    coord_flip() +
    facet_grid(~set_id) +
    scale_fill_manual(name = "Type of error", values = get_colors_errors()) +
    scale_alpha_manual(values = c("computational" = 1, "modification" = 1, 
                                  "timeout" = 1, "missings" = 1, "none" = 0.8)) +
    theme_minimal() +
    theme(legend.position = "bottom", 
          axis.text.x = element_blank()) +
    xlab("method")
  
}


plot_errors_mechanism <- function(imputation_summary) {
  
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-measure, -score, -imputation_fun) %>% 
    unique() %>% 
    group_by(method) %>% 
    mutate(overall_errors = sum(!is.na(error))) %>% 
    ungroup() %>% 
    group_by(method, mechanism) %>% 
    mutate(n_attempts = n()) %>% 
    mutate(error = ifelse(is.na(error), "none", error)) %>% 
    mutate(error = factor(error, levels = c("computational",  "modification", 
                                            "timeout", "missings", "none"))) %>% 
    # mutate(method = factor(method, levels = sort(unique(imputation_summary$method), decreasing = TRUE))) %>% 
    rename(`Type of error` = "error") %>% 
    group_by(method, `Type of error`, mechanism) %>% 
    reframe(error_frac = 100*n()/n_attempts, overall_errors = overall_errors) %>% 
    unique() %>% 
    ggplot() + 
    geom_col(aes(x = reorder(method, overall_errors), y = error_frac, fill = `Type of error`, 
                 alpha = `Type of error`)) +
    ylim(0, 100) +
    ylab("imputations [%]" ) +
    coord_flip() +
    facet_grid(~mechanism) +
    scale_fill_manual(name = "Type of error", values = get_colors_errors()) +
    scale_alpha_manual(values = c("computational" = 1, "modification" = 1, 
                                  "timeout" = 1, "missings" = 1, "none" = 0.8)) +
    theme_minimal() +
    theme(legend.position = "bottom", 
          axis.text.x = element_blank()) +
    xlab("method")
  
}


shreks_plot <- function(imputation_summary ) {
  
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  imputation_summary %>%
    filter(!is.na(measure)) %>% 
    select(-imputation_fun) %>% 
    filter(case == "complete", measure == "energy_std") %>% 
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
    mutate(method = factor(method, levels = unique(method))) %>% 
    ggplot() +
    geom_tile(aes(x = case_id, y = method, fill = ranking), colour = "black") +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_text(aes(x = case_id, y = method, label = ranking)) +
    scale_fill_continuous() +
    guides(fill = guide_colourbar(barwidth = 0.5, barheight = 40)) +
    scale_fill_gradient(low = "darkgreen", high = "white") 
  
}

plot_cases <- function(imputation_summary ) {
  
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  dat <- imputation_summary %>%
    filter(!is.na(measure)) %>% 
    select(-imputation_fun) %>% 
    filter(case == "complete", measure == "energy_std") %>% 
    unique() %>% 
    group_by(method, set_id, mechanism) %>% 
    reframe(score = mean(score, na.rm = TRUE)) %>% 
    mutate(case_id = paste0(set_id, mechanism)) %>% 
    mutate(score = ifelse(is.nan(score), NA, score)) %>% 
    group_by(set_id, mechanism) %>% 
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
  
  
  # plts <- lapply(unique(dat[["method"]]), function(one_method) {
  #   dat %>% 
  #     filter(method == one_method) %>%
  #     select(case_id, ranking, method) %>% 
  #     pivot_wider(names_from = case_id, values_from = ranking) %>% 
  #     select(-method) %>% 
  #     rbind(Min = n_methods, Max = 0, .) %>% 
  #     radarchart(maxmin  = TRUE,
  #                cglty = 1, cglcol = "gray",
  #                pcol = 1, plwd = 2,
  #                pdensity = 10,
  #                pangle = 40,
  #                title = one_method)
  # })
  
  par(mfrow = c(1, 1))
  
  for(one_method in unique(dat[["method"]])) {
    dat %>% 
      filter(method == one_method) %>%
      select(case_id, ranking, method) %>% 
      pivot_wider(names_from = case_id, values_from = ranking) %>% 
      select(-method) %>% 
      rbind(Min = n_methods, Max = 0, .) %>% 
      radarchart(maxmin  = TRUE,
                 cglty = 1, cglcol = "gray",
                 pcol = 1, plwd = 2,
                 pfcol = rgb(0, 0.4, 1, 0.25),
                 title = one_method)
  }
  
  areas <- c(rgb(1, 0, 0, 0.25),
             rgb(0, 1, 0, 0.25),
             rgb(0, 0, 1, 0.25))
  
  dat %>% 
    filter(method %in% c("mice_cart", "hyperimpute", "areg")) %>%
    select(case_id, ranking, method) %>% 
    pivot_wider(names_from = case_id, values_from = ranking) %>% 
    select(-method) %>% 
    rbind(Min = n_methods, Max = 0, .) %>% 
    radarchart(maxmin  = TRUE,
               cglty = 1, cglcol = "gray",
               pcol = 1, plwd = 2,
               pfcol = areas)
  
  legend("topright",
         legend = c("mice_cart", "hyperimpute", "areg"),
         bty = "n", text.col = "grey25", pch = 20, col = areas)

  
}





plot_energy_time_ranking <- function(arrange_success = TRUE, breaks = c(0, 1, 40, 80, 99, 100)) {
  
  
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  dat_plt <- imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-imputation_fun, -attempts) %>% 
    # filter(!(set_id %in% c("oes10", "scm1d", "scm20d"))) %>% 
    filter(case == "complete", measure == "energy_std") %>%
    unique() %>% 
    group_by(method) %>% 
    mutate(`success [%]` = mean(is.na(error)) * 100) %>% 
    group_by(method, set_id, mechanism, ratio) %>% 
    mutate(score = mean(score, na.rm = TRUE),
           time = mean(time, na.rm = TRUE)) %>% 
    select(-rep, -case, -error) %>% 
    unique() %>% 
    mutate(score = ifelse(is.nan(score), NA, score)) %>% 
    ungroup() %>% 
    group_by(set_id, mechanism, ratio) %>% 
    mutate(ranking =  {
      ranking <- rep(NA, length(score))
      ranking[!is.na(score)] <- rank(score[!is.na(score)])
      ranking[is.na(ranking)] <- n_methods
      ranking
    }) %>% 
    ungroup() %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            mean_ranking = mean(ranking, na.rm = TRUE),
            median_ranking = median(ranking, na.rm = TRUE),
            time = mean(time, na.rm = TRUE), 
            `success [%]` = `success [%]`) %>% 
    mutate(`success [%]` = cut(`success [%]`, breaks, 
                               include.lowest = TRUE)) %>% 
    unique() %>% 
    arrange(mean_score) %>% 
    mutate(method = factor(method, levels = method))
  
  if(arrange_success)
    dat_plt <- dat_plt %>%
    arrange(mean_ranking) %>%
    mutate(method = factor(method, levels = method))
  
  min_time <- min(dat_plt$time) * 1000
  
  p1 <- dat_plt %>% 
    ggplot(aes(x = method, y = time * 1000, fill = `success [%]`)) +
    geom_rect(aes(xmin = as.numeric(method) - 0.4, 
                  xmax = as.numeric(method) + 0.4,
                  ymin = min_time - 10, 
                  ymax = time * 1000, 
                  fill = `success [%]`)) +
    scale_fill_manual(name = "success [%]", values = get_colors_fractions()) +
    labs(x = "Methods", y = "Average Time") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_x_discrete(position = "top") +
    coord_flip() +
    scale_y_continuous("Time", trans = c("log10", "reverse"),
                       breaks = c(min_time/1000, 1, 60, 600, 1800, 1800*2, 1800*4, 1800*6) * 1000, 
                       labels = c("116ms", "1s", "1min", "10min", "30min", "1h", "2h", "3h")) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(color = "black", linetype = "dashed"))
  
  
  p2 <- dat_plt %>% 
    ungroup() %>% 
    mutate(max_score = max(log10(mean_score), na.rm = TRUE)) %>% 
    ggplot(aes(x = method, y = log10(mean_score), fill = `success [%]`)) +
    geom_col() +
    scale_fill_manual(name = "success [%]", 
                      values = get_colors_fractions()) +
    labs(x = "Methods", y = "Mean Energy") +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0.5),
          axis.title.y = element_blank()) +
    ylab("log10energy") +
    coord_flip() +
    geom_text(aes(x = method, y = max_score + 0.5, label = round(mean_ranking, 1)), size = 3)
  
  p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
}






plot_energy_time_ranking <- function(arrange_success = TRUE, breaks = c(0, 1, 40, 80, 99, 100)) {
  
  
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  dat_plt <- imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-imputation_fun, -attempts) %>% 
    # filter(!(set_id %in% c("oes10", "scm1d", "scm20d"))) %>% 
    filter(case == "complete", measure == "energy_std") %>%
    unique() %>% 
    group_by(method) %>% 
    mutate(`success [%]` = mean(is.na(error)) * 100) %>% 
    group_by(method, set_id, mechanism, ratio) %>% 
    mutate(score = mean(score, na.rm = TRUE),
           time = mean(time, na.rm = TRUE)) %>% 
    select(-rep, -case, -error) %>% 
    unique() %>% 
    mutate(score = ifelse(is.nan(score), NA, score)) %>% 
    ungroup() %>% 
    group_by(set_id, mechanism, ratio) %>% 
    mutate(ranking =  {
      ranking <- rep(NA, length(score))
      ranking[!is.na(score)] <- rank(score[!is.na(score)])
      ranking[is.na(ranking)] <- n_methods
      ranking
    }) %>% 
    ungroup() %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            mean_ranking = mean(ranking, na.rm = TRUE),
            median_ranking = median(ranking, na.rm = TRUE),
            time = mean(time, na.rm = TRUE), 
            `success [%]` = `success [%]`) %>% 
    mutate(`success [%]` = cut(`success [%]`, breaks, 
                               include.lowest = TRUE)) %>% 
    unique() %>% 
    arrange(mean_score) %>% 
    mutate(method = factor(method, levels = method))
  
  if(arrange_success)
    dat_plt <- dat_plt %>%
    arrange(mean_ranking) %>%
    mutate(method = factor(method, levels = method))
  
  min_time <- min(dat_plt$time) * 1000
  
  p1 <- dat_plt %>% 
    ggplot(aes(x = method, y = time * 1000, fill = `success [%]`)) +
    geom_rect(aes(xmin = as.numeric(method) - 0.4, 
                  xmax = as.numeric(method) + 0.4,
                  ymin = min_time - 10, 
                  ymax = time * 1000, 
                  fill = `success [%]`)) +
    scale_fill_manual(name = "success [%]", values = get_colors_fractions()) +
    labs(x = "Methods", y = "Average Time") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_x_discrete(position = "top") +
    coord_flip() +
    scale_y_continuous("Time", trans = c("log10", "reverse"),
                       breaks = c(min_time/1000, 1, 60, 600, 1800, 1800*2, 1800*4, 1800*6) * 1000, 
                       labels = c("116ms", "1s", "1min", "10min", "30min", "1h", "2h", "3h")) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(color = "black", linetype = "dashed"))
  
  
  p2 <- dat_plt %>% 
    ungroup() %>% 
    mutate(max_score = max(log10(mean_score), na.rm = TRUE)) %>% 
    ggplot(aes(x = method, y = log10(mean_score), fill = `success [%]`)) +
    geom_col() +
    scale_fill_manual(name = "success [%]", 
                      values = get_colors_fractions()) +
    labs(x = "Methods", y = "Mean Energy") +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0.5),
          axis.title.y = element_blank()) +
    ylab("log10energy") +
    coord_flip() +
    geom_text(aes(x = method, y = max_score + 0.5, label = round(mean_ranking, 1)), size = 3)
  
  p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
}



# 
# plot_111234 <- function(arrange_success = TRUE, breaks = c(0, 1, 40, 80, 99, 100)) {
#   
#   
#   n_methods <- length(unique(pull(imputation_summary, method)))
#   
#   dat_plt <- imputation_summary %>% 
#     filter(!is.na(measure)) %>% 
#     select(-imputation_fun, -attempts) %>% 
#     # filter(!(set_id %in% c("oes10", "scm1d", "scm20d"))) %>% 
#     filter(case == "complete", measure == "energy") %>%
#     unique() %>% 
#     group_by(method) %>% 
#     mutate(`success [%]` = mean(is.na(error)) * 100) %>% 
#     group_by(method, set_id, mechanism, ratio) %>% 
#     mutate(score = mean(score, na.rm = TRUE),
#            time = mean(time, na.rm = TRUE)) %>% 
#     select(-rep, -case, -error) %>% 
#     unique() %>% 
#     mutate(score = ifelse(is.nan(score), NA, score)) %>% 
#     group_by(set_id, mechanism, ratio) %>% 
#     mutate(ranking =  {
#       ranking <- rep(NA, length(score))
#       ranking[!is.na(score)] <- order(score[!is.na(score)])
#       ranking[is.na(ranking)] <- n_methods
#       ranking
#     }) %>% 
#     group_by(method) %>% 
#     mutate(mean_score_total = mean(score, na.rm = TRUE)) %>% 
#     group_by(method, set_id) %>% 
#     reframe(mean_score = mean(score, na.rm = TRUE),
#             mean_ranking = mean(ranking, na.rm = TRUE),
#             median_ranking = median(ranking, na.rm = TRUE),
#             time = mean(time, na.rm = TRUE), 
#             `success [%]` = `success [%]`,
#             set_id = set_id,
#             mean_score_total = mean_score_total) %>% 
#     mutate(`success [%]` = cut(`success [%]`, breaks, 
#                                include.lowest = TRUE)) %>% 
#     unique() %>% 
#     arrange(mean_score) %>% 
#     mutate(method = factor(method, levels = unique(method)))
#   
#   if(arrange_success)
#     dat_plt <- dat_plt %>%
#     arrange(mean_score_total) %>%
#     mutate(method = factor(method, levels = unique(method)))
#   
#   min_time <- min(dat_plt$time) * 1000
#   
#   p1 <- dat_plt %>% 
#     mutate(time = time * 1000) %>% 
#     ggplot() +
#     geom_col(aes(x = method, y = log10(time), fill = set_id)) +
#     scale_fill_manual(name = "dataset", values = get_colors_datasets()) +
#     labs(x = "Methods", y = "Average Time") +
#     theme_bw() +
#     theme(axis.text.y = element_blank(),
#           axis.title.y = element_blank(),
#           legend.position = "none") +
#     scale_x_discrete(position = "top") +
#     coord_flip() +
#     scale_y_continuous("log10 time", trans = c("reverse"))
#   
#   
#   p2 <- ggplot(dat_plt, aes(x = method, y = log10(mean_score), fill = set_id)) +
#     geom_col() +
#     scale_fill_manual(name = "dataset", values = get_colors_datasets()) +
#     labs(x = "Methods", y = "Mean Energy") +
#     theme_bw() +
#     theme(axis.text.y = element_text(hjust = 0.5),
#           axis.title.y = element_blank()) +
#     ylab("log10energy") +
#     coord_flip()
#   
#   p1 + p2 + plot_layout(guides = "collect")  & theme(legend.position = 'bottom')
#   
# }








# plot_score_tile_dataset <- function(imputation_summary) {
#   
#   datasets <- unique(imputation_summary[["set_id"]])
#   
#   imputation_summary_tmp <- imputation_summary %>%
#     filter(measure == "energy_std", case == "complete", method != "pemm") %>%
#     select(set_id, method, score) %>%
#     group_by(set_id, method) %>%
#     reframe(score = mean(score, na.rm = TRUE)) %>% 
#     mutate(score = log10(score))
#   
#   p <- list()
#   
#   p1 <- imputation_summary_tmp %>%
#     filter(set_id == datasets[1]) %>% 
#     ggplot() +
#     geom_tile(aes(x = method, y = 1, fill = score), color = "white", linetype = 1) +
#     scale_fill_gradient(low = "darkolivegreen2", high = "firebrick4", na.value = "darkgrey") +
#     coord_flip() +
#     ggtitle("energy dist") +
#     theme_minimal() +
#     theme(legend.position = "bottom", 
#           axis.title.x = element_blank(),
#           axis.ticks.x = element_blank(),
#           axis.text.x = element_blank(),
#           legend.text = element_text(angle = 90),
#           legend.title=element_blank()) +
#     ggtitle(datasets[1])
#   
#   
#   
#   for(i in datasets[-1]) {
#     p[[i]] <- imputation_summary_tmp %>%
#       filter(set_id == i) %>% 
#       ggplot() +
#       geom_tile(aes(x = method, y = 1, fill = score), color = "white", linetype = 1) +
#       scale_fill_gradient(low = "darkolivegreen2", high = "firebrick4", na.value = "darkgrey") +
#       coord_flip() +
#       ggtitle("energy dist") +
#       theme_minimal() +
#       theme(legend.position = "bottom", 
#             axis.title.x = element_blank(),
#             axis.ticks.x = element_blank(),
#             axis.text.x = element_blank(),
#             axis.title.y = element_blank(),
#             axis.ticks.y = element_blank(),
#             axis.text.y = element_blank(),
#             legend.title = element_blank(),
#             legend.text = element_text(angle = 90)) +
#       ggtitle(i)
#   }
#   
#   p <- c(list(p1), p)
#   
#   patchwork::wrap_plots(p, nrow = 1) + plot_annotation('log10 energy std')
#   
#   
# }



# plot_averaged_energy <- function(imputation_summary) {
#   imputation_summary %>% 
#     filter(!is.na(measure)) %>% 
#     filter(measure == "energy_std") %>% 
#     group_by(method, set_id) %>% 
#     reframe(mean_score = mean(score, na.rm = TRUE)) %>% 
#     filter(!is.na(mean_score)) %>% 
#     ggplot() +
#     geom_col(aes(x = reorder(method, mean_score), y = log10(mean_score), fill = set_id)) +
#     ylab("log10 energy") +
#     xlab("method") +
#     coord_flip() +
#     theme_light() +
#     theme(legend.position = "bottom")
# }



# plot_violins <- function(imputation_summary) {
#   imputation_summary %>%
#     filter(measure == "energy") %>%
#     group_by(method) %>%
#     mutate(`success percentage` = cut(mean(is.na(error)) * 100, c(10, 40, 90, 100))) %>%
#     group_by(method, ratio, mechanism) %>%
#     mutate(mean_score = mean(log10(score[!is.na(score)]), na.rm = TRUE)) %>%
#     filter(!is.na(score)) %>%
#     ggplot(aes(x = reorder(method, log10(mean_score)), y = log10(score), fill = `success percentage`)) +
#     geom_violin(alpha = 0.6) +
#     geom_point(mapping = aes(x = reorder(method, (mean_score)), y = mean_score), size = 3, shape = 8) +
#     ylab("log10 energy") +
#     xlab("method") +
#     coord_flip() +
#     theme_light() +
#     facet_grid(mechanism~ratio)
# }


show_amputation <- function(amputation_summary) {
  
  amputation_summary %>% 
    filter(!is.na(ratio)) %>% 
    ggplot(aes(x = ratio, y = amputed_ratio, col = mechanism)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    xlab("assumed ratio") +
    ylab("obtained ratio") +
    theme_light()
  
}




plot_progress <- function(imputation_summary) {
  
  imputation_summary %>% 
    mutate(complete = !is.na(measure)) %>% 
    select(method, set_id, mechanism, ratio, rep, complete) %>% 
    unique() %>% 
    ggplot() +
    geom_tile(aes(x = method, y = set_id, fill = complete)) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_grid(ratio + mechanism ~ rep) +
    ylab("dataset") +
    scale_fill_manual("Finished", values = c("snow3", "springgreen4"))
  
}






plot_rankings <- function(imputation_summary, breaks = c(1, 3, 10, 30)) {
  
  breaks <- c(breaks, length(unique(imputation_summary$method)))
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  imputation_summary %>%
    filter(!is.na(measure)) %>% 
    select(-imputation_fun) %>% 
    filter(case == "complete", measure == "energy_std") %>% 
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
    mutate(ranking = cut(ranking, breaks = breaks, include.lowest = TRUE)) %>% 
    group_by(method, ranking) %>% 
    reframe(n = n(), mean_ranking = mean_ranking) %>% 
    unique() %>% 
    ggplot(aes(y = reorder(method, mean_ranking), x = n, fill = ranking)) +
    geom_col() +
    scale_fill_manual(name = "Ranking", values = get_colors_ranks()) +
    xlab("simulation cases") +
    ylab("method") +
    theme_light() +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 90))
  
}


plot_all_measures <- function(imputation_summary, breaks = c(1, 3, 10, 30)) {
  
  breaks <- c(breaks, length(unique(imputation_summary$method)))
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  imputation_summary %>%
    select(-imputation_fun) %>% 
    filter(case == "complete") %>% 
    unique() %>% 
    mutate(score = ifelse(measure == "rsq", -score, score),
           score = ifelse(measure == "ccc", -score, score)) %>% 
    filter(measure != "IScore", measure != "rmse") %>% 
    group_by(method, set_id, mechanism, ratio, measure) %>% 
    reframe(score = mean(score, na.rm = TRUE), 
            error = ifelse(any(is.na(error)), NA, unique(error)[1])) %>% 
    unique() %>% 
    mutate(case_id = paste0(set_id, mechanism, ratio)) %>% 
    mutate(score = ifelse(is.nan(score), NA, score)) %>% 
    group_by(set_id, mechanism, ratio, measure) %>% 
    mutate(ranking =  {
      ranking <- rep(NA, length(score))
      ranking[!is.na(score)] <- rank(score[!is.na(score)])
      ranking[is.na(ranking)] <- n_methods
      ranking[is.na(score) & is.na(error)] <- NA
      ranking
    }) %>% 
    group_by(method, measure) %>% 
    summarise(ranking = mean(ranking, na.rm = TRUE)) %>% 
    group_by(method) %>% 
    mutate(mean_ranking = mean(ranking, na.rm = TRUE),
           energy_total = ranking[measure == "energy_std"]) %>% 
    ggplot() +
    geom_tile(aes(x = measure, y = reorder(method, energy_total), fill = ranking))
    
}






plot_energy_time_ranking <- function(arrange_success = TRUE, breaks = c(0, 1, 40, 80, 99, 100)) {
  
  
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  dat_plt <- imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-imputation_fun, -attempts) %>% 
    # filter(!(set_id %in% c("oes10", "scm1d", "scm20d"))) %>% 
    filter(case == "complete", measure == "energy_std") %>%
    unique() %>% 
    group_by(method) %>% 
    mutate(`success [%]` = mean(is.na(error)) * 100) %>% 
    group_by(method, set_id, mechanism, ratio) %>% 
    mutate(score = mean(score, na.rm = TRUE),
           time = mean(time, na.rm = TRUE)) %>% 
    select(-rep, -case, -error) %>% 
    unique() %>% 
    mutate(score = ifelse(is.nan(score), NA, score)) %>% 
    ungroup() %>% 
    group_by(set_id, mechanism, ratio) %>% 
    mutate(ranking =  {
      ranking <- rep(NA, length(score))
      ranking[!is.na(score)] <- rank(score[!is.na(score)])
      ranking[is.na(ranking)] <- n_methods
      ranking
    }) %>% 
    ungroup() %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            mean_ranking = mean(ranking, na.rm = TRUE),
            median_ranking = median(ranking, na.rm = TRUE),
            time = mean(time, na.rm = TRUE), 
            `success [%]` = `success [%]`,
            upr_ranking = quantile(ranking, 0.75),
            lwr_ranking = quantile(ranking, 0.25)) %>% 
    mutate(`success [%]` = cut(`success [%]`, breaks, 
                               include.lowest = TRUE)) %>% 
    unique() %>% 
    arrange(mean_score) %>% 
    mutate(method = factor(method, levels = method))
  
  if(arrange_success)
    dat_plt <- dat_plt %>%
    arrange(mean_ranking) %>%
    mutate(method = factor(method, levels = method))
  
  min_time <- min(dat_plt$time) * 1000
  
  p1 <- dat_plt %>% 
    ggplot(aes(x = method, y = time * 1000, fill = `success [%]`)) +
    geom_rect(aes(xmin = as.numeric(method) - 0.4, 
                  xmax = as.numeric(method) + 0.4,
                  ymin = min_time - 10, 
                  ymax = time * 1000, 
                  fill = `success [%]`)) +
    scale_fill_manual(name = "success [%]", values = get_colors_fractions()) +
    labs(x = "Methods", y = "Average Time") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_x_discrete(position = "top") +
    coord_flip() +
    scale_y_continuous("Time", trans = c("log10", "reverse"),
                       breaks = c(min_time/1000, 1, 60, 600, 1800, 1800*2, 1800*4, 1800*6) * 1000, 
                       labels = c("116ms", "1s", "1min", "10min", "30min", "1h", "2h", "3h")) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(color = "black", linetype = "dashed"))
  
  
  p2 <- dat_plt %>% 
    ungroup() %>% 
    mutate(max_score = max(log10(mean_score), na.rm = TRUE)) %>% 
    ggplot(aes(x = method, y = ranking, fill = `success [%]`)) +
    geom_boxplot() +
    geom_segment(mapping = aes(x = method, xend = method, y = lwr_ranking, yend = upr_ranking)) +
    scale_fill_manual(name = "success [%]", 
                      values = get_colors_fractions()) +
    labs(x = "Methods", y = "Mean Energy") +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0.5),
          axis.title.y = element_blank()) +
    ylab("log10energy") +
    coord_flip() +
    geom_text(aes(x = method, y = max_score + 0.5, label = round(mean_ranking, 1)), size = 3)
  
  p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
}






plot_energy_time_ranking <- function(arrange_success = TRUE, breaks = c(0, 1, 40, 80, 99, 100)) {
  
  
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  dat_plt <- imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-imputation_fun, -attempts) %>% 
    # filter(!(set_id %in% c("oes10", "scm1d", "scm20d"))) %>% 
    filter(case == "complete", measure == "energy_std") %>%
    unique() %>% 
    group_by(method) %>% 
    mutate(`success [%]` = mean(is.na(error)) * 100) %>% 
    group_by(method, set_id, mechanism, ratio) %>% 
    mutate(score = mean(score, na.rm = TRUE),
           time = mean(time, na.rm = TRUE)) %>% 
    select(-rep, -case, -error) %>% 
    unique() %>% 
    mutate(score = ifelse(is.nan(score), NA, score)) %>% 
    ungroup() %>% 
    group_by(set_id, mechanism, ratio) %>% 
    mutate(ranking =  {
      ranking <- rep(NA, length(score))
      ranking[!is.na(score)] <- rank(score[!is.na(score)])
      ranking[is.na(ranking)] <- n_methods
      ranking
    }) %>% 
    ungroup() %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            mean_ranking = mean(ranking, na.rm = TRUE),
            median_ranking = median(ranking, na.rm = TRUE),
            time = mean(time, na.rm = TRUE), 
            `success [%]` = `success [%]`,
            ranking = ranking) %>% 
    mutate(`success [%]` = cut(`success [%]`, breaks, 
                               include.lowest = TRUE)) %>% 
    unique() %>% 
    arrange(mean_score) %>% 
    mutate(method = factor(method, levels = unique(method)))
  
  min_time <- min(dat_plt$time) * 1000
  
  p1 <- dat_plt %>% 
    ggplot(aes(x = method, y = time * 1000, fill = `success [%]`)) +
    geom_rect(aes(xmin = as.numeric(method) - 0.4, 
                  xmax = as.numeric(method) + 0.4,
                  ymin = min_time - 10, 
                  ymax = time * 1000, 
                  fill = `success [%]`)) +
    scale_fill_manual(name = "success [%]", values = get_colors_fractions()) +
    labs(x = "Methods", y = "Average Time") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_x_discrete(position = "top") +
    coord_flip() +
    scale_y_continuous("Time", trans = c("log10", "reverse"),
                       breaks = c(min_time/1000, 1, 60, 600, 1800, 1800*2, 1800*4, 1800*6) * 1000, 
                       labels = c("116ms", "1s", "1min", "10min", "30min", "1h", "2h", "3h")) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(color = "black", linetype = "dashed"))
  
  
  p2 <- dat_plt %>% 
    ungroup() %>% 
    ggplot(aes(x = reorder(method, mean_ranking), y = ranking)) +
    geom_boxplot(fill = "gray") +
    geom_point(aes(x = reorder(method, mean_ranking), y = mean_ranking), col = "darkblue", size = 2) +
    scale_fill_manual(name = "success [%]", 
                      values = get_colors_fractions()) +
    labs(x = "Methods", y = "Mean Energy") +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0.5),
          axis.title.y = element_blank()) +
    ylab("log10energy") +
    coord_flip() 
  
  p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
}




shreks_plot <- function(imputation_summary ) {
  
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  imputation_summary %>%
    filter(!is.na(measure)) %>% 
    select(-imputation_fun) %>% 
    filter(case == "complete", measure == "energy_std") %>% 
    unique() %>% 
    group_by(method, set_id, mechanism, ratio) %>% 
    reframe(score = mean(score, na.rm = TRUE)) %>% 
    mutate(case_id = paste0(set_id, mechanism, ratio)) %>% 
    mutate(score = ifelse(is.nan(score), NA, score)) %>% 
    group_by(set_id, mechanism, ratio) %>% 
    mutate(ranking =  {
      ranking <- rep(NA, length(score))
      ranking[!is.na(score)] <- rank(score[!is.na(score)])
      ranking[is.na(ranking)] <- max(ranking, na.rm = TRUE)
      ranking
    }) %>% 
    ungroup() %>% 
    group_by(method) %>% 
    mutate(mean_ranking = mean(ranking, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(mean_ranking) %>% 
    mutate(method = factor(method, levels = unique(method))) %>% 
    ggplot() +
    geom_tile(aes(x = case_id, y = method, fill = ranking), colour = "black") +
    theme(axis.text.x = element_text(angle = 90)) +
    geom_text(aes(x = case_id, y = method, label = ranking)) +
    scale_fill_continuous() +
    guides(fill = guide_colourbar(barwidth = 0.5, barheight = 40)) +
    scale_fill_gradient(low = "darkgreen", high = "white") 
  
}




