
get_colors_errors <- function() {
  c("computational" = "#7A4E7E", "modification" = "#A3A725", 
    "timeout" = "#1E9CC2", "missings" = "#3D6649", "none" = "#EDF2EF")
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



plot_time <- function(imputation_summary, timeout = 10 * 60 * 60, breaks = c(0, 1, 40, 80, 99, 100)) {
  
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-measure, -score, -imputation_fun) %>% 
    filter(case == "complete") %>% 
    unique() %>% 
    group_by(method) %>% 
    reframe(time = mean(time, na.rm = TRUE),
            `success [%]` = mean(is.na(error)) * 100) %>% 
    mutate(`success [%]` = cut(`success [%]`, breaks, 
                               include.lowest = TRUE)) %>% 
    ggplot() + 
    geom_col(aes(x = reorder(method, time), y = time, fill = `success [%]`)) +
    coord_flip() +
    xlab("methods") +
    geom_hline(aes(yintercept = timeout, color = "time threshold"), linetype = "dashed") +
    scale_colour_manual(name = "", values = c(`time threshold` = "black")) +
    scale_fill_manual(name = "success [%]", values = get_colors_fractions()) +
    theme_minimal() +
    guides(fill = guide_legend(order=2))

}


plot_time <- function(imputation_summary, timeout = 600) {
  
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-measure, -score, -imputation_fun) %>% 
    filter(case == "complete") %>% 
    unique() %>% 
    group_by(method, set_id) %>% 
    reframe(time = mean(time, na.rm = TRUE)) %>% 
    group_by(set_id) %>% 
    mutate(mean_set_time = mean(time)) %>% 
    ungroup() %>% 
    arrange(method) %>% 
    mutate(method = factor(method, levels = unique(method))) %>% 
    ggplot() + 
    geom_tile(aes(x = method, y = reorder(set_id, mean_set_time), fill = time),
              colour = "black") +
    coord_flip() +
    xlab("methods") +
    ylab("dataset") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 20, hjust = 0.7))
  
}

plot_best <- function(imputation_summary ) {
  
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


plot_energy_time <- function(arrange_success = TRUE, breaks = c(0, 1, 40, 80, 99, 100)) {
  
  dat_plt <- imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-imputation_fun) %>% 
    # filter(!(set_id %in% c("oes10", "scm1d", "scm20d"))) %>% 
    filter(case == "complete", measure == "energy") %>%
    unique() %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            time = mean(time, na.rm = TRUE),
            `success [%]` = mean(is.na(error)) * 100) %>% 
    mutate(`success [%]` = cut(`success [%]`, breaks, 
                               include.lowest = TRUE)) %>% 
    mutate(is_top = `success [%]` == "(99,100]") %>% 
    arrange(mean_score) %>% 
    mutate(method = factor(method, levels = method))
  
  if(arrange_success)
    dat_plt <- dat_plt %>%  
      arrange(-is_top, mean_score) %>% 
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
  
  
  p2 <- ggplot(dat_plt, aes(x = method, y = log10(mean_score), fill = `success [%]`)) +
    geom_col() +
    scale_fill_manual(name = "success [%]", 
                      values = get_colors_fractions()) +
    labs(x = "Methods", y = "Mean Energy") +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0.5),
          axis.title.y = element_blank()) +
    ylab("log10 energy") +
    coord_flip()
  
  p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
}


plot_energy_time_segments <- function(arrange_success = TRUE, breaks = c(0, 1, 40, 80, 99, 100)) {
  
  dat_plt <- imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-imputation_fun) %>% 
    # filter(!(set_id %in% c("oes10", "scm1d", "scm20d"))) %>% 
    filter(case == "complete", measure == "energy_std") %>%
    unique() %>% 
    group_by(method) %>% 
    reframe(mean_score = median(score, na.rm = TRUE),
            time = mean(time, na.rm = TRUE),
            `success [%]` = mean(is.na(error)) * 100) %>% 
    mutate(`success [%]` = cut(`success [%]`, breaks, 
                               include.lowest = TRUE)) %>% 
    mutate(is_top = `success [%]` == "(99,100]") %>% 
    arrange(mean_score) %>% 
    mutate(method = factor(method, levels = method))
  
  if(arrange_success)
    dat_plt <- dat_plt %>%  
      arrange(-is_top, mean_score) %>% 
      mutate(method = factor(method, levels = method))
  
  min_time <- min(dat_plt$time) * 1000
  
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-imputation_fun) %>% 
    filter(case == "complete", measure == "energy_std") %>%
    unique() %>% 
    mutate(score = ifelse(is.na(score), Inf, score)) %>% 
    group_by(method, set_id, mechanism, ratio) %>% 
    reframe(score = mean(score), error = error, set_id = set_id) %>% 
    unique() %>% 
    group_by(set_id, mechanism, ratio) %>% 
    mutate(ranking = order(score, decreasing = FALSE)) %>% 
    mutate(ranking = ifelse(is.infinite(score), n_methods, ranking)) %>% 
    group_by(method, set_id) %>% 
    reframe(median_ranking = median(ranking),
            mean_ranking = mean(ranking),
            lwr = quantile(ranking, 0.25),
            upr = quantile(ranking, 0.75),
            `success [%]` = mean(is.na(error)) * 100) %>% 
    mutate(`success [%]` = cut(`success [%]`, breaks, include.lowest = TRUE)) %>% 
    ggplot() +
    # geom_col(mapping = aes(x = reorder(method, median_ranking), y = median_ranking, fill = `success [%]`)) +
    geom_point(mapping = aes(x = reorder(method, median_ranking), y = median_ranking), col = "black") +
    geom_point(mapping = aes(x = reorder(method, median_ranking), y = mean_ranking), col = "blue") +
    geom_segment(mapping = aes(x = reorder(method, median_ranking), y = lwr, yend = upr)) +
    labs(x = "Methods", y = "Mean Energy") +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0.5),
          axis.title.y = element_blank()) +
    ylab("Ranking") +
    coord_flip() +
    facet_grid(~ set_id)
  
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
  
  
  p2 <- ggplot(dat_plt, aes(x = method, y = log10(mean_score), fill = `success [%]`)) +
    geom_col() +
    scale_fill_manual(name = "success [%]", 
                      values = get_colors_fractions()) +
    labs(x = "Methods", y = "Mean Energy") +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0.5),
          axis.title.y = element_blank()) +
    ylab("log10energy") +
    coord_flip() +
    geom_text(aes(x = method, y = log10(mean_score) + 0.5, label = round(mean_ranking, 1)), size = 3)
  
  p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
}




plot_111234 <- function(arrange_success = TRUE, breaks = c(0, 1, 40, 80, 99, 100)) {
  
  
  n_methods <- length(unique(pull(imputation_summary, method)))
  
  dat_plt <- imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-imputation_fun, -attempts) %>% 
    # filter(!(set_id %in% c("oes10", "scm1d", "scm20d"))) %>% 
    filter(case == "complete", measure == "energy") %>%
    unique() %>% 
    group_by(method) %>% 
    mutate(`success [%]` = mean(is.na(error)) * 100) %>% 
    group_by(method, set_id, mechanism, ratio) %>% 
    mutate(score = mean(score, na.rm = TRUE),
           time = mean(time, na.rm = TRUE)) %>% 
    select(-rep, -case, -error) %>% 
    unique() %>% 
    mutate(score = ifelse(is.nan(score), NA, score)) %>% 
    group_by(set_id, mechanism, ratio) %>% 
    mutate(ranking =  {
      ranking <- rep(NA, length(score))
      ranking[!is.na(score)] <- order(score[!is.na(score)])
      ranking[is.na(ranking)] <- n_methods
      ranking
    }) %>% 
    group_by(method) %>% 
    mutate(mean_score_total = mean(score, na.rm = TRUE)) %>% 
    group_by(method, set_id) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            mean_ranking = mean(ranking, na.rm = TRUE),
            median_ranking = median(ranking, na.rm = TRUE),
            time = mean(time, na.rm = TRUE), 
            `success [%]` = `success [%]`,
            set_id = set_id,
            mean_score_total = mean_score_total) %>% 
    mutate(`success [%]` = cut(`success [%]`, breaks, 
                               include.lowest = TRUE)) %>% 
    unique() %>% 
    arrange(mean_score) %>% 
    mutate(method = factor(method, levels = unique(method)))
  
  if(arrange_success)
    dat_plt <- dat_plt %>%
    arrange(mean_score_total) %>%
    mutate(method = factor(method, levels = unique(method)))
  
  min_time <- min(dat_plt$time) * 1000
  
  p1 <- dat_plt %>% 
    mutate(time = time * 1000) %>% 
    ggplot() +
    geom_col(aes(x = method, y = log10(time), fill = set_id)) +
    scale_fill_manual(name = "dataset", values = get_colors_datasets()) +
    labs(x = "Methods", y = "Average Time") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_x_discrete(position = "top") +
    coord_flip() +
    scale_y_continuous("log10 time", trans = c("reverse"))
  
  
  p2 <- ggplot(dat_plt, aes(x = method, y = log10(mean_score), fill = set_id)) +
    geom_col() +
    scale_fill_manual(name = "dataset", values = get_colors_datasets()) +
    labs(x = "Methods", y = "Mean Energy") +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0.5),
          axis.title.y = element_blank()) +
    ylab("log10energy") +
    coord_flip()
  
  p1 + p2 + plot_layout(guides = "collect")  & theme(legend.position = 'bottom')
  
}








plot_score_tile_dataset <- function(imputation_summary) {
  
  datasets <- unique(imputation_summary[["set_id"]])
  
  imputation_summary_tmp <- imputation_summary %>%
    filter(measure == "energy_std", case == "complete") %>%
    select(set_id, method, score) %>%
    group_by(set_id, method) %>%
    reframe(score = mean(score, na.rm = TRUE)) %>% 
    mutate(score = log10(score))
  
  p <- list()
  
  p1 <- imputation_summary_tmp %>%
    filter(set_id == datasets[1]) %>% 
    ggplot() +
    geom_tile(aes(x = method, y = 1, fill = score), color = "white", linetype = 1) +
    scale_fill_gradient(low = "darkolivegreen2", high = "firebrick4", na.value = "darkgrey") +
    coord_flip() +
    ggtitle("energy dist") +
    theme_minimal() +
    theme(legend.position = "bottom", 
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          legend.text = element_text(angle = 90),
          legend.title=element_blank()) +
    ggtitle(datasets[1])
  
  
  
  for(i in datasets[-1]) {
    p[[i]] <- imputation_summary_tmp %>%
      filter(set_id == i) %>% 
      ggplot() +
      geom_tile(aes(x = method, y = 1, fill = score), color = "white", linetype = 1) +
      scale_fill_gradient(low = "darkolivegreen2", high = "firebrick4", na.value = "darkgrey") +
      coord_flip() +
      ggtitle("energy dist") +
      theme_minimal() +
      theme(legend.position = "bottom", 
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(angle = 90)) +
      ggtitle(i)
  }
  
  p <- c(list(p1), p)
  
  patchwork::wrap_plots(p, nrow = 1) + plot_annotation('log10 energy std')
  
  
}


plot_score_tile_pattern <- function(imputation_summary) {
  
  patterns <- unique(imputation_summary$mechanism)
  
  imputation_summary_tmp <- imputation_summary %>%
    filter(measure == "energy_std", case == "complete") %>%
    select(mechanism, method, score) %>%
    group_by(mechanism, method) %>%
    reframe(score = mean(score, na.rm = TRUE)) %>% 
    mutate(score = log10(score))
  
  p <- list()
  
  p1 <- imputation_summary_tmp %>%
    filter(mechanism == "mcar") %>% 
    ggplot() +
    geom_tile(aes(x = method, y = 1, fill = score), color = "white", linetype = 1) +
    scale_fill_gradient(low = "darkolivegreen2", high = "firebrick4", na.value = "darkgrey") +
    coord_flip() +
    ggtitle("energy dist") +
    theme_minimal() +
    theme(legend.position = "bottom", 
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          legend.text = element_text(angle = 90),
          legend.title=element_blank()) +
    ggtitle("mcar")
  
  p2 <- imputation_summary_tmp %>%
    filter(mechanism == "mar") %>% 
    ggplot() +
    geom_tile(aes(x = method, y = 1, fill = score), color = "white", linetype = 1) +
    scale_fill_gradient(low = "darkolivegreen2", high = "firebrick4", na.value = "darkgrey") +
    coord_flip() +
    ggtitle("energy dist") +
    theme_minimal() +
    theme(legend.position = "bottom", 
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(angle = 90)) +
    ggtitle("mar")
  
  
  p1 + p2 + plot_annotation('log10 energy std')
  
  
}



plot_averaged_energy <- function(imputation_summary) {
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    filter(measure == "energy_std") %>% 
    group_by(method, set_id) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE)) %>% 
    filter(!is.na(mean_score)) %>% 
    ggplot() +
    geom_col(aes(x = reorder(method, mean_score), y = log10(mean_score), fill = set_id)) +
    ylab("log10 energy") +
    xlab("method") +
    coord_flip() +
    theme_light() +
    theme(legend.position = "bottom")
}


plot_averaged_nrmse <- function(imputation_summary) {
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    filter(measure == "nrmse") %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            mean_time = mean(time),
            `success percentage` = mean(is.na(error)) * 100) %>% 
    filter(!is.na(mean_score)) %>% 
    mutate(`success percentage` = cut(`success percentage`, c(0, 40, 90, 100))) %>% 
    ggplot() +
    geom_col(aes(x = reorder(method, mean_score), y = mean_score, fill = `success percentage`)) +
    scale_fill_manual(name = "successful imputation [%]", values = get_colors_fractions()) +
    ylab("NRMSE") +
    xlab("method") +
    coord_flip() +
    theme_light() +
    theme(legend.position = "bottom")
}



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

plot_averaged_colorscale <- function(imputation_summary, measure_name = "energy") {
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    filter(measure == measure_name) %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            mean_time = mean(time),
            error_proportion = mean(!is.na(error))) %>% 
    filter(!is.na(mean_score)) %>% 
    ggplot() +
    geom_col(aes(x = reorder(method, mean_score), y = log10(mean_score), fill = error_proportion), alpha = 0.8) +
    scale_fill_gradient(low = "blue", high = "red", limits = c(0, 1)) +
    ylab(paste0("log10", measure_name)) +
    xlab("method") +
    coord_flip() +
    theme_light() +
    theme(legend.position = "bottom")
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


plot_imputation_one_dim <- function(dim, set.id, methods, mechanism="mcar", ratio="0.1", 
                                    rep="1") {
  
  # if object amputed_all does not exist:
  if (!exists("amputed_all")){
    targets::tar_load("amputed_all")
    print("..loading amputed_all")
  }
  
  # Get amputed data (same for all methods)
  amputed_string <- paste0("amputed_dat_", mechanism, ".", ratio, ".", rep, ".", set.id)
  amputed <- amputed_all[[amputed_string]]
  
  # Get original data (same for all methods)
  original_data_path <- paste0("data/datasets/complete/", set.id, ".RDS")
  original_data <- readRDS(original_data_path)
  
  # Convert to data.frame
  original_data <- as.data.frame(original_data)
  amputed <- as.data.frame(amputed)
  
  # Create missing mask
  missing_mask <- is.na(amputed)
  
  # Create list to store plots
  plots <- list()
  
  # Loop through each method and create plots
  for(method in methods) {
    # Get imputed data for current method
    imputed_string <- paste0("imputed_dat_", method, ".", mechanism, ".", ratio, ".", rep, ".", set.id)
    imputed <- targets::tar_read_raw(imputed_string)$imputed
    imputed <- as.data.frame(imputed)
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Original = original_data[,dim],
      Imputed = imputed[,dim],
      Status = factor(ifelse(missing_mask[,dim], "Imputed", "Original"))
    )
    
    # Create plot
    p <- ggplot(plot_data, aes(x = Original, y = Imputed, color = Status)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = c("Original" = "#0072B2", "Imputed" = "#D55E00")) +
      labs(
        title = method,
        x = paste("Original Dimension", dim),
        y = paste("Imputed Dimension", dim)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
    
    plots[[method]] <- p
  }
  
  # Combine plots using patchwork
  combined_plot <- wrap_plots(plots, ncol = length(methods))
  
  # Print dimensions for verification
  cat("Dimensions:\n")
  cat("Original:", dim(original_data), "\n")
  cat("Amputed:", dim(amputed), "\n")
  for(method in methods) {
    imputed_string <- paste0("imputed_dat_", method, ".", mechanism, ".", ratio, ".", rep, ".", set.id)
    imputed <- targets::tar_read_raw(imputed_string)$imputed
    cat(method, ":", dim(imputed), "\n")
  }
  
  return(combined_plot)
}

plot_imputation <- function(dims, set.id, methods, mechanism="mcar", ratio="0.1", 
                            rep="1") {
  
  # if dims is a number => make it a vector of one
  if(is.numeric(dims)) {
    dims <- c(dims)
  }

  if ((length(dims) == 1)){
    return(plot_imputation_one_dim(dims[1], set.id, methods, mechanism, ratio, rep))
  }
  
  # if object amputed_all does not exist:
  if (!exists("amputed_all")){
    targets::tar_load("amputed_all")
    print("..loading amputed_all")
  }
  
  # Get amputed data (same for all methods)
  amputed_string <- paste0("amputed_dat_", mechanism, ".", ratio, ".", rep, ".", set.id)
  amputed <- amputed_all[[amputed_string]]
  
  # Get original data (same for all methods)
  original_data_path <- paste0("data/datasets/complete/", set.id, ".RDS")
  original_data <- readRDS(original_data_path)
  
  # Convert to data.frame
  original_data <- as.data.frame(original_data)
  amputed <- as.data.frame(amputed)
  
  # Create missing mask for both dimensions
  missing_mask <- is.na(amputed[,dims[1]]) | is.na(amputed[,dims[2]])
  
  # Create list to store plots
  plots <- list()
  
  # Loop through each method and create plots
  for(method in methods) {
    # Get imputed data for current method
    imputed_string <- paste0("imputed_dat_", method, ".", mechanism, ".", ratio, ".", rep, ".", set.id)
    imputed <- targets::tar_read_raw(imputed_string)$imputed
    imputed <- as.data.frame(imputed)
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Dim1 = imputed[,dims[1]],
      Dim2 = imputed[,dims[2]],
      Status = factor(ifelse(missing_mask, "Imputed", "Original"))
    )
    
    # Create plot
    p <- ggplot(plot_data, aes(x = Dim1, y = Dim2, color = Status)) +
      geom_point(alpha = 0.6) +
      scale_color_manual(values = c("Original" = "#0072B2", "Imputed" = "#D55E00")) +
      labs(
        title = method,
        x = paste("Dimension", dims[1]),
        y = paste("Dimension", dims[2])
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
    
    plots[[method]] <- p
  }
  
  # Combine plots using patchwork
  combined_plot <- wrap_plots(plots, ncol = length(methods))
  
  # Print dimensions for verification
  cat("Dimensions:\n")
  cat("Original:", dim(original_data), "\n")
  cat("Amputed:", dim(amputed), "\n")
  for(method in methods) {
    imputed_string <- paste0("imputed_dat_", method, ".", mechanism, ".", ratio, ".", rep, ".", set.id)
    imputed <- targets::tar_read_raw(imputed_string)$imputed
    cat(method, ":", dim(imputed), "\n")
  }
  
  return(combined_plot)
}



