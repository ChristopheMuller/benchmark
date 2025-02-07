
get_colors_errors <- function() {
  c("computational" = "#7A4E7E", "modification" = "#A3A725", 
    "timeout" = "#1E9CC2", "missings" = "#3D6649", "none" = "#EDF2EF")
}

get_colors_fractions <- function() {
  c("[0,1]" = "#C9C3D5", "(1,40]"= "#9E94B3", "(40,80]" = "#7E7099", "(80,99]" = "#615577", "(99,100]" = "#443B54")
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



plot_time <- function(imputation_summary, timeout = 600, breaks = c(0, 1, 40, 80, 99, 100)) {
  
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
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-imputation_fun) %>% 
    filter(case == "complete", measure == "energy_std") %>% 
    unique() %>% 
    group_by(method, set_id, mechanism, ratio) %>% 
    reframe(score = mean(score, na.rm = TRUE)) %>% 
    mutate(case_id = paste0(set_id, mechanism, ratio)) %>% 
    mutate(score = ifelse(is.nan(score), Inf, score)) %>% 
    group_by(case_id) %>% 
    # mutate(best = score == min(score, na.rm = TRUE)) %>% 
    mutate(ranking = order(score, decreasing = FALSE)) %>% 
    group_by(method) %>% 
    mutate(mean_ranking = mean(ranking)) %>% 
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
    filter(case == "complete", measure == "energy_std") %>%
    unique() %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            time = mean(time, na.rm = TRUE),
            `success [%]` = mean(is.na(error)) * 100) %>% 
    mutate(`success [%]` = cut(`success [%]`, breaks, 
                               include.lowest = TRUE)) %>% 
    mutate(is_top = `success [%]` == "(90,100]") %>% 
    arrange(mean_score) %>% 
    mutate(method = factor(method, levels = method))
  
  if(arrange_success)
    dat_plt <- dat_plt %>%  
      arrange(-is_top, mean_score) %>% 
      mutate(method = factor(method, levels = method))
  
  
  p1 <- ggplot(dat_plt, aes(x = method, y = log10(time * 1000), fill = `success [%]`)) +
    geom_col() +
    scale_fill_manual(name = "success [%]", values = get_colors_fractions()) +
    labs(x = "Methods", y = "Average Time") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none") +
    scale_x_discrete(position = "top") +
    coord_flip() +
    scale_y_continuous("log10 time [ms]", trans =  "reverse")
  
  p2 <- ggplot(dat_plt, aes(x = method, y = log10(mean_score), fill = `success [%]`)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(name = "success [%]", 
                      values = get_colors_fractions()) +
    labs(x = "Methods", y = "Mean Energy") +
    theme_bw() +
    theme(axis.text.y = element_text(hjust = 0.5),
          axis.title.y = element_blank()) +
    ylab("log10 energy")
  
  p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
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
    filter(measure == "energy_std", method == "remasker") %>% 
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



