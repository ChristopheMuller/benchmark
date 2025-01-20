
get_colors_errors <- function() {
  c("computational" = "#FB3640", "modification" = "#D0E37F", 
    "timeout" = "#429EA6", "missings" = "#0D3B66", "none" = "#BDB4BF")
}

get_colors_fractions <- function() {
  c("[0,40]"= "#96E7EE", "(40,80]" = "#50D6E2", "(80,99]" = "#1FB4C1", "(99,100]" = "#12666E")
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



plot_time <- function(imputation_summary, timeout = 600) {
  
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-measure, -score, -imputation_fun) %>% 
    filter(case == "complete") %>% 
    unique() %>% 
    group_by(method) %>% 
    reframe(time = mean(time, na.rm = TRUE),
            `success [%]` = mean(is.na(error)) * 100) %>% 
    mutate(`success [%]` = cut(`success [%]`, c(0, 40, 80, 99, 100), 
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




plot_energy_time <- function(arrange_success = TRUE) {
  
  dat_plt <- imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-imputation_fun) %>% 
    filter(case == "complete", measure == "energy") %>%
    unique() %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            time = mean(time, na.rm = TRUE),
            `success [%]` = mean(is.na(error)) * 100) %>% 
    mutate(`success [%]` = cut(`success [%]`, c(0, 40, 80, 99, 100), 
                               include.lowest = TRUE)) %>% 
    mutate(is_top = `success [%]` == "(99,100]") %>% 
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


# plot_score <- function(imputation_summary, 
#                        measure_name = "energy", 
#                        which_case = "complete") {
#   imputation_summary %>% 
#     filter(measure == measure_name, case == which_case) %>% 
#     select(mechanism, mechanism, method, score) %>% 
#     group_by(mechanism, method) %>% 
#     reframe(score = mean(score)) %>% 
#     ggplot() +
#     geom_tile(aes(x = method, y = mechanism, fill = score)) +
#     coord_flip() +
#     ggtitle("energy dist")
# }


plot_averaged_energy <- function(imputation_summary) {
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    filter(measure == "energy") %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            mean_time = mean(time),
            `success percentage` = mean(is.na(error)) * 100) %>% 
    filter(!is.na(mean_score)) %>% 
    mutate(`success percentage` = cut(`success percentage`, c(0, 40, 90, 100))) %>% 
    ggplot() +
    geom_col(aes(x = reorder(method, mean_score), y = log10(mean_score), fill = `success percentage`)) +
    scale_fill_manual(name = "successful imputation [%]", values = get_colors_fractions()) +
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


