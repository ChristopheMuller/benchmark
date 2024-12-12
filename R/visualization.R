

plot_errors <- function(imputation_summary) {
  
  imputation_summary %>% 
    filter(case == "complete") %>% 
    select(-measure, -score) %>% 
    unique() %>% 
    group_by(method) %>% 
    mutate(n_attempts = n()) %>% 
    mutate(ifelse(is.na(error), "none", error)) %>% 
    rename(`Type of error` = "error") %>% 
    group_by(method, `Type of error`) %>% 
    reframe(error_frac = n()/n_attempts) %>% 
    unique() %>% 
    ggplot() + 
    geom_col(aes(x = method, y = error_frac, fill = `Type of error`)) +
    ylim(0, 1) +
    coord_flip()
}

plot_time <- function(imputation_summary) {
  
  imputation_summary %>% 
    select(-measure, -score, -imputation_fun) %>% 
    unique() %>% 
    group_by(method) %>% 
    reframe(time = mean(time, na.rm = TRUE),
            error = error) %>% 
    ggplot() + 
    geom_col(aes(x = reorder(method, time), y = time, fill = error)) +
    coord_flip()
}


plot_score <- function(imputation_summary, 
                       measure_name = "energy", 
                       which_case = "complete") {
  imputation_summary %>% 
    filter(measure == measure_name, case == which_case) %>% 
    select(mechanism, mechanism, method, score) %>% 
    group_by(mechanism, method) %>% 
    reframe(score = mean(score)) %>% 
    ggplot() +
    geom_tile(aes(x = method, y = mechanism, fill = score)) +
    coord_flip() +
    ggtitle("energy dist")
}


plot_averaged <- function(imputation_summary, measure_name = "energy") {
  imputation_summary %>% 
    filter(measure == measure_name) %>% 
    group_by(method) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            mean_time = mean(time),
            `success percentage` = mean(is.na(error)) * 100) %>% 
    filter(!is.na(mean_score)) %>% 
    mutate(`success percentage` = cut(`success percentage`, c(10, 40, 90, 100))) %>% 
    ggplot() +
    geom_col(aes(x = reorder(method, mean_score), y = log10(mean_score), fill = `success percentage`), alpha = 0.8) +
    ylab(paste0("log10", measure_name)) +
    xlab("method") +
    coord_flip() +
    theme_light() +
    theme(legend.position = "bottom")
}

plot_violins <- function(imputation_summary) {
  imputation_summary %>% 
    filter(measure == "energy") %>% 
    group_by(method) %>% 
    mutate(`success percentage` = cut(mean(is.na(error)) * 100, c(10, 40, 90, 100))) %>% 
    group_by(method, ratio, mechanism) %>% 
    mutate(mean_score = mean(log10(score[!is.na(score)]), na.rm = TRUE)) %>% 
    filter(!is.na(score)) %>% 
    ggplot(aes(x = reorder(method, log10(mean_score)), y = log10(score), fill = `success percentage`)) +
    geom_violin(alpha = 0.6) +
    geom_point(mapping = aes(x = reorder(method, (mean_score)), y = mean_score), size = 3, shape = 8) +
    ylab("log10 energy") +
    xlab("method") +
    coord_flip() +
    theme_light() +
    facet_grid(mechanism~ratio)
}


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





