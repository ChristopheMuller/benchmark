

plot_errors <- function(imputation_summary) {
  
  imputation_summary %>% 
    filter(case == "complete") %>% 
    select(-measure, -score) %>% 
    unique() %>% 
    group_by(method) %>% 
    mutate(n_attempts = n()) %>% 
    group_by(method, error) %>% 
    reframe(error_frac = n()/n_attempts) %>% 
    unique() %>% 
    ggplot() + 
    geom_col(aes(x = method, y = error_frac, fill = error)) +
    ylim(0, 1) +
    scale_fill_discrete(name = "Type of error", labels = c("computational", 
                                                           "modification", 
                                                           "none")) +
    coord_flip()
  
}

plot_time <- function(imputation_summary) {
  
  imputation_summary %>% 
    select(-measure, -score, -imputation_fun) %>% 
    unique() %>% 
    group_by(method) %>% 
    reframe(time = mean(time, na.rm = TRUE)) %>% 
    ggplot() + 
    geom_col(aes(x = reorder(method, time), y = time)) +
    coord_flip()
  
}


plot_score <- function(imputation_summary, 
                       measure_name = "energy", 
                       which_case = "complete") {
  imputation_summary %>% 
    filter(measure == measure_name, case == which_case) %>% 
    select(set_id, mechanism, method, score) %>% 
    group_by(set_id, method) %>% 
    reframe(score = mean(score)) %>% 
    ggplot() +
    geom_tile(aes(x = method, y = set_id, fill = score)) +
    coord_flip() +
    ggtitle("energy dist")
}


plot_averaged <- function(imputation_summary, measure_name = "energy") {
  imputation_summary %>% 
    filter(measure == measure_name) %>% 
    group_by(method, measure) %>% 
    reframe(mean_score = mean(score, na.rm = TRUE),
            mean_time = mean(time)) %>% 
    filter(!is.na(mean_score)) %>% 
    ggplot() +
    geom_col(aes(x = reorder(method, mean_score), y = log10(mean_score))) +
    ylab(paste0("log10", measure_name)) +
    xlab("method") +
    coord_flip() 
}






