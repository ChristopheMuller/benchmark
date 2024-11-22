
library(ggplot2)

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
    geom_col(aes(x = method, y = time)) +
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


