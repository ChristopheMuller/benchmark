
tar_source()


imputation_summary <- readRDS("./results/imputation_summary.RDS") %>% 
  mutate(case_id = paste0(set_id, mechanism, ratio),
         New = TRUE) 


imputation_summary <- readRDS("./results/imputation_summary_benchmark.RDS") %>% 
  mutate(case_id = paste0(set_id, mechanism, ratio), 
         New = FALSE) %>% 
  filter(case_id %in% unique(pull(imputation_summary, case_id))) %>% 
  rbind(imputation_summary) %>% 
  select(-case_id) %>% 
  filter(case == "complete")


imputation_summary %>% 
  filter(method == "my_method")


imputation_summary %>%  shreks_plot()

