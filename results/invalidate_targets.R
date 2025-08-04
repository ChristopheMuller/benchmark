
library(dplyr)


params <- readRDS("./data/params.RDS")

imputation_summary_timeouts <- readRDS("./results/imputation_summary_timeouts.RDS")

imputed_targets <- params %>%
  left_join(readRDS("./results/imputation_summary_timeouts.RDS")) %>%
  filter(!is.na(error)) %>%
  select(-time, -attempts, -error) %>% 
  unique() %>% 
  pull(imputed_id) %>% 
  paste0("imputed_dat_", .)


obtained_scores_targets <- params %>%
  left_join(readRDS("./results/imputation_summary_timeouts.RDS")) %>%
  filter(!is.na(error)) %>%
  select(-time, -attempts, -error) %>% 
  unique() %>% 
  pull(imputed_id)%>% 
  paste0("obtained_scores_", .)

######### invalidate targets

lapply(1:length(imputed_targets), function(ith_target) {
  tar_invalidate(imputed_targets[[ith_target]])
  print(paste0(imputed_targets[[ith_target]], " invalidated"))
  
  tar_invalidate(obtained_scores_targets[[ith_target]])
  print(paste0(obtained_scores_targets[[ith_target]], " invalidated"))
})




