
library(targets)
library(stringr)
library(dplyr)



read_imputation_summary_files <- function(ids_vec) {
  
  lapply(ids_vec, function(ith_id) { 
    readRDS(paste0("./results/imputation_summary_", ith_id, ".RDS"))
  }) %>% 
    bind_rows()
}


imputation_summary <- read_imputation_summary_files(c(31, 33, 35, 36, 37))

# here we divide energy results by n/2

imputation_summary <- imputation_summary %>% 
  left_join(readRDS("./data/datasets/sets_dim.RDS")) %>% 
  filter(measure %in% c("energy", "energy_std")) %>% 
  mutate(score = score / (n_row / 2))


imputation_summary %>% 
  select(-measure, -score) %>% 
  unique() %>% 
  filter(error == "timeout") %>%  
  saveRDS("./results/imputation_summary_timeouts.RDS")




