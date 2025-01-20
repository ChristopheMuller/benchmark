
library(targets)
library(stringr)
library(dplyr)

targets_tbl <- tar_progress()

params <- readRDS("./data/params.RDS")

complete_targets <- targets_tbl %>% 
  filter(progress %in% c("completed", "skipped")) %>% 
  filter(str_starts(name, "obtained_scores_")) %>% 
  pull(name)

all_scores <- lapply(complete_targets, function(ith_case) {
  print(paste0(which(ith_case == complete_targets), " / ", length(complete_targets)))
  tar_load(!!ith_case)
  get(ith_case)
}) %>% 
  bind_rows()

params %>% 
  left_join(all_scores, by = "imputed_id") %>% 
  dplyr::select(set_id, mechanism, ratio, rep, case, method, imputation_fun, 
                time, attempts, error, measure, score) %>% 
  saveRDS("./results/imputation_summary_partial.RDS")