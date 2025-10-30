
library(dplyr)
library(tidyr)


tbl <- expand.grid(error = c("computational",  "modification", 
                             "timeout", "missings", "none"))

imputation_summary %>% 
  filter(!is.na(measure)) %>% 
  select(-measure, -score) %>% 
  unique() %>% 
  group_by(method) %>% 
  mutate(n_attempts = n()) %>% 
  mutate(error = ifelse(is.na(error), "none", error)) %>% 
  group_by(error) %>% 
  reframe(n = 100* n() / nrow(.)) %>% 
  filter(error != "none") %>%  pull(n) %>%  sum()


imputation_summary %>% 
  filter(!is.na(error)) %>% 
  pull(method) %>%  unique() %>%  length()



imputation_summary %>% ungroup() %>% 
  filter(!is.na(measure)) %>% 
  select(-measure, -score) %>% 
  unique() %>% 
  mutate(error = ifelse(is.na(error), "none", error)) %>% 
  filter(attempts == 2, error == "none") %>%  nrow()


imputation_summary %>% ungroup() %>% 
  filter(!is.na(measure)) %>% 
  select(-measure, -score) %>% 
  unique() %>% 
  mutate(error = ifelse(is.na(error), "none", error)) %>% 
  filter(attempts == 2, error == "none") %>% 
  pull(method) %>%  unique()

