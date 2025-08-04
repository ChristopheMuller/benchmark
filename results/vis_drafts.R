


imputation_summary  %>% 
  filter(!is.na(measure)) %>% 
  filter(method == "mice_cart") %>% 
  filter(set_id == "allergens") %>% 
  select(-measure, -score) %>% 
  unique()


imputation_summary %>% 
  filter(set_id == "allergens") %>% 
  filter(!is.na(measure)) %>% 
  select(-measure, -score, -imputation_fun) %>% 
  unique() %>% 
  mutate(id = paste0(set_id, mechanism, ratio, rep, case, method)) %>% 
  filter(time < 750, time > 600) %>% 
  pull(method) %>% 
  unique()



imputation_summary %>% 
  filter(set_id == "allergens") %>% 
  filter(!is.na(measure)) %>% 
  select(-measure, -score, -imputation_fun) %>% 
  unique() %>% 
  mutate(id = paste0(set_id, mechanism, ratio, rep, case, method)) %>% 
  mutate(mice_drf = method == "mice_drf") %>% 
  mutate(timeout = error %in% "timeout") %>% 
  select(id, mice_drf, time, timeout) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = reorder(id, time), y = time, fill = timeout)) +
  coord_flip() +
  geom_hline(aes(yintercept = 1200, color = "time threshold"), linetype = "dashed") +
  scale_colour_manual(name = "", values = c(`time threshold` = "black")) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab("imputation id") +
  geom_hline(aes(yintercept = 600, color = "time threshold"), linetype = "dashed")


imputation_summary %>% 
  filter(set_id == "allergens") %>% 
  filter(!is.na(measure)) %>% 
  select(-measure, -score, -imputation_fun) %>% 
  unique() %>% 
  mutate(id = paste0(set_id, mechanism, ratio, rep, case, method)) %>% 
  mutate(mice_drf = method == "mice_cart") %>% 
  mutate(timeout = error %in% "timeout") %>% 
  select(id, mice_drf, time, timeout) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = reorder(id, time), y = time, fill = mice_drf)) +
  coord_flip() +
  geom_hline(aes(yintercept = 1200, color = "time threshold"), linetype = "dashed") +
  scale_colour_manual(name = "", values = c(`time threshold` = "black")) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab("imputation id")






