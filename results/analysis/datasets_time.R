
library(ggplot2)
library(dplyr)
library(patchwork)

p1 <- imputation_summary %>% 
  filter(!is.na(measure)) %>% 
  select(set_id, time, ratio) %>% 
  mutate(ratio = as.character(ratio)) %>% 
  unique() %>% 
  group_by(set_id, ratio) %>% 
  reframe(avg_time = mean(time)) %>% 
  ggplot(aes(x = reorder(set_id, -avg_time), y = avg_time, fill = ratio)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(
    name = "Averaged imputation time",
    trans = "log10",
    breaks = c(4, 10, 60, 1200, 3600, 7200),
    labels = c("4s", "10s", "1min", "20min", "1h", "2h")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  scale_fill_manual("Missingness proportion", values = c("0.1" = "indianred1", "0.2" = "indianred3", "0.3" = "indianred4"))


p2 <- imputation_summary %>% 
  filter(!is.na(measure)) %>% 
  select(set_id, time, mechanism) %>% 
  unique() %>% 
  group_by(set_id, mechanism) %>% 
  reframe(avg_time = mean(time)) %>% 
  ggplot(aes(x = reorder(set_id, -avg_time), y = avg_time, fill = mechanism)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(
    name = "Averaged imputation time",
    trans = "log10",
    breaks = c(4, 10, 60, 1200, 3600, 7200),
    labels = c("4s", "10s", "1min", "20min", "1h", "2h")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_fill_manual("Mechanism", values = c("mar" = "slateblue3", "mcar" = "orange1"),
                    labels = c("mar" = "MAR", "mcar" = "MCAR"))

(p1 | p2) + plot_annotation(tag_levels = 'A')

################################################################################

n_methods <- length(unique(pull(imputation_summary, method)))

dat_plt <- imputation_summary %>% 
  filter(!is.na(measure)) %>% 
  select(-imputation_fun, -attempts) %>% 
  # filter(!(set_id %in% c("oes10", "scm1d", "scm20d"))) %>% 
  filter(case == "complete", measure == "energy_std") %>%
  unique() %>% 
  group_by(method) %>% 
  mutate(`success [%]` = mean(is.na(error)) * 100) %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  mutate(score = mean(score, na.rm = TRUE),
         time = mean(time, na.rm = TRUE)) %>% 
  select(-rep, -case, -error) %>% 
  unique() %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  ungroup() %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- n_methods
    ranking
  }) %>% 
  ungroup() %>% 
  group_by(method) %>% 
  reframe(mean_score = mean(score, na.rm = TRUE),
          mean_ranking = mean(ranking, na.rm = TRUE),
          median_ranking = median(ranking, na.rm = TRUE),
          time = mean(time, na.rm = TRUE), 
          `success [%]` = `success [%]`) %>% 
  unique() %>% 
  arrange(mean_score) %>% 
  mutate(method = factor(method, levels = method))


dat_plt %>% 
  ggplot(aes(x = reorder(method, `success [%]`), y = time * 1000, fill = `success [%]`)) +
  geom_col() +
  theme_minimal() +
  scale_y_continuous(
    name = "Averaged imputation time",
    trans = "log10",
    breaks = c(1, 10, 60, 1200, 3600, 10800) * 1000,
    labels = c("1s", "10s", "1min", "20min", "1h", "3h")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())


####################################
  
