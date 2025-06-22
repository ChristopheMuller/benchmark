
library(dplyr)
library(tidyr)
library(ggfortify)
library(ggplot2)


imputation_summary <- readRDS("./results/imputation_summary_31_to_42.RDS") %>% 
  rbind(readRDS("./results/imputation_summary_42_only_autocomplete.RDS"))

pca_dat <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter(case == "complete", measure == "energy_std") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  select(method, ranking, case_id) %>% 
  pivot_wider(names_from = case_id, values_from = ranking)


library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(pheatmap)

corr <- cor(scale(pca_dat[, -1]))

pheatmap(corr)

pca_res <- PCA(scale(pca_dat[, -1]))

df <- pca_res$var$cor[, c(1, 2)] %>%
  as.data.frame()

df_ind <- pca_res$ind$coord[, c(1, 2)] %>% 
  as.data.frame() %>%
  mutate(method = unlist(pca_dat[, 1]))

colnames(df)[1:2] <- paste0("dim ", c(1, 2))

df %>%
  mutate(var = rownames(.)) %>%
  mutate(dataset = sapply(var, function(ith) strsplit(ith, " ")[[1]][1])) %>% 
  ggplot(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]), col = dataset)) +
  xlim(0, 1.2) +
  geom_segment(aes(x = 0,
                   y = 0,
                   xend = get(colnames(df)[1]),
                   yend = get(colnames(df)[2])),
               arrow = arrow(), size = 1, alpha = 0.4) +
  geom_text(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]),
                label = var), hjust = -0.2, size = 3) +
  geom_point(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]))) +
  theme_minimal() +
  xlab("Dim 1 (58.2 %)") +
  ylab("Dim 2 (13 %)") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")


ggplot() +
  geom_text(data = df_ind, 
            aes(x = Dim.1, y = Dim.2, label = method), 
            color = "black", size = 4) +
  # xlim(-11, 13) +
  # ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_light() +
  xlab("Dim 1 (58.2 %)") +
  ylab("Dim 2 (13 %)")

performance <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter(case == "complete", measure == "energy_std") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  group_by(method) %>% 
  reframe(mean_ranking = mean(ranking))


df_ind %>% 
  merge(performance) %>% 
  ggplot() +
  geom_text(aes(x = Dim.1, y = Dim.2, label = method, colour = mean_ranking), size = 4) +
  # xlim(-11, 13) +
  # ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_light() +
  xlab("Dim 1 (58.2 %)") +
  ylab("Dim 2 (13 %)") +
  scale_color_continuous("mean\nranking")

##########################

performance_on_large <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter(case == "complete", measure == "energy_std",
         set_id %in% c("oes10", "scm1d")) %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  group_by(method) %>% 
  reframe(mean_ranking = mean(ranking))


df_ind %>% 
  merge(performance_on_large) %>% 
  ggplot() +
  geom_text(aes(x = Dim.1, y = Dim.2, label = method, col = mean_ranking), size = 4) +
  # xlim(-11, 13) +
  # ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_light() +
  xlab("Dim 1 (58.2 %)") +
  ylab("Dim 2 (13 %)") +
  scale_color_continuous("mean ranking on\nlarge datasets")

performance_on_large %>% 
  ggplot() +
  geom_col(aes(x = mean_ranking, y = reorder(method, mean_ranking))) +
  ylab("method") +
  xlab("mean ranking on large daasets")

###################################

performance_on_large <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter(case == "complete", measure == "energy_std",
         !(set_id %in% c("oes10", "scm1d"))) %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  group_by(method) %>% 
  reframe(mean_ranking = mean(ranking))


df_ind %>% 
  merge(performance_on_large) %>% 
  ggplot() +
  geom_text(aes(x = Dim.1, y = Dim.2, label = method, col = mean_ranking), size = 4) +
  # xlim(-11, 13) +
  # ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_light() +
  xlab("Dim 1 (58.2 %)") +
  ylab("Dim 2 (13 %)") +
  scale_color_continuous("mean ranking on\nlarge datasets")

###################################

pc2_correlations <- pca_res$var$cor %>% 
  as.data.frame() %>% 
  arrange(-Dim.2)

pc2_correlations_sorted <- sort(pc2_correlations, decreasing = TRUE)

head(pc2_correlations_sorted, 10)

source("./data/get_datasets_dim.R")

sets_dim <- readRDS("./data/datasets/sets_dim.RDS")

df %>%
  mutate(var = rownames(.)) %>%
  mutate(set_id = sapply(var, function(ith) strsplit(ith, " ")[[1]][1])) %>% 
  merge(sets_dim) %>% 
  mutate(n_columns = cut(n_col, breaks = c(1, 15, 76, 112, 265, 314))) %>% 
  ggplot(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]), col = n_columns)) +
  geom_segment(aes(x = 0,
                   y = 0,
                   xend = get(colnames(df)[1]),
                   yend = get(colnames(df)[2])),
               arrow = arrow(), size = 1, alpha = 0.4) +
  geom_text(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]),
                label = var), hjust = "right") +
  geom_point(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]))) +
  theme_minimal() +
  xlab("Dim 1 (58.2 %)") +
  ylab("Dim 2 (13 %)") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")






