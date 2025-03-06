
library(dplyr)
library(tidyr)
library(ggfortify)
library(ggplot2)


imputation_summary <- readRDS("./results/imputation_summary_31_to_42.RDS")


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
    ranking[is.na(ranking)] <- n_methods
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


ggplot() +
  geom_text(data = df_ind, 
             aes(x = Dim.1, y = Dim.2, label = method), 
             color = "black", size = 4) +
  xlim(-11, 13) +
  ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_light() +
  xlab("Dim 1 (58.2 %)") +
  ylab("Dim 2 (13 %)")



pc2_correlations <- pca_res$var$cor %>% 
  as.data.frame() %>% 
  arrange(-Dim.1)

pc2_correlations_sorted <- sort(pc2_correlations, decreasing = TRUE)

head(pc2_correlations_sorted, 10)



