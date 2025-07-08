
library(dplyr)
library(ggplot2)
library(patchwork)

set.seed(10)

dat <- MASS::mvrnorm(1000, mu = c(0, 0), Sigma = diag(2, 2) + 5)
colnames(dat) <- c("X1", "X2")
dat_full <- dat

dat[runif(1000) > 0.5, 2] <- NA 

dat_imp_norm <- mice::mice(dat, m = 1, method = "norm")
dat_imp_norm <- mice::complete(dat_imp_norm) %>% 
  mutate(missing = is.na(dat[, 2]))

dat_imp_norm.predict <- mice::mice(dat, m = 1, method = "norm.predict")
dat_imp_norm.predict <- mice::complete(dat_imp_norm.predict) %>% 
  mutate(missing = is.na(dat[, 2]))

dat_full <- dat_full %>% 
  as.data.frame() %>% 
  mutate(missing = is.na(dat[, 2]))

p1 <- ggplot(dat_imp_norm.predict, aes(x = X1, y = X2, col = missing)) +
  geom_point() +
  geom_point(aes(x = 30, y = 30, color = "new_category"), size = 5, show.legend = TRUE) +
  scale_color_manual(name = "", 
                     values = c("FALSE" = "#0D3B66", 
                                "TRUE" = "#FB3640", 
                                "new_category" = "springgreen3"),
                     labels = c("FALSE" = "observed", 
                                "TRUE" = "imputed", 
                                "new_category" = "true (missing)"),
                     breaks = c("FALSE", "TRUE", "new_category")) +
  theme_minimal(base_size = 14) +
  ggtitle("Missing data imputed by Regression Imputation") +
  xlim(min(dat_imp_norm.predict$X1), max(dat_imp_norm.predict$X1)) +
  ylim(min(dat_imp_norm.predict$X2), max(dat_imp_norm.predict$X2)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

p2 <- ggplot(dat_imp_norm, aes(x = X1, y = X2, col = missing)) +
  geom_point() +
  theme_minimal(base_size = 14) +
  ggtitle("Missing data imputed by Gaussian Imputation") +
  geom_point(aes(x = 30, y = 30, color = "new_category"), size = 5, show.legend = TRUE) +
  scale_color_manual(name = "", 
                     values = c("FALSE" = "#0D3B66", 
                                "TRUE" = "#FB3640", 
                                "new_category" = "springgreen3"),
                     labels = c("FALSE" = "observed", 
                                "TRUE" = "imputed", 
                                "new_category" = "true (missing)"),
                     breaks = c("FALSE", "TRUE", "new_category")) +
  xlim(min(dat_imp_norm$X1), max(dat_imp_norm$X1)) +
  ylim(min(dat_imp_norm$X2), max(dat_imp_norm$X2)) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

p3 <- ggplot(dat_full, aes(x = X1, y = X2, col = missing)) +
  geom_point() +
  scale_color_manual(values = c("#0D3B66", "springgreen3"),
                     name = "", labels = c("observed", "missing")) +
  theme_minimal(base_size = 14) +
  ggtitle("True data") +
  theme(legend.position = "none")

((p3 + p1 + p2 + plot_layout(guides = "collect")) & 
  guides(color = guide_legend(override.aes = list(size = 5)))) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 20, face = "bold"))



