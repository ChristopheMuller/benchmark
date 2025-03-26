
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
  scale_color_manual(values = c("#0D3B66", "#FB3640"),
                     name = "", labels = c("observed", "imputed")) +
  theme_classic() +
  ggtitle("Missing data imputed with mice norm.predict")

p2 <- ggplot(dat_imp_norm, aes(x = X1, y = X2, col = missing)) +
  geom_point() +
  scale_color_manual(values = c("#0D3B66", "#FB3640"),
                     name = "", labels = c("observed", "imputed")) +
  theme_classic() +
  ggtitle("Missing data imputed with mice.norm")

p3 <- ggplot(dat_full, aes(x = X1, y = X2, col = missing)) +
  geom_point() +
  scale_color_manual(values = c("#0D3B66", "springgreen3"),
                     name = "", labels = c("observed", "missing")) +
  theme_classic() +
  ggtitle("True data")

p3 + p1 + p2 + plot_layout(guides = "collect") & 
  guides(color = guide_legend(override.aes = list(size = 5)))



