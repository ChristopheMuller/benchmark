
library(dplyr)
library(ggplot2)
library(patchwork)

draw_one_plot <- function(imputed, miss_col, title, observed) {
  
  rmse_val <- round(sqrt(mean((imputed[is.na(miss_col), 2] - observed[is.na(miss_col), 2])^2)), 3)
  energy_val <- round(energy_dist(observed, imputed), 3)
  
  imputed %>% 
    as.data.frame() %>% 
    mutate(missing = is.na(miss_col)) %>% 
    ggplot(aes(x = X1, y = X2, col = missing)) +
    geom_point() +
    scale_color_manual(values = c("#0D3B66", "#FB3640"),
                       name = "", labels = c("observed", "imputed")) +
    theme_classic() +
    ggtitle(title) +
    theme(legend.position = "none") +
    annotate(
      geom = "text", x = 5, y = -6, 
      label = paste0("energy: ", energy_val), hjust = 0, vjust = 1,
      col = "black", size = 4
    ) +
    annotate(
      geom = "text", x = 5, y = -7.5, 
      label = paste0("RMSE: ", rmse_val), hjust = 0, vjust = 1,
      col = "black", size = 4
    ) 
    
}

###############333

set.seed(10)

dat <- MASS::mvrnorm(1000, mu = c(0, 0), Sigma = diag(2, 2) + 5)
colnames(dat) <- c("X1", "X2")
dat_full <- dat

dat[dat[, 1] > 0 & runif(1000) > 0.5, 2] <- NA 

dat_full_plt <- dat_full %>% 
  as.data.frame() %>% 
  mutate(missing = is.na(dat[, 2]))


dat_imp_norm.predict <- mice::complete(mice::mice(dat, m = 1, method = "norm.predict"))
dat_imp_mean <- mice::complete(mice::mice(dat, m = 1, method = "mean"))
dat_imp_norm <- mice::complete(mice::mice(dat, m = 1, method = "norm"))


p1 <- draw_one_plot(dat_imp_mean, dat[, 2], "Mean imputation", dat_full)
p4 <- draw_one_plot(dat_imp_norm.predict, dat[, 2], "Regression imputation", dat_full)
p2 <- draw_one_plot(dat_imp_norm, dat[, 2], "Gaussian imputation", dat_full)


p3 <- ggplot(dat_full_plt, aes(x = X1, y = X2, col = missing)) +
  geom_point() +
  scale_color_manual(values = c("#0D3B66", "springgreen3"),
                     name = "", labels = c("observed", "missing")) +
  theme_classic() +
  ggtitle("True data") +
  theme(legend.position = "none")


p3 + p1 + p2 + p4 + plot_layout(guides = "collect") & 
  guides(color = guide_legend(override.aes = list(size = 5)))



