library(ggplot2)
library(dplyr)
library(patchwork)
library(GGally)

missdf <- readRDS("./results/amputed/mar.0.1.1.slump.RDS")
df <- readRDS("./data/datasets/complete/slump.RDS")


which.max(colMeans(is.na(missdf)))

imputed <- impute_mice_cart(missdf)

missdf_1 <- as.data.frame(as.matrix(missdf) + matrix(rnorm(missdf, 0, 1e-6), nrow = nrow(missdf), ncol = ncol(missdf)))
imputed_1 <- impute_mice_cart(missdf_1)

miceDRF::energy_dist(scale(df), scale(imputed))
miceDRF::energy_dist(scale(df), scale(imputed_1))

res <- sapply(1:100, function(i) {
  imputed <- impute_mice_cart(missdf)
  miceDRF::energy_dist(scale(df), scale(imputed))
})

set.seed(222442)

missdf_1 <- as.data.frame(as.matrix(missdf) + matrix(rnorm(nrow(missdf) * ncol(missdf), 0, 1e-5), nrow = nrow(missdf), ncol = ncol(missdf)))

res2 <- sapply(1:100, function(i) {
  
  imputed_1 <- impute_mice_cart(missdf_1)
  miceDRF::energy_dist(scale(df), scale(imputed_1))
})



############################

imputation_summary %>% 
  filter(mechanism == "mar", measure == "energy_std", set_id == "slump", ratio == 0.1) %>% 
  group_by(method) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(mice_cart = method == "mice_cart") %>% 
  ggplot() +
  geom_col(aes(x = reorder(method, score), y = score, fill = mice_cart), alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "gray30")) +
  theme(legend.position = "none") +
  geom_hline(yintercept = mean(res), linewidth = 1, col = "gray30") +
  geom_hline(yintercept = quantile(res, 0.75), linetype = "dashed", linewidth = 1, col = "gray30") +
  geom_hline(yintercept = quantile(res, 0.25), linetype = "dashed", linewidth = 1, col = "gray30") + 
  geom_hline(yintercept = min(res), linetype = "longdash", linewidth = 1, col = "gray30") +
  geom_hline(yintercept = max(res), linetype = "longdash", linewidth = 1, col = "gray30") +
  geom_hline(yintercept = mean(res2), col = "black", linewidth = 1) +
  geom_hline(yintercept = quantile(res2, 0.75), linetype = "dashed", col = "black", linewidth = 1) +
  geom_hline(yintercept = quantile(res2, 0.25), linetype = "dashed", col = "black", linewidth = 1) + 
  geom_hline(yintercept = min(res2), linetype = "longdash", col = "black", linewidth = 1) +
  geom_hline(yintercept = max(res2), linetype = "longdash", col = "black", linewidth = 1)



################################

p1 <- df %>% 
  mutate(missing_x3 = is.na(missdf[, 3])) %>% 
  ggplot() +
  geom_point(aes(X3, X5, col = missing_x3), size = 2.5) +
  scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "tomato")) +
  theme_minimal() +
  ggtitle("Observed")

p2 <- imputed %>% 
  mutate(missing_x3 = is.na(missdf[, 3])) %>% 
  ggplot() +
  geom_point(aes(X3, X5, col = missing_x3), size = 2.5) +
  scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "tomato")) +
  theme_minimal() +
  ggtitle("Imputed")


p1 + p2 + plot_layout(guides = "collect")

which.max(abs(cor(df) - cor(imputed)))

df1 <- df %>%  mutate(missing_x3 = is.na(missdf[, 3]))

p1 <- GGally::ggpairs(df, diag = list(continuous = wrap("barDiag", bins = 30)))

df1 <- imputed %>%  mutate(missing_x3 = is.na(missdf[, 3]))

p2 <- GGally::ggpairs(df1, diag = list(continuous = wrap("barDiag", bins = 30)),
                      aes(color = missing_x3)) +
  scale_color_manual(values = c("FALSE" = "gray30", "TRUE"= "dodgerblue1")) +
  scale_fill_manual(values = c("FALSE" = "gray30", "TRUE"= "dodgerblue1"))

plot(df)

diag(cov(df))


colMeans(df == 0)

length(unique(df$X2))
length(df$X2)

var(df$X2[df$X2 != 0])


imputation_summary %>% 
  filter(mechanism == "mar", measure == "energy_std", set_id == "slump", ratio == 0.1) %>% 
  group_by(method) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% data.frame()
mutate(mice_cart = method == "mice_cart") %>% 
  ggplot() +
  geom_col(aes(x = reorder(method, score), y = score, fill = mice_cart)) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "gray30")) +
  theme(legend.position = "none") +
  geom_hline(yintercept = mean(res)) +
  geom_hline(yintercept = quantile(res, 0.75), linetype = "dashed") +
  geom_hline(yintercept = quantile(res, 0.25), linetype = "dashed") + 
  geom_hline(yintercept = min(res), linetype = "dashed") +
  geom_hline(yintercept = max(res), linetype = "dashed")

#######################


library(tidyverse)
library(mice)

data <- missdf

summary_stats <- summary(data)
cor_matrix <- cor(data, use = "pairwise.complete.obs")

hist_plots <- data %>% 
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

table(data$X9)

p1 <- df %>% 
  mutate(missing_x9 = is.na(missdf[, 9])) %>% 
  ggplot() +
  geom_histogram(aes(X3, fill = missing_x9)) +
  scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "tomato")) +
  theme_minimal() +
  ggtitle("Observed")

p2 <- imputed %>% 
  mutate(missing_x9 = is.na(missdf[, 9])) %>% 
  ggplot() +
  geom_histogram(aes(X3, fill = missing_x9)) +
  scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "tomato")) +
  theme_minimal() +
  ggtitle("Imputed")


sapply(df, function(ith_col) {
  length(unique(ith_col))
})


################## check hyperimpute

missdf <- make_integer_double(missdf)
a <- hyperimpute_imp(missdf, method = "hyperimpute", seed = 1)

##########################


allergens <- readRDS("./data/datasets/complete/allergens.RDS")


colMeans(allergens == 0)

allergens1 <- readRDS("C:/gits/benchmark/results/amputed/mar.0.1.1.allergens.RDS")

colSums(allergens1 == 0, na.rm = TRUE) / nrow(allergens)


hist(colMeans(allergens == 0))

hist(colSums(allergens1 == 0, na.rm = TRUE) / nrow(allergens))


plt_dat <- rbind(cbind(means = colMeans(allergens1, na.rm = TRUE), column = 1:ncol(allergens), amputed = TRUE),
                 cbind(means = colMeans(allergens, na.rm = TRUE), column = 1:ncol(allergens), amputed = FALSE)) 

plt_dat <- cbind(means = colMeans(allergens1, na.rm = TRUE), column = 1:ncol(allergens), means_1 = colMeans(allergens, na.rm = TRUE))

plt_dat %>% 
  ggplot() +
  geom_point(aes(x = column, y = means)) +
  geom_point(aes(x = column, y = means_1)) +
  geom_segment(mapping = aes(x = column, xend = column, y = means, yend = means_1), arrow = arrow(length = unit(0.3, "cm")))


mean(allergens[is.na(allergens1)] == 0)
mean(allergens == 0)





