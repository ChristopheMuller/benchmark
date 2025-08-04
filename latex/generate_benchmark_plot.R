
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(tidyr)

url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"

dat <- read_sheet(url, sheet = "Cleaned Benchmarks") %>% 
  select(-`Miao et al. (2022)`) %>% # not objective !
  filter(methods != "complete_case")


dict_of_unproper <- list(
  "KNN-custom" = c("knn_own", "knn_unspecified"),
  "RF-custom" = c("rf_own", "rf_unspecified"),
  "NN-custom" = c("NN_own"),
  "VAE-custom" = c("VAE_own", "V_AE_unspecified"),
  "Regression-custom" = c("Regression_own", "regression_unspecified"),
  "Linear-Interpolation-custom" = c("linear_interpolation_own", "linear_interpolation_unspecified"),
  "SVM-custom" = c("SVM_own", "SVM_unspecified"),
  "BayesianReg-custom" = c("BayesianRegression_own", "BayesianRegression_unspecified"),
  "BPCA-custom" = c("BPCA_own", "bpca_unspecified"),
  "Tree-custom" = c("CART_own", "Tree_unspecified", "Tree-custom"),
  "CubicSplineIP-custom" = c("cubic_spline_interpolation_own", "CubicSplineIP_unspecified"),
  "Daeu-custom" = c("daeu_own", "daeu_unspecified"),
  "Ensemble-custom" = c("Ensemble_own", "ensemble_unspecified"),
  "GaussianEM-custom" = c("Gaussian_EM", "GaussianEM_unspecified"),
  "wKNN-custom" = c("knn_weighted_own", "wKNN_unspecified"),
  "MCMC-custom" = c("MCMC_own", "MCMC"),
  "MICE-custom" = c("MICE_own", "mice_unspecified"),
  "Mode-custom" = c("mode_own", "mode"),
  "PCHIP-custom" = c("pchip_interpolation_own"),
  "SOM-custom" = c("SOM_own", "SOM_unspecified"),
  "XGB-custom" = c("xgboost_imputation_own"),
  "MF-custom" = c("MF_own", "MF_unspecified")
  
)

map_method <- function(method, dict) {
  for (name in names(dict)) {
    if (method %in% dict[[name]]) {
      return(name)
    }
  }
  return(method)
}

dat$methods <- sapply(dat$methods, map_method, dict = dict_of_unproper)


dat <- dat %>% filter(count > 0) %>% 
  arrange(-count) %>% 
  select(-implementation, -count)

dat_long <- dat %>%
  pivot_longer(cols = -methods, names_to = "benchmark", values_to = "present")

dat_long <- dat_long %>%
  group_by(methods) %>% 
  mutate(sum_methods = sum(present)) %>%
  ungroup() %>%
  group_by(benchmark) %>% 
  mutate(sum_benchmarks = sum(present)) %>%
  ungroup()

dat_long <- dat_long %>%
  mutate(fill = as.character(present))

proper_df <- dat %>% select(methods, proper)
dat_long <- dat_long %>%
  left_join(proper_df, by = "methods")


ordering <- dat_long %>%
  distinct(methods, proper, sum_methods) %>%
  arrange(desc(proper), desc(sum_methods)) %>%
  mutate(y_order = row_number())

separator_y <- ordering %>%
  filter(proper == 1) %>%
  summarise(pos = max(y_order) + 0.5) %>%  # +0.5 puts the line *between* rows
  pull(pos)

dat_long <- dat_long %>%
  filter(benchmark != "proper")

ggplot(dat_long) +
  geom_tile(aes(
    x = reorder(benchmark, -sum_benchmarks),
    y = reorder(methods, -proper * 1000 - sum_methods),
    fill = fill
  ), color = "black") +
  geom_hline(yintercept = separator_y, color = "black", size = 1.2) +  # <-- thick separator
  scale_fill_manual(
    name = "Included",
    values = c("0" = "white", "1" = "lightsteelblue3"),
    labels = c("0" = "No", "1" = "Yes")
  ) +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  ) +
  coord_flip() +
  ggsave("~/INRIA/R_scripts/benchmark/latex/benchmarks.pdf", width = 15, height = 6, units = "in")

