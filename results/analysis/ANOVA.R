library(dplyr)
library(ggplot2)


# Load and prepare the imputation summary dataset
imputation_summary <- readRDS("./results/imputation_summary_M2.RDS")
table_scores <- imputation_summary %>%
  filter(measure == "energy_std") %>%
  select(c(set_id, mechanism, ratio, rep, method, imputation_fun, score))

# Define a placeholder for maximum rank value
K_NA <- 65

# Process the data: Compute ranks, handle failed methods, and assign max rank where necessary
table_scores <- table_scores %>%
  group_by(set_id, mechanism, ratio, rep) %>%
  mutate(
    rank = min_rank(score),
    failed = ifelse(is.na(rank), 1, 0),
    rank = ifelse(is.na(rank), K_NA, rank)
  ) %>%
  ungroup() %>%
  select(-score)

# take average of both rep
table_scores <- table_scores %>%
  group_by(set_id, mechanism, ratio, imputation_fun) %>%
  summarize(rank = mean(rank), failed = mean(failed), .groups = "drop")

# Summary statistics by imputation method
summary_by_method <- table_scores %>%
  group_by(imputation_fun) %>%
  summarize(
    mean_rank = mean(rank),
    median_rank = median(rank),
    sd_rank = sd(rank),
    n = n()
  ) %>%
  arrange(mean_rank)

# Print overall performance of methods
print(summary_by_method)

# Calculate the confidence interval (C.I.) for each method's mean rank
summary_by_method <- summary_by_method %>%
  mutate(
    ci_lower = mean_rank - qt(0.975, df = n - 1) * sd_rank / sqrt(n),
    ci_upper = mean_rank + qt(0.975, df = n - 1) * sd_rank / sqrt(n)
  )

# Create the plot
ggplot(summary_by_method, aes(x = reorder(imputation_fun, mean_rank), y = mean_rank)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "black") +
  labs(title = "Imputation Method Performance with Confidence Intervals",
       x = "Imputation Method",
       y = "Mean Rank (Lower is Better)") +
  coord_flip() +  # Flip for better readability
  theme_minimal()




##
# 3. ANOVA
##
# Convert categorical variables to factors
table_scores$set_id <- as.factor(table_scores$set_id)
table_scores$mechanism <- as.factor(table_scores$mechanism)
table_scores$imputation_fun <- as.factor(table_scores$imputation_fun)

# Fit ANOVA model (excluding failed methods)
anova_model <- aov(rank ~ set_id + mechanism + ratio + imputation_fun, data = table_scores %>% filter(failed == 0))

# Summary and diagnostics
summary(anova_model)
plot(anova_model, which = 1)  # Residuals vs Fitted
qqnorm(residuals(anova_model))
qqline(residuals(anova_model))

# Effect size using Eta-squared
library(DescTools)
eta_sq <- EtaSq(anova_model, type = 2)  # Type II SS
print(eta_sq)

# Effect size using effectsize package
library(effectsize)
anova_effect_size <- eta_squared(anova_model)
print(anova_effect_size)


##
# 4. Log.Reg for FAILED
##
# Logistic regression model to predict method failure
glm_failed <- glm(failed ~ set_id + mechanism * imputation_fun + ratio, 
                  data = table_scores, family = binomial)

# Summary of the logistic model
summary(glm_failed)

# Multicollinearity check using VIF
library(car)
vif(glm_failed)

# ROC curve for failure prediction
library(pROC)
pred_probs <- predict(glm_failed, type = "response")
roc_obj <- roc(table_scores$failed, pred_probs)
plot(roc_obj, col = "blue", lwd = 2)
auc(roc_obj)

# Summary of failure rates by method
failure_summary <- table_scores %>%
  group_by(imputation_fun) %>%
  summarize(failure_rate = mean(failed)) %>%
  arrange(desc(failure_rate))

print(failure_summary)

# Residuals vs fitted values plot for logistic model
plot(glm_failed$fitted.values, residuals(glm_failed), main = "Residuals vs Fitted (Logistic)")


## 
# Plot interactions plots
##
# Calculate method variability across mechanisms
method_variability <- table_scores %>%
  group_by(imputation_fun, mechanism) %>%
  summarize(mean_rank = mean(rank), .groups = "drop") %>%
  group_by(imputation_fun) %>%
  summarize(rank_variability = max(mean_rank) - min(mean_rank), .groups = "drop") %>%
  arrange(desc(rank_variability))

# Select top 10 methods with the highest variability
top_k <- 10
top_movers <- method_variability %>%
  head(top_k) %>%
  pull(imputation_fun)

manual_methods <- c("impute_mice_drf", "impute_mice_cart")  # Add the method names as characters

# Ensure that all elements in top_ratio_movers are character type (if they aren't already)
top_movers <- as.character(top_movers)

# Append the manual methods to the top_ratio_movers
top_movers <- c(top_movers, manual_methods)


# Create plot for variability across mechanisms
table_scores %>%
  group_by(imputation_fun, mechanism) %>%
  summarize(mean_rank = mean(rank), .groups = "drop") %>%
  mutate(is_top_mover = imputation_fun %in% top_movers) %>%
  ggplot(aes(x = mechanism, y = mean_rank, group = imputation_fun, 
             color = if_else(is_top_mover, imputation_fun, "Other Methods"),
             alpha = if_else(is_top_mover, 1, 0.3),
             size = if_else(is_top_mover, 1, 0.5),
             linetype = if_else(is_top_mover, "solid", "dashed"))) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Other Methods" = "grey50", 
                                setNames(scales::hue_pal()(length(top_movers)), top_movers))) +
  scale_alpha_identity() +
  scale_size_identity() +
  scale_linetype_identity() +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(title = "Imputation Method", 
                              override.aes = list(alpha = 1, size = 1))) +
  labs(title = "Imputation Method Performance by Missing Data Mechanism",
       subtitle = "Highlighting the 10 methods with most variable performance across mechanisms",
       y = "Mean Rank (lower is better)", x = "Mechanism")


# Calculate variability of methods across ratios
method_ratio_variability <- table_scores %>%
  group_by(imputation_fun, ratio) %>%
  summarize(mean_rank = mean(rank), .groups = "drop") %>%
  group_by(imputation_fun) %>%
  summarize(rank_variability = max(mean_rank) - min(mean_rank), .groups = "drop") %>%
  arrange(desc(rank_variability))

# Select top 10 methods with the highest variability
top_ratio_movers <- method_ratio_variability %>%
  head(top_k) %>%
  pull(imputation_fun)

manual_methods <- c("impute_mice_drf", "impute_mice_cart")  # Add the method names as characters

# Ensure that all elements in top_ratio_movers are character type (if they aren't already)
top_ratio_movers <- as.character(top_ratio_movers)

# Append the manual methods to the top_ratio_movers
top_ratio_movers <- c(top_ratio_movers, manual_methods)



# Create plot for variability across ratios
table_scores %>%
  group_by(imputation_fun, ratio) %>%
  summarize(mean_rank = mean(rank), .groups = "drop") %>%
  mutate(is_top_mover = imputation_fun %in% top_ratio_movers) %>%
  ggplot(aes(x = ratio, y = mean_rank, group = imputation_fun, 
             color = if_else(is_top_mover, imputation_fun, "Other Methods"),
             alpha = if_else(is_top_mover, 1, 0.3),
             size = if_else(is_top_mover, 1, 0.5),
             linetype = if_else(is_top_mover, "solid", "dashed"))) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Other Methods" = "grey50", 
                                setNames(scales::hue_pal()(length(top_ratio_movers)), top_ratio_movers))) +
  scale_alpha_identity() +
  scale_size_identity() +
  scale_linetype_identity() +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(title = "Imputation Method", 
                              override.aes = list(alpha = 1, size = 1))) +
  labs(title = "Imputation Method Performance by Missing Data Ratio",
       subtitle = "Highlighting the 10 methods with most variable performance across ratios",
       y = "Mean Rank (lower is better)", x = "Missing Data Ratio")


## 
# Linear Mixed Effects Model
##
library(lme4)

# Fit the mixed-effects model
mixed_model <- lmer(rank ~ mechanism + ratio + imputation_fun + 
                      mechanism:imputation_fun + ratio:imputation_fun + 
                      (1|set_id), data = table_scores)

# Model summary
summary(mixed_model)

# Plot residuals vs fitted values and check normality
plot(mixed_model, which = 1)  # Residuals vs Fitted
qqnorm(residuals(mixed_model))
qqline(residuals(mixed_model))

# Calculate R-squared for the model
library(performance)
r2(mixed_model)

# Compare models with and without interactions
model_with_failed <- lmer(rank ~ mechanism + ratio + imputation_fun + failed +
                            mechanism:imputation_fun + ratio:imputation_fun + 
                            (1|set_id), data = table_scores)
anova(model_with_failed, mixed_model)

##
# estimated marginal means
##
library(emmeans)
library(ggplot2)

# Compute EMMs for imputation methods
emm_results <- emmeans(mixed_model, ~ imputation_fun, type = "response")

# Convert to dataframe for plotting
emm_df <- as.data.frame(emm_results)

# Sort methods from best (lowest rank) to worst (highest rank)
emm_df <- emm_df[order(emm_df$emmean), ]

# Plot the EMMs with confidence intervals
ggplot(emm_df, aes(x = reorder(imputation_fun, emmean), y = emmean)) +
  geom_point(size = 3, color = "blue") +  # Plot points for each method's estimated mean
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, color = "black") +  # Add confidence intervals
  labs(title = "Estimated Ranks of Imputation Methods with 95% Confidence Intervals",
       x = "Imputation Method",
       y = "Estimated Rank (Lower is Better)") +
  coord_flip() +  # Flip for better readability
  theme_minimal()  # Apply a clean theme
