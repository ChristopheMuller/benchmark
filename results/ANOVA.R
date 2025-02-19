# Load required libraries
library(lme4)  # For mixed models
library(car)   # For ANOVA
library(dplyr)
library(emmeans)
library(ggplot2)


# Read and prepare data
imputation_summary <- readRDS("~/INRIA/R_scripts/benchmark/results/imputation_summary_31_to_38.RDS")

score <- "energy_std"

imputation_summary <- imputation_summary[imputation_summary$measure == score,]

imputation_summary <- imputation_summary %>%
  group_by(set_id, ratio, mechanism) %>%
  mutate(rank = rank(score, ties.method = "average")) %>%
  ungroup()
imputation_summary$score <- imputation_summary$rank
# drop rows with Nas
imputation_summary <- imputation_summary[c("set_id", "mechanism", "ratio", "rep", "method", "score")]
imputation_summary <- imputation_summary[complete.cases(imputation_summary),]
imputation_summary$score <- log(imputation_summary$score  + 1)



# Convert to factors
imputation_summary <- imputation_summary %>%
  mutate(across(c(set_id, mechanism, ratio, rep, method), factor))

data <- imputation_summary
hist(data$score)

# Check the structure of your data
str(data)
summary(data)


model <- lmer(score ~ method * mechanism * ratio + (1 | set_id), data = data)

# Summary of the simplified model
summary(model)

# Perform ANOVA
anova(model)
anova_model <- Anova(model, type = "III")  # Type III for interaction effects
print(anova_model)


summary(model)$varcor
method_means <- emmeans(model, ~ method)

# Display results
summary(method_means)

ggplot(as.data.frame(method_means), aes(x = reorder(method, emmean), y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(
    x = "Method",
    y = "Estimated Mean Score",
    title = "Estimated Mean Scores with 95% Confidence Intervals (random effect)"
  ) +
  theme_minimal() +
  coord_flip()



### fixed effect


# Fixed-effect model with set_id as a fixed factor
model_fixed <- lm(score ~ method * mechanism *ratio + set_id, data = data)

# Summary of the model
summary(model_fixed)

# Perform ANOVA
anova(model_fixed)

# For estimated marginal means of method
library(emmeans)
method_means_fixed <- emmeans(model_fixed, ~ method)

# Display results
summary(method_means_fixed)

# Plot the results with confidence intervals
ggplot(as.data.frame(method_means_fixed), aes(x = reorder(method, emmean), y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(
    x = "Method",
    y = "Estimated Mean Score",
    title = "Estimated Mean Scores with 95% Confidence Intervals (Fixed Effect)"
  ) +
  theme_minimal() +
  coord_flip()








