
options(warn = -1)  # Turn off warnings
# options(warn = 0)  # Re-enable warnings



library(targets)
library(tarchetypes)
library(purrr)


# Source custom functions
tar_source("R/functions.R")


# Tar options
tar_option_set(
  packages = c("imputomics", "energy"),
)


methods <- list(method = c("mean", "median", "zero", "min", "halfmin", "random", 
                           "mice_cart", "mice_pmm", "mice_mixed", "mice_rf", "missforest", 
                           "metabimpute_rf", "missmda_em", "amelia", "areg", "tknn", 
                           "corknn", "knn", "bpca", "metabimpute_bpca", "cm", "softimpute", 
                           "bayesmetab", "mice_drf", "mice_norm_predict", "mice_norm_nob"))



# Define pipeline

create_data <- list(
  # Data generation parameters
  tar_target(
    data_params,
    list(
      n = 100,              # number of samples
      d = 10,               # number of variables
      prc_missing = 0.15,   # percentage of missing values
      seed = 123            # random seeds
    )
  ),
  
  # Generate synthetic data
  tar_target(
    synthetic_data,
    generate_fake_data(
      n = data_params$n,
      d = data_params$d,
      prc_missing = data_params$prc_missing,
      seed = data_params$seed
    )
  ),
  
  # Compute and display statistics
  tar_target(
    data_stats,
    compute_statistics(synthetic_data)
  )
)


mapped <- tar_map(
    unlist = FALSE,
    
    values = methods,
    tar_target(
      imputed,
      {
        imputed <- impute_all(synthetic_data$missing, method)
        imputed
      }
    ),
    tar_target(
      imputed_scores,
      {
        imputed_scores = list(
          rmse = compute_imputation_rmse(synthetic_data$complete, imputed$Imp),
          energy = compute_imputation_energy(synthetic_data$complete, imputed$Imp)
        )
        imputed_scores
      }
      
    )
)

combine_scores <- tar_combine(
  combined_scores,
  mapped[["imputed_scores"]],
  
  command = dplyr::bind_rows(!!!.x, .id = "method")
)


list(
  create_data,
  mapped,
  combine_scores
)



































### TRACKING ###

# tar_target(
#   imputed_mean,
#   impute_mean(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_median,
#   impute_median(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_min,
#   impute_min(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_halfmin,
#   impute_halfmin(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_random,
#   impute_random(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_zero,
#   impute_zero(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_mice_cart,
#   impute_mice_cart(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_mice_pmm,
#   impute_mice_pmm(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_mice_mixed,
#   impute_mice_mixed(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_mice_rf,
#   impute_mice_rf(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_missforest,
#   impute_missforest(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_metabimpute_rf,
#   impute_metabimpute_rf(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_missmda_em,
#   impute_missmda_em(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_amelia,
#   impute_amelia(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_areg,
#   impute_areg(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_tknn,
#   impute_tknn(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_corknn,
#   impute_corknn(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_knn,
#   impute_knn(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_bpca,
#   impute_bpca(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_metabimpute_bpca,
#   impute_metabimpute_bpca(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_cm,
#   impute_cm(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_softimpute,
#   impute_softimpute(synthetic_data$missing)
# ),
# 
# tar_target(
#   imputed_bayesmetab,
#   impute_bayesmetab(synthetic_data$missing)
# ),



# Compute score individually


# tar_target(
#   imputed_mean_scores,
#   {
#     imputed_mean_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_mean$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_mean$Imp)
#     )
#     imputed_mean_scores
#   }
# ),  
# 
# tar_target(
#   imputed_median_scores,
#   {
#     imputed_median_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_median$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_median$Imp)
#     )
#     imputed_median_scores
#   }
# ),
# 
# tar_target(
#   imputed_min_scores,
#   {
#     imputed_min_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_min$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_min$Imp)
#     )
#     imputed_min_scores
#   }
# ),
# 
# tar_target(
#   imputed_halfmin_scores,
#   {
#     imputed_halfmin_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_halfmin$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_halfmin$Imp)
#     )
#     imputed_halfmin_scores
#   }
# ),
# 
# tar_target(
#   imputed_random_scores,
#   {
#     imputed_random_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_random$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_random$Imp)
#     )
#     imputed_random_scores
#   }
# ),
# 
# tar_target(
#   imputed_zero_scores,
#   {
#     imputed_zero_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_zero$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_zero$Imp)
#     )
#     imputed_zero_scores
#   }
# ),
# 
# tar_target(
#   imputed_mice_cart_scores,
#   {
#     imputed_mice_cart_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_mice_cart$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_mice_cart$Imp)
#     )
#     imputed_mice_cart_scores
#   }
# ),
# 
# tar_target(
#   imputed_mice_pmm_scores,
#   {
#     imputed_mice_pmm_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_mice_pmm$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_mice_pmm$Imp)
#     )
#     imputed_mice_pmm_scores
#   }
# ),
# 
# tar_target(
#   imputed_mice_mixed_scores,
#   {
#     imputed_mice_mixed_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_mice_mixed$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_mice_mixed$Imp)
#     )
#     imputed_mice_mixed_scores
#   }
# ),
# 
# tar_target(
#   imputed_mice_rf_scores,
#   {
#     imputed_mice_rf_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_mice_rf$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_mice_rf$Imp)
#     )
#     imputed_mice_rf_scores
#   }
# ),
# 
# tar_target(
#   imputed_missforest_scores,
#   {
#     imputed_missforest_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_missforest$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_missforest$Imp)
#     )
#     imputed_missforest_scores
#   }
# ),
# 
# tar_target(
#   imputed_metabimpute_rf_scores,
#   {
#     imputed_metabimpute_rf_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_metabimpute_rf$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_metabimpute_rf$Imp)
#     )
#     imputed_metabimpute_rf_scores
#   }
# ),
# 
# tar_target(
#   imputed_missmda_em_scores,
#   {
#     imputed_missmda_em_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_missmda_em$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_missmda_em$Imp)
#     )
#     imputed_missmda_em_scores
#   }
# ),
# 
# tar_target(
#   imputed_amelia_scores,
#   {
#     imputed_amelia_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_amelia$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_amelia$Imp)
#     )
#     imputed_amelia_scores
#   }
# ),
# 
# tar_target(
#   imputed_areg_scores,
#   {
#     imputed_areg_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_areg$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_areg$Imp)
#     )
#     imputed_areg_scores
#   }
# ),
# 
# tar_target(
#   imputed_tknn_scores,
#   {
#     imputed_tknn_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_tknn$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_tknn$Imp)
#     )
#     imputed_tknn_scores
#   }
# ),
# 
# tar_target(
#   imputed_corknn_scores,
#   {
#     imputed_corknn_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_corknn$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_corknn$Imp)
#     )
#     imputed_corknn_scores
#   }
# ),
# 
# tar_target(
#   imputed_knn_scores,
#   {
#     imputed_knn_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_knn$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_knn$Imp)
#     )
#     imputed_knn_scores
#   }
# ),
# 
# tar_target(
#   imputed_bpca_scores,
#   {
#     imputed_bpca_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_bpca$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_bpca$Imp)
#     )
#     imputed_bpca_scores
#   }
# ),
# 
# tar_target(
#   imputed_metabimpute_bpca_scores,
#   {
#     imputed_metabimpute_bpca_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_metabimpute_bpca$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_metabimpute_bpca$Imp)
#     )
#     imputed_metabimpute_bpca_scores
#   }
# ),
# 
# tar_target(
#   imputed_cm_scores,
#   {
#     imputed_cm_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_cm$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_cm$Imp)
#     )
#     imputed_cm_scores
#   }
# ),
# 
# tar_target(
#   imputed_softimpute_scores,
#   {
#     imputed_softimpute_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_softimpute$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_softimpute$Imp)
#     )
#     imputed_softimpute_scores
#   }
# ),
# 
# tar_target(
#   imputed_bayesmetab_scores,
#   {
#     imputed_bayesmetab_scores = list(
#       rmse = compute_imputation_rmse(synthetic_data$complete, imputed_bayesmetab$Imp),
#       energy = compute_imputation_energy(synthetic_data$complete, imputed_bayesmetab$Imp)
#     )
#     imputed_bayesmetab_scores
#   }
# ),




# Combine all imputations into a list
# tar_target(
#   imputation_list,
#   list(
#     mean = imputed_mean,
#     median = imputed_median,
#     min = imputed_min,
#     halfmin = imputed_halfmin,
#     random = imputed_random,
#     zero = imputed_zero,
#     mice_cart = imputed_mice_cart,
#     mice_pmm = imputed_mice_pmm,
#     mice_mixed = imputed_mice_mixed,
#     mice_rf = imputed_mice_rf,
#     missforest = imputed_missforest,
#     metabimpute_rf = imputed_metabimpute_rf,
#     missmda_em = imputed_missmda_em,
#     amelia = imputed_amelia,
#     areg = imputed_areg,
#     tknn = imputed_tknn,
#     corknn = imputed_corknn,
#     knn = imputed_knn,
#     bpca = imputed_bpca,
#     metabimpute_bpca = imputed_metabimpute_bpca,
#     cm = imputed_cm,
#     softimpute = imputed_softimpute,
#     bayesmetab = imputed_bayesmetab
#   )
# ),
# 
# tar_target(
#   imputation_score_list,
#   {
#     imputation_score_list = list(
#       mean = imputed_mean_scores,
#       median = imputed_median_scores,
#       min = imputed_min_scores,
#       halfmin = imputed_halfmin_scores,
#       random = imputed_random_scores,
#       zero = imputed_zero_scores,
#       mice_cart = imputed_mice_cart_scores,
#       mice_pmm = imputed_mice_pmm_scores,
#       mice_mixed = imputed_mice_mixed_scores,
#       mice_rf = imputed_mice_rf_scores,
#       missforest = imputed_missforest_scores,
#       metabimpute_rf = imputed_metabimpute_rf_scores,
#       missmda_em = imputed_missmda_em_scores,
#       amelia = imputed_amelia_scores,
#       areg = imputed_areg_scores,
#       tknn = imputed_tknn_scores,
#       corknn = imputed_corknn_scores,
#       knn = imputed_knn_scores,
#       bpca = imputed_bpca_scores,
#       metabimpute_bpca = imputed_metabimpute_bpca_scores,
#       cm = imputed_cm_scores,
#       softimpute = imputed_softimpute_scores,
#       bayesmetab = imputed_bayesmetab_scores
#     )
#     imputation_score_list
#   }
# ),
# 
# # Target for the bar plot
# tar_target(
#   imputation_rmse_plot,
#   plot_imputation_scores(imputation_score_list, log_y = TRUE, keep_score="rmse")
# ),
# 
# tar_target(
#   imputation_energy_plot,
#   plot_imputation_scores(imputation_score_list, log_y = TRUE, keep_score="energy")
# ),
# 
# tar_target(
#   imputation_several_scores_plots,
#   plot_imputation_several_scores(imputation_score_list, log_y = TRUE)
# ),
# 
# tar_target(
#   imputation_errors_list,
#   get_methods_with_errors(imputation_list)
# )


