
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


methods <- list(method = c(
          "mean", "median", "zero", "min", "halfmin", "random",
          "mice_cart", "mice_pmm", "mice_mixed", "mice_rf", "missforest",
          "metabimpute_rf", "missmda_em", "amelia", "areg", "tknn",
          "corknn", "knn", "bpca", "metabimpute_bpca", "cm", "softimpute",
          "bayesmetab", "mice_drf", "mice_norm_predict", "mice_norm_nob",
          "FEFI", "pca_lls", "pca_classic", "pca_robust",
          "rmiMAE", "gamlss", "FHDI"))



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
          energy = compute_imputation_energy(synthetic_data$complete, imputed$Imp),
          runtime = imputed$Runtime,
          count_err = imputed$Err.count,
          name = method
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

plotting_scores <- tar_target(
  plot_scores,
  {
    # Create individual plots
    plot_rmse <- plot_imputation_scores(combined_scores, log_y = T, score_name = "rmse")
    plot_energy <- plot_imputation_scores(combined_scores, log_y = T, score_name = "energy")
    plot_runtime <- plot_imputation_scores(combined_scores, log_y = T, score_name = "runtime")
    
    # Combine plots using patchwork
    library(patchwork)
    combined_plot <- (plot_rmse / plot_energy / plot_runtime) +
      plot_layout(heights = c(1, 1, 1)) +
      plot_annotation(
        title = "Imputation Method Comparison",
        theme = theme(plot.title = element_text(hjust = 0.5))
      )
    
    # Return all plots in a list
    list(
      rmse = plot_rmse,
      energy = plot_energy,
      runtime = plot_runtime,
      combined = combined_plot
    )
  }
)


list(
  create_data,
  mapped,
  combine_scores,
  plotting_scores
)

