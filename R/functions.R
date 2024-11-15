
#####################
# DATA GENERATION
#####################



# Generate synthetic data with missing values
generate_fake_data <- function(n, d, prc_missing, seed) {
  # Set random seed for reproducibility
  set.seed(seed)
  
  # Generate complete data matrix
  X.complete <- matrix(rnorm(n*d), n, d)
  colnames(X.complete) <- paste0("V", 1:d)
  
  # Create missing values
  X.missing <- X.complete
  X.missing[sample(1:(n*d), n*d*prc_missing)] <- NA
  
  # Convert to data frame
  X.missing <- as.data.frame(X.missing)
  
  # Remove rows where all values are missing
  idx_not_all_missing <- apply(X.missing, 1, function(x) any(!is.na(x)))
  X.missing <- X.missing[idx_not_all_missing,]
  X.complete <- as.data.frame(X.complete[idx_not_all_missing,])
  
  # Return both complete and missing datasets
  list(
    complete = X.complete,
    missing = X.missing
  )
}

# Compute basic statistics of the data
compute_statistics <- function(data) {
  # Statistics for complete data
  complete_stats <- list(
    n_rows = nrow(data$complete),
    n_cols = ncol(data$complete),
    means = colMeans(data$complete),
    sds = apply(data$complete, 2, sd),
    complete_summary = summary(data$complete)
  )
  
  # Statistics for data with missing values
  missing_stats <- list(
    n_rows = nrow(data$missing),
    n_cols = ncol(data$missing),
    n_missing = sum(is.na(data$missing)),
    pct_missing = mean(is.na(data$missing)) * 100,
    missing_by_col = colSums(is.na(data$missing)),
    missing_by_col_pct = colMeans(is.na(data$missing)) * 100,
    available_means = colMeans(data$missing, na.rm = TRUE),
    available_sds = apply(data$missing, 2, sd, na.rm = TRUE),
    missing_summary = summary(data$missing)
  )
  
  # Return both sets of statistics
  list(
    complete = complete_stats,
    missing = missing_stats
  )
}



#####################
# IMPUTATION
#####################


impute_wrapper <- function(DATA.MISSING, package_name, function_name, err.tolerance=5, ...) {
  # Define empty list to store results
  imputation_result <- list(Imp = NULL, Err.count = 0, Same.data = FALSE, Runtime = NA)
  
  # Identify observed cells in the original data
  observed_indices <- !is.na(DATA.MISSING)
  
  # Loop for catching errors
  for (i in 1:err.tolerance) {
    tryCatch({
      # Start measuring the running time
      start_time <- Sys.time()
      
      # Dynamically call the imputation function using get to locate the function within the package
      imputation_function <- get(function_name, envir = asNamespace(package_name))
      imputation_result$Imp <- do.call(imputation_function, args = list(...))
      
      # Stop measuring the running time
      end_time <- Sys.time()
      imputation_result$Runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      # Check if observed data remains the same
      imputation_result$Same.data <- all(DATA.MISSING[observed_indices] == imputation_result$Imp[observed_indices])
      
      # Break the loop if successful
      break
      
    }, error = function(e) {
      # Increment error count on catch
      imputation_result$Err.count <<- imputation_result$Err.count + 1
    })
  }
  
  # Return the imputation result with runtime
  return(imputation_result)
}


impute_all <- function(data, method) {
  
  methods_imputomics <- c("mean", "median", "zero", "min", "halfmin", "random", 
                          "mice_cart", "mice_pmm", "mice_mixed", "mice_rf", "missforest", 
                          "metabimpute_rf", "missmda_em", "amelia", "areg", "tknn", 
                          "corknn", "knn", "bpca", "metabimpute_bpca", "cm", "softimpute", "bayesmetab")
  
  if (method %in% methods_imputomics){
    imputed_data <- impute_wrapper(DATA.MISSING=data, package_name="imputomics", function_name=paste0("impute_", method), missdf=data)
  }
  
  else if (method %in% c("mice_drf", "mice_norm_predict", "mice_norm_nob")){
    source("miceDRF.R")
    method_name <- ifelse(method == "mice_drf", "DRF", ifelse(method == "mice_norm_predict", "norm.predict", "norm.nob"))
    temp <- impute_wrapper(DATA.MISSING=data, package_name="mice", function_name="mice", m=1, data=data, method=method_name, printflag=FALSE)
    imputed_data <- temp
    imputed_data$Imp <- mice::complete(temp$Imp)
  }
  
  
  return(imputed_data)
}


# # Mean imputation
# impute_mean <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_mean", missdf=data)
# }
# 
# # Median imputation
# impute_median <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_median", missdf=data)
# }
# 
# # Min imputation
# impute_min <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_min", missdf=data)
# }
# 
# # Halfmin imputation
# impute_halfmin <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_halfmin", missdf=data)
# }
# 
# # Random imputation
# impute_random <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_random", missdf=data)
# }
# 
# # Zero imputation
# impute_zero <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_zero", missdf=data)
# }
# 
# # Mice cart imputation
# impute_mice_cart <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_mice_cart", missdf=data)
# }
# 
# # Mice pmm imputation
# impute_mice_pmm <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_mice_pmm", missdf=data)
# }
# 
# # Mice mixed imputation
# impute_mice_mixed <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_mice_mixed", missdf=data)
# }
# 
# # Mice rf imputation
# impute_mice_rf <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_mice_rf", missdf=data)
# }
# 
# # missforest imputation
# impute_missforest <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_missforest", missdf=data)
# }
# 
# # metabimpute_rf imputation
# impute_metabimpute_rf <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_metabimpute_rf", missdf=data)
# }
# 
# # missmda_em imputation
# impute_missmda_em <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_missmda_em", missdf=data)
# }
# 
# # amelia imputation
# impute_amelia <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_amelia", missdf=data)
# }
# 
# # areg imputation
# impute_areg <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_areg", missdf=data)
# }
# 
# # tknn imputation
# impute_tknn <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_tknn", missdf=data)
# }
# 
# # corknn imputation
# impute_corknn <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_corknn", missdf=data)
# }
# 
# # knn imputation
# impute_knn <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_knn", missdf=data)
# }
# 
# # bpca imputation
# impute_bpca <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_bpca", missdf=data)
# }
# 
# # metabimpute bpca imputation
# impute_metabimpute_bpca <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_metabimpute_bpca", missdf=data)
# }
# 
# # cm imputation
# impute_cm <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_cm", missdf=data)
# }
# 
# # softimpute imputation
# impute_softimpute <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_softimpute", missdf=data)
# }
# 
# # bayesmetab imputation
# impute_bayesmetab <- function(data) {
#   imputed_data <- impute_wrapper(data=data, package_name="imputomics", function_name="impute_bayesmetab", missdf=data)
# }



#####################
# EVALUATION
#####################


compute_imputation_rmse <- function(complete_data, imputated_data) {
  
  complete_data <- as.matrix(complete_data)
  imputated_data <- as.matrix(imputated_data)
  
  sqrt(mean((complete_data - imputated_data)^2))
  
}

compute_imputation_energy <- function(complete_data, imputated_data) {
  
  as.vector(eqdist.e(rbind(complete_data, imputated_data), c(nrow(complete_data), nrow(imputated_data))))
  
}


# Function to create a bar plot of RMSE scores
plot_imputation_scores <- function(score_list, log_y = FALSE, keep_score="rmse") {
  library(ggplot2)
  
  # Convert list to data frame for easy plotting
  scores_df <- do.call(rbind, lapply(score_list, function(x) data.frame(keep_score = x[[keep_score]])))
  scores_df$method <- rownames(scores_df)
  
  # Plot with ggplot2
  p <- ggplot(scores_df, aes(x = method, y = keep_score)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    theme_minimal() +
    labs(title = paste("Imputation ", keep_score, " Scores by Method"), x = "Method", y = keep_score) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Apply log scale if requested
  if (log_y) {
    p <- p + scale_y_log10()
  }
  
  p
}

# Function to create a bar plot showing multiple imputation scores in two rows
plot_imputation_several_scores <- function(score_list, log_y = FALSE) {
  library(ggplot2)
  library(tidyr)
  
  # Convert the score_list to a data frame
  scores_df <- do.call(rbind, lapply(score_list, function(x) {
    # Extract the selected metrics (ensure they are atomic)
    data.frame(rmse = x$rmse, energy = x$energy)
  }))
  
  # Add method names as a column
  scores_df$method <- rownames(scores_df)
  
  # Convert to long format using tidyr::pivot_longer() for easier plotting
  scores_df_long <- pivot_longer(scores_df, cols = c("rmse", "energy"), 
                                 names_to = "metric", values_to = "score")
  
  # Plot with ggplot2
  p <- ggplot(scores_df_long, aes(x = method, y = score, fill = method)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ metric, scales = "free_y", nrow = 2) +  # Create two rows of plots
    theme_minimal() +
    labs(title = "Imputation Scores by Method", x = "Method", y = "Score") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(strip.text = element_text(size = 12),  # Customize facet labels
          legend.position = "none")  # Hide legend
  
  # Apply log scale if requested
  if (log_y) {
    p <- p + scale_y_log10()
  }
  
  p
}

get_methods_with_errors <- function(imputation_list) {
  # Filter methods with Err.count > 0 and sort by Err.count in descending order
  error_methods <- sapply(imputation_list, function(imputation_result) {
    return(imputation_result$Err.count)
  })
  
  # Filter out methods with Err.count == 0
  error_methods <- error_methods[error_methods > 0]
  
  # Sort methods by Err.count from most to least errors
  sorted_error_methods <- sort(error_methods, decreasing = TRUE)

  # Create a data frame with method names and error counts
  error_methods_df <- data.frame(
    method = names(sorted_error_methods),
    err_count = sorted_error_methods,
    stringsAsFactors = FALSE
  )
  
  return(error_methods_df)
}








