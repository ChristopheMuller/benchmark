
calculate_scores <- function(imputed, amputed, imputation_fun, multiple, 
                             imputed_id, timeout_thresh, filepath_original, 
                             case, var_type = NULL) {
  
  imputed_data <- imputed[["imputed"]]
  res <- imputed[["res"]]
  
  if(!is.na(res[["error"]])) {
    return(cross_join(res, data.frame(measure = c("mae", "rmse", "nrmse", 
                                                  "rsq", "ccc", "energy", 
                                                  "energy_std", "IScore", 
                                                  "IScore_cat"),
                                      score = NA)))
  }
  
  original_data <- readRDS(filepath_original)
  
  scores <- switch(
    case,
    complete = scores_for_complete(original_data = original_data, 
                                   amputed_data = amputed, 
                                   imputed_data = imputed_data,
                                   imputation_fun = imputation_fun),
    incomplete = scores_for_incomplete(original_data = original_data, 
                                       imputed_data = imputed_data, 
                                       imputation_fun = imputation_fun,
                                       multiple = multiple,
                                       timeout_thresh = timeout_thresh),
    categorical = scores_for_categorical(original_data = original_data, 
                                         imputed_data = imputed_data),
    incomplete_categorical = scores_for_incomplete(original_data = original_data, 
                                                   imputed_data = imputed_data, 
                                                   imputation_fun = imputation_fun,
                                                   multiple = multiple,
                                                   timeout_thresh = timeout_thresh,
                                                   case = case, var_type = var_type)
  )
  
  res %>% 
    cross_join(scores)
}



summarize_imputations <- function(all_scores, params) {
  
  params %>% 
    left_join(all_scores, by = "imputed_id") %>% 
    dplyr::select(set_id, mechanism, ratio, rep, case, method, imputation_fun, 
                  time, attempts, error, measure, score)
}




scores_for_categorical <- function(original_data, imputed_data) {
  
  ids_categoricals <- which(sapply(original_data, is.factor))
  # imputed_data <- mutate(imputed_data, across(matches(names(ids_categoricals)), as.factor))
  imputed_data[ids_categoricals] <- mutate_all(imputed_data[ids_categoricals], as.factor)
  
  dim_imputed <- dim(imputed_data)
  dim_original <- dim(original_data)
  
  # first rbind
  binded_data <- rbind(original_data, imputed_data)
  # then one hot encode
  binded_data <- one_hot_encoding(binded_data)
  # then split
  original_data <- binded_data[1:dim_original[1], ]
  imputed_data <- binded_data[dim_original[1]+1:dim_imputed[1], ]
  
  # original_data <- one_hot_encoding(original_data)
  # imputed_data <- one_hot_encoding(imputed_data)
  
  energy <- as.numeric(miceDRF::energy_dist(X = original_data, 
                                            X_imp = imputed_data))
  scaled_original <- scale(original_data)
  
  scaled_imputed <- sapply(1:ncol(imputed_data), function(i) {
    (imputed_data[, i] - attr(scaled_original, "scaled:center")[i])/ 
      attr(scaled_original, "scaled:scale")[i]
  })
  
  energy_std <- as.numeric(miceDRF::energy_dist(X = scaled_original, 
                                                X_imp = scaled_imputed))
  
  data.frame(measure = c("energy", "energy_std"), score = c(energy, energy_std))
  
}


one_hot_encoding <- function(dat) {
  data.frame(mltools::one_hot(data.table::as.data.table(dat)))
}




scores_for_complete <- function(original_data, amputed_data, 
                                imputed_data, imputation_fun) {
  
  imputomics_measures <- imputomics:::calculate_measures(
    original_data, 
    amputed_data, 
    imputed_data,
    measures = c("mae", "rmse", "nrmse", "rsq", "ccc")
  ) %>% 
    rename(score = "value")
  
  energy <- as.numeric(miceDRF::energy_dist(X = original_data, 
                                            X_imp = imputed_data))
  scaled_original <- scale(original_data)
  
  scaled_imputed <- sapply(1:ncol(imputed_data), function(i) {
    (imputed_data[, i] - attr(scaled_original, "scaled:center")[i])/ 
      attr(scaled_original, "scaled:scale")[i]
  })
  
  energy_std <- as.numeric(miceDRF::energy_dist(X = scaled_original, 
                                                X_imp = scaled_imputed))
  
  feature_wise_wasserstein <- safe_score({
    mean(sapply(1:ncol(original_data), function(ith_col) 
      transport::wasserstein1d(as.matrix(original_data)[, ith_col], 
                               as.matrix(imputed_data)[, ith_col])), na.rm = TRUE)
  }, original_data, imputed_data)
  
  
  KLD  <- safe_score({ 
    median(FNN::KL.dist(as.matrix(original_data), 
                        as.matrix(imputed_data), 
                        floor(sqrt(nrow(original_data)))), na.rm = TRUE)
  }, original_data, imputed_data)
  
  entropic_wasserstein <- safe_score({ 
    T4transport::sinkhorn(as.matrix(original_data), 
                          as.matrix(imputed_data), 
                          lambda = 1)$distance
  }, original_data, imputed_data)
  
  sliced_wasserstein <- safe_score({ 
    T4transport::swdist(as.matrix(original_data), 
                        as.matrix(imputed_data))$distance
  }, original_data, imputed_data)
  
  
  rbind(imputomics_measures,
        data.frame(measure = c("energy", "energy_std", "feature_wise_wasserstein", 
                               "KLD", "entropic_wasserstein", "sliced_wasserstein"), 
                   score = c(energy, energy_std, feature_wise_wasserstein, 
                             KLD, entropic_wasserstein, sliced_wasserstein)))
}



stop_on_timeout <- function(missing_data_set, imputing_function, timeout_thresh = 600) {
  R.utils::withTimeout(imputing_function(missing_data_set), 
                       timeout = timeout_thresh, onTimeout = "error")
}




scores_for_incomplete <- function(original_data, imputed_data, imputation_fun,
                                  multiple, timeout_thresh, case, var_type) {
  
  #calculate IScore here
  
  if(case == "incomplete_categorical") {
    
    ids_categoricals <- which(sapply(original_data, is.factor))
    imputed_data <- mutate(imputed_data, across(matches(names(ids_categoricals)), as.factor))
    
    ImpScore <- try({
      miceDRF::Iscore_cat(X = original_data, X_imp = imputed_data, 
                          imputation_func = imputation_fun, var_type == "Factor", 
                          multiple = multiple)
    })
    score_name <- "IScore_cat"
  } else {
    ImpScore <- try({
      miceDRF::Iscore(X = original_data, X_imp = imputed_data, 
                      multiple = multiple, imputation_func = imputation_fun)
    })
    score_name <- "IScore"
  }
  
  if(inherits(ImpScore, "try-error")) {
    ImpScore <- NA
  } else {
    ImpScore <- as.numeric(ImpScore)
  }
  
  data.frame(measure = score_name, score = ImpScore)
}
