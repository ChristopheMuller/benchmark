

scores_for_complete <- function(original_data, amputed_data, imputed_data,
                                imputation_fun) {
  
  imputomics_measures <- imputomics:::calculate_measures(original_data, 
                                                         amputed_data, 
                                                         imputed_data) %>% 
    rename(score = "value")
  # energy <- as.numeric(miceDRF::energy_dist(X = original_data, 
  #                                           X_imp = imputed_data))
  energy <- NULL  ## BUG WITH MICE DRF
  # no IScore for complete data
  # ImpScore <- miceDRF::Iscore(X = amputed_data, imputation_func = imputation_fun)
  
  rbind(imputomics_measures,
        data.frame(measure = c("energy"), 
                   score = c(energy)))
}


scores_for_incomplete <- function(original_data, imputed_data, imputation_fun) {
  
  #calculate IScore here
  safe_imputation_fun <- function(missdf) safe_impute(missdf, imputation_fun)
  
  # ImpScore <- miceDRF::Iscore(original_data, 
  #                             imputation_func = safe_imputation_fun)
  ImpScore <- NULL  ## BUG WITH MICE DRF
  data.frame(measure = "IScore", score = as.numeric(ImpScore))
}



