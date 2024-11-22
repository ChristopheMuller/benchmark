

scores_for_complete <- function(original_data, amputed_data, imputed_data,
                                imputation_fun) {
  
  imputomics_measures <- imputomics:::calculate_measures(original_data, 
                                                         amputed_data, 
                                                         imputed_data) %>% 
    rename(score = "value")
  
  energy <- as.numeric(miceDRF::energy_dist(X = original_data, 
                                            X_imp = imputed_data))
  
  ImpScore <- miceDRF::Iscore(original_data, imputed_data, 
                              imputation_func = imputation_fun)
  
  rbind(imputomics_measures,
        data.frame(measure = c("energy", "IScore"), 
                   score = c(energy, ImpScore)))
}



scores_for_incomplete <- function(original_data, imputed_data, imputation_fun) {
  #calculate IScore here
  ImpScore <- miceDRF::Iscore(original_data, imputed_data, 
                              imputation_func = imputation_fun)
  
  data.frame(measure = "IScore", score = ImpScore) 
}
