

scores_for_complete <- function(original_data, amputed_data, imputed_data) {
  
  imputomics_measures <- imputomics:::calculate_measures(original_data, 
                                                         amputed_data, 
                                                         imputed_data) %>% 
    rename(score = "value")
  
  energy <- as.numeric(miceDRF::energy_dist(X = original_data, 
                                            X_imp = imputed_data))
  
  # IScore <- ....
  IScore <- NA
  
  rbind(imputomics_measures,
        data.frame(measure = c("energy", "IScore"), 
                   score = c(energy, IScore)))
}



scores_for_incomplete <- function(original_data, imputed_data) {
  #calculate IScore here
  IScore <- NA
  
  data.frame(measure = "IScore", score = IScore) 
}
