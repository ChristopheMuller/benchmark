
calculate_scores <- function(imputed, amputed, imputation_fun, multiple, 
                             imputed_id, timeout_thresh, filepath_original, case) {
  
  imputed_data <- imputed[["imputed"]]
  res <- imputed[["res"]]
  
  if(!is.na(res[["error"]])) {
    return(cross_join(res, data.frame(measure = c("mae", "rmse", "nrmse", 
                                                  "rsq", "ccc", "energy", 
                                                  "IScore"),
                                      score = NA)))
  }
  
  original_data <- readRDS(filepath_original)
  
  if(case == "complete") {
    scores <- scores_for_complete(original_data = original_data, 
                                  amputed_data = amputed, 
                                  imputed_data = imputed_data,
                                  imputation_fun = imputation_fun)
  } else {
    scores <- scores_for_incomplete(original_data = original_data, 
                                    imputed_data = imputed_data, 
                                    imputation_fun = imputation_fun,
                                    multiple = multiple,
                                    timeout_thresh = timeout_thresh)
  }
  res %>% 
    cross_join(scores)
}






summarize_imputations <- function(all_scores, params) {
  
  params %>% 
    left_join(all_scores, by = "imputed_id") %>% 
    dplyr::select(set_id, mechanism, ratio, rep, case, method, imputation_fun, 
                  time, attempts, error, measure, score)
}





scores_for_complete <- function(original_data, amputed_data, imputed_data,
                                imputation_fun) {
  imputomics_measures <- imputomics:::calculate_measures(
    original_data, 
    amputed_data, 
    imputed_data,
    measures = c("mae", "rmse", "nrmse", "rsq", "ccc")
  ) %>% 
    rename(score = "value")
  energy <- as.numeric(miceDRF::energy_dist(X = original_data, 
                                            X_imp = imputed_data))
  rbind(imputomics_measures,
        data.frame(measure = c("energy"), 
                   score = c(energy)))
}



stop_on_timeout <- function(missing_data_set, imputing_function, timeout_thresh = 600) {
  R.utils::withTimeout(imputing_function(missing_data_set), 
                       timeout = timeout_thresh, onTimeout = "error")
}




scores_for_incomplete <- function(original_data, imputed_data, imputation_fun,
                                  multiple, timeout_thresh) {
  #calculate IScore here
  time_limited_fun <- function(missdf) stop_on_timeout(missdf, imputation_fun, timeout_thresh)
  
  ImpScore <- try({
    miceDRF::Iscore(X = original_data, X_imp = imputed_data, 
                    multiple = multiple, imputation_func = time_limited_fun)
  })
  
  ImpScore <- ifelse(inherits(ImpScore, "try-error"), NA, as.numeric(ImpScore))
  
  data.frame(measure = "IScore", score = ImpScore)
}



