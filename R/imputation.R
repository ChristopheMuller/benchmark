

safe_impute <- function(missing_data_set, 
                        imputing_function, 
                        timeout = 600, 
                        n_attempts = 3) {
  
  missing_data_set <- data.frame(missing_data_set)
  
  imputed <- structure(structure(list(), class = "try-error"))
  n <- 1
  
  while(inherits(imputed, "try-error") & n < n_attempts) {
    
    imputed <- try({ 
      suppressWarnings({
        R.utils::withTimeout(imputing_function(missing_data_set), 
                             timeout = timeout, onTimeout = "error")
      })
    })
    
    n <- n + 1
  }
  
  imputed
}



impute <- function(dataset_id, missing_data_set, imputing_function, 
                   timeout = 600, n_attempts = 3) {
  
  missing_data_set <- pre_process(missing_data_set)
  
  start_time <- Sys.time()
  imputed <- safe_impute(missing_data_set, imputing_function, timeout, n_attempts)
  time <- as.numeric(Sys.time() - start_time)
  
  if(inherits(imputed, "try-error")) {
    error <- ifelse(grepl("reached elapsed time limit", imputed[[1]]),
                    "timeout", "computational")
  } else {
    error <- validate_imputation(imputed, missing_data_set)
    imputed <- post_process(imputed)
  }
  
  res <- data.frame(imputed_id = dataset_id,
                    time= time,
                    error = error)
  
  list(imputed = imputed, 
       res = res)
  
}


validate_imputation <- function(imputed, missing_data_set) {
  
  if(! isTRUE(all.equal(imputed[!is.na(missing_data_set)], 
                 missing_data_set[!is.na(missing_data_set)])))
    return("modification")
  
  if(any(is.na(imputed)))
    return("missings")
  
  return(NA)
}



pre_process <- function(missing_data_set) {
  missing_data_set
}


post_process <- function(imputed) {
  imputed
}


summarize_imputations <- function(imputed_all, params) {
  
  results <- lapply(imputed_all, function(ith_imputed) {
    imputed_data <- ith_imputed[["imputed"]]
    res <- ith_imputed[["res"]]
    
    if(!is.na(res[["error"]])) {
      return(cross_join(res, data.frame(measure = c("mae", "rmse", "nrmse", 
                                                    "mpe", "mape", "rsq", 
                                                    "ccc", "energy", "IScore"),
                                        score = NA)))
    }
    
    params_one_row <- params %>% 
      filter(imputed_id == res[["imputed_id"]])
    
    original_data <- readRDS(params_one_row[["filepath_original"]])
    amputed_data <- readRDS(params_one_row[["filepath_amputed"]])
    imputation_fun <- get(params_one_row[["imputation_fun"]])

    if(params_one_row[["case"]] == "complete") {
      scores <- scores_for_complete(original_data, amputed_data, imputed_data,
                                    imputation_fun)
    } else {
      scores <- scores_for_incomplete(original_data, imputed_data, imputation_fun)
    }
    res %>% 
      cross_join(scores)
    
  }) %>% 
    bind_rows()
  
  params %>% 
    left_join(results, by = "imputed_id") %>% 
    dplyr::select(set_id, mechanism, ratio, rep, case, method, imputation_fun, 
                  time, error, measure, score)
}



