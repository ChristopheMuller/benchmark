

safe_impute <- function(missing_data_set, 
                        imputing_function, 
                        timeout = 600, 
                        n_attempts = 3) {
  
  missing_data_set <- data.frame(missing_data_set)
  
  imputed <- structure(structure(list(), class = "try-error"))
  n <- 1
  while(inherits(imputed, "try-error") & n < n_attempts) {
    
    imputed <- try({ 
      R.utils::withTimeout(imputing_function(missing_data_set), 
                           timeout = timeout, onTimeout = "error")
    })
    
    n <- n + 1
  }
  
  imputed
}

