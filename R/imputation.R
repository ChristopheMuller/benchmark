
load_imputations_env <- function() {
  library(imputomics)
  library(miceDRF)
  library(ImputeRobust)
  library(mice)
  library(glmnet)
  library(CALIBERrfimpute)
  
  source("python/python_imputation_functions.R")
  
  targets::tar_source()
}


check_time_error <- function(imputed) {
  grepl("timeout_threshError", imputed[[1]]) |
    grepl("reached CPU time limit", imputed[[1]]) | 
    grepl("reached elapsed time limit", imputed[[1]]) |
    grepl("User interrupt", imputed[[1]]) | 
    grepl("callr timed out", imputed[[1]])
}



safe_impute <- function(missing_data_set, 
                        imputing_function, 
                        timeout_thresh = 600, 
                        n_attempts = 3) {
  
  missing_data_set <- data.frame(missing_data_set)
  
  imputed <- structure(structure("error", class = "try-error"))
  n <- 1
  
  while((inherits(imputed, "try-error") & n <= n_attempts) && !(check_time_error(imputed))) {
    
    start_time <- Sys.time()
    
    imputed <- try({ 
      callr::r(
        function(data, func, load_imputations_env) {
          load_imputations_env()
          
          start_time <- Sys.time()
          
          # imputation is here
          imputed <- func(data)
          
          time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
          
          list(imputed = imputed, time = time)
        }, 
        timeout = timeout_thresh, 
        args = list(data = missing_data_set, 
                    func = imputing_function, 
                    load_imputations_env = load_imputations_env)
      )
    })
    
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    if(is.null(imputed))
      imputed <- structure("Computational error!", class = "try-error")
    
    n <- n + 1
  }
  
  # set time to total time in case of failure
  if(inherits(imputed, "try-error"))
    imputed <- list(imputed = imputed, time = total_time)
  else {
    imputed[["imputed"]] <- as.data.frame(imputed[["imputed"]], check.names = FALSE)
  }
  c(imputed, attempts = n - 1)
}



impute <- function(dataset_id, missing_data_set, imputing_function, 
                   timeout_thresh = 600, n_attempts = 3, var_type, case) {
  
  load_imputations_env()
  
  if(case %in% c("categorical", "incomplete_categorical")) {
    cat_columns <- which(sapply(missing_data_set, is.factor))
    unique_categoricals <- lapply(cat_columns, function(i) {
      as.numeric(attr(missing_data_set[, i], "levels"))
    })
    missing_data_set <- pre_process(missing_data_set, imputing_function, var_type)
  }
  
  imputing_function <- get(imputing_function)
  
  col_names <- colnames(missing_data_set)
  
  # call safe imputation
  imputed <- safe_impute(missing_data_set, imputing_function, timeout_thresh, n_attempts)
  
  time <- imputed[["time"]]
  attempts <- imputed[["attempts"]]
  imputed <- imputed[["imputed"]]
  
  if(inherits(imputed, "try-error")) {
    
    error <- ifelse(check_time_error(imputed), "timeout", "computational")
    
  } else {
    error <- validate_imputation(mutate_all(imputed, as.numeric), 
                                 mutate_all(missing_data_set, as.numeric))
    imputed <- post_process(imputed)
    colnames(imputed) <- col_names
    
    if(case %in% c("categorical", "incomplete_categorical")) {
        error_categorical <- validate_categorical(imputed, unique_categoricals)
      
      if(!is.na(error_categorical)) {
        if(is.na(error)){
          error <- error_categorical
        } else {
          error <- paste0(error, "+", error_categorical)
        }
      }
    }
  }
  
  res <- data.frame(imputed_id = dataset_id,
                    time = time,
                    attempts = attempts,
                    error = error)
  
  list(imputed = imputed, 
       res = res)
  
}

validate_categorical <- function(imputed, unique_categoricals) {
  cat_cols <- names(unique_categoricals)
  
  check <- all(sapply(cat_cols, function(ith_col) {
    all(unique(imputed[[ith_col]]) %in% unique_categoricals[[ith_col]])
  }))
  
  if(!check) {
    return("wrong_levels")
  }
  
  NA
}


validate_imputation <- function(imputed, missing_data_set) {
  
  if(! isTRUE(all.equal(imputed[!is.na(missing_data_set)],
                        missing_data_set[!is.na(missing_data_set)], 
                        tolerance = 1.5e-5)))
    return("modification")
  
  if(any(is.na(imputed)))
    return("missings")
  
  return(NA)
}



pre_process <- function(missing_data_set, imputing_function, var_type) {
  
  if (var_type == "Numeric") {
    factor_cols <- sapply(missing_data_set, is.factor)
    missing_data_set[factor_cols] <- lapply(missing_data_set[factor_cols], factor_to_numeric)
    
    missing_data_set[, !sapply(missing_data_set, is.numeric)] <- as.numeric(missing_data_set[, !sapply(missing_data_set, is.numeric)])
  }
  
  missing_data_set
}


post_process <- function(imputed) {
  imputed
}


factor_to_numeric <- function(x) {
  as.numeric(levels(x))[x]
}


