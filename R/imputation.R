
check_time_error <- function(imputed) {
  grepl("TimeoutError", imputed[[1]]) |
    grepl("reached CPU time limit", imputed[[1]]) | 
    grepl("reached elapsed time limit", imputed[[1]]) |
    grepl("User interrupt", imputed[[1]])
}



# safe_impute <- function(missing_data_set, 
#                         imputing_function, 
#                         timeout = 600, 
#                         n_attempts = 3) {
  
#   missing_data_set <- data.frame(missing_data_set)

  
#   imputed <- structure(structure(list(), class = "try-error"))
#   n <- 1
  
#   while(inherits(imputed, "try-error") & n <= n_attempts) {
    
#     start_time <- Sys.time()
#     imputed <- try({ 
#       suppressWarnings({
#         R.utils::withTimeout(imputing_function(missing_data_set), 
#                              timeout = timeout, onTimeout = "error")
#       })
#     })
#     time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
#     n <- n + 1
#   }
  
#   list(imputed = imputed, time = time, attempts = n - 1)
# }


library(parallel)

safe_impute <- function(missing_data_set, imputing_function, timeout = 600, n_attempts = 3) {
  missing_data_set <- data.frame(missing_data_set)
  imputed <- structure(list(), class = "try-error")
  time <- NA
  n <- 1
  
  while (inherits(imputed, "try-error") && n <= n_attempts) {
    start_time <- Sys.time()
    
    # Launch the imputation in a separate process
    impute_process <- mcparallel({
      tryCatch(
        imputing_function(missing_data_set),
        error = function(e) structure(list(), class = "try-error")
      )
    }, mc.set.seed = TRUE)
    
    # Collect the result with a timeout
    result <- tryCatch({
      collected <- mccollect(impute_process, wait = TRUE, timeout = timeout)
      if (is.null(collected) || length(collected) == 0 || inherits(collected[[1]], "try-error")) {
        stop("Timeout or error during imputation.")
      }
      collected[[1]]
    }, error = function(e) {
      # Forcefully kill process if timeout/error occurs
      if (!is.null(impute_process$pid)) {
        system2("kill", as.character(impute_process$pid))
      }
      structure(list(), class = "try-error")
    })
    
    time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    imputed <- result
    n <- n + 1
  }
  
  list(imputed = imputed, time = time, attempts = n - 1)
}



impute <- function(dataset_id, missing_data_set, imputing_function, 
                   timeout = 600, n_attempts = 3) {
  
  missing_data_set <- pre_process(missing_data_set)
  
  col_names <- colnames(missing_data_set)
  
  imputed <- safe_impute(missing_data_set, imputing_function, timeout, n_attempts)
  
  time <- imputed[["time"]]
  attempts <- imputed[["attempts"]]
  imputed <- imputed[["imputed"]]
  
  if(inherits(imputed, "try-error")) {
    
    error <- ifelse(check_time_error(imputed), "timeout", "computational")
    
  } else {
    error <- validate_imputation(imputed, missing_data_set)
    imputed <- post_process(imputed)
    colnames(imputed) <- col_names
  }
  
  res <- data.frame(imputed_id = dataset_id,
                    time = time,
                    attempts = attempts,
                    error = error)
  
  list(imputed = imputed, 
       res = res)
  
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



pre_process <- function(missing_data_set) {
  missing_data_set
}


post_process <- function(imputed) {
  imputed
}


