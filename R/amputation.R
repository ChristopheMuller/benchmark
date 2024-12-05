
library(dplyr)

# general amputation function
ampute_dataset <- function(filepath, mechanism, ratio) {
  
  dat <- readRDS(filepath)
  
  if(!is.na(mechanism)) {
    dat <- try({
      get(mechanism)(dat, ratio)
    })
    
    if(inherits(dat, "try-error"))
      dat <- NA
  }
  
  dat  
}


# here we could define mechanisms
# some dummy MCAR examples below

classic_mar <- function(dat, ratio, ...) {
  produce_NA(data = dat, mechanism = "MAR", perc.missing = ratio, ...)$data.incomp
}

classic_mcar <- function(dat, ratio, ...) {
  produce_NA(data = dat, mechanism = "MCAR", perc.missing = ratio, ...)$data.incomp
}


#' Introduce Missingness in Variables Conditionally
#'
#' @param X A dataframe or matrix
#' @param p Probability of making other variables missing
#' @return Modified dataframe or matrix with potential missingness
#' 
#' @examples
#' X <- matrix(rnorm(1000), nrow = 100)
#' dist_shift(X, p = 0.4)
#' 

dist_shift <- function(X, p = 0.4, ratio = NULL,...) {
  
  p <- ratio + 0.2
  
  # Ensure X is a matrix or dataframe
  X <- as.data.frame(X, check.names = FALSE)
  
  # Get dimensions of X
  n_rows <- nrow(X)
  n_cols <- ncol(X)
  
  # Check if there are at least two columns
  if (n_cols < 2) {
    stop("Input matrix/dataframe must have at least two columns")
  }
  
  # Select the last two columns
  last_two_cols <- X[, (n_cols-1):n_cols]
  
  random_two_cols_id <- sample(1:ncol(X), 2, replace = FALSE)
  
  random_two_cols <- X[, random_two_cols_id]
  
  # Calculate column-wise means of the last two columns
  col_means <- colMeans(random_two_cols, na.rm = TRUE)
  
  # Create a logical matrix indicating where values exceed mean
  exceeds_mean <- t(apply(random_two_cols, 1, function(row) row > col_means))
  
  # Determine if row meets missingness condition
  trigger_missingness <- apply(exceeds_mean, 1, any)
  
  # Create a matrix of missingness probabilities
  missingness_matrix <- matrix(
    ifelse(trigger_missingness, p, 0), 
    nrow = n_rows, 
    ncol = n_cols
  )
  
  # Introduce missingness in remaining columns
  if (n_cols > 2) {
    for (i in setdiff(1:n_cols, random_two_cols_id)) {
      X[, i] <- ifelse(
        runif(n_rows) < missingness_matrix[, i], 
        NA, 
        X[, i]
      )
    }
  }
  
  return(as.data.frame(X, check.names = FALSE))
}



mechanism3 <- function(dat, ...) {
  missing_cols <- sample(1:ncol(dat), 5, replace = FALSE)
  dat <- as.matrix(dat)
  dat[, missing_cols][runif(nrow(dat) * 5) < 0.7] <- NA
  data.frame(dat, check.names = FALSE)
}


# amputation summary

summarize_amputation <- function(amputed_all, params) {
  # was the amputation successful? What would we like to know about amputed datasets?
  # did all the mechanisms work?
  
  amputation_ids <- names(amputed_all)
  
  amputation_res <- lapply(amputation_ids, function(i) {
    ith_amputed <- amputed_all[[i]]
    
    data.frame(amputed_id = str_remove(i, "amputed_dat_"),
               amputed_ratio = sum(is.na(ith_amputed))/prod(dim(ith_amputed)))
  }) %>% 
    bind_rows() 
  
  params %>% 
    select(set_id, amputed_id, case, mechanism, rep, ratio) %>% 
    unique() %>% 
    left_join(amputation_res, by = "amputed_id") %>% 
    select(-amputed_id) %>% 
    mutate(ratio = ratio,
           diff = round(abs(ratio - amputed_ratio), 2))
}


