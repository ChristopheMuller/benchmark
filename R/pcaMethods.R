
impute_llsImpute <- function(missdf, ...) {
  
  k <- min(ncol(missdf) - 1, 10)
  imputed <- pcaMethods::llsImpute(missdf, k = k, completeObs = TRUE, ...)
  
  pcaMethods::completeObs(imputed)
}
