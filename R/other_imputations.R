
impute_minProb <- function(missdf, ...) {
  imputeLCMD::impute.MinProb(missdf, ...)
}

impute_random <- function(missdf) {
  compute_values <- imputomics:::compute_col_random
  imputed_values_vector <- compute_values(missdf)
  do.call(cbind, lapply(1L:ncol(missdf), function(ith_column_id) {
    imputomics:::impute_constant(missdf[ith_column_id], 
                                 imputed_values_vector[[ith_column_id]])
  }))
}


