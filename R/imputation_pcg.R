


impute_SVTImpute <- function(missdf, ...) {
  missdf <- as.matrix(missdf)
  imputed <- imputation::SVTImpute(missdf, 3)[["x"]]
  imputed <- as.data.frame(imputed, check_names = FALSE)
  imputed
}

impute_SVDImpute <- function(missdf, ...) 
  imputation::SVDImpute(missdf, 3)[["x"]]
