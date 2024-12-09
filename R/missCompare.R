
impute_mice_mixed <- function(missdf) {
  imputed <- missCompare::impute_data(missdf, scale = FALSE, n.iter = 1, sel_method = 11)
  data.frame(imputed[["mice_mixed_imputation"]][[1]])
}

impute_nlpca <- function(missdf) {
  imputed <- missCompare::impute_data(missdf, scale = FALSE, n.iter = 1, sel_method = 10)
  data.frame(imputed[["pcaMethods_NLPCA_imputation"]][[1]])
}