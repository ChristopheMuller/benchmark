
impute_areg_tmp <- function(missdf, type_imp = "pmm", verbose = FALSE, ...) {
  
  all_args <- imputomics:::extend_arglist(
    list(...),
    list(formula = stats::as.formula(paste("~", paste(colnames(missdf),
                                                      collapse = "+"))),
         data = missdf, n.impute = 1, type = type_imp),
    list(burnin = 5, nk = 0)
  )
  
  imputomics:::silence_function(verbose)(imputed <- do.call(Hmisc::aregImpute, all_args))
  
  data.frame(do.call(cbind, Hmisc::impute.transcan(imputed,
                                                   imputation = 1,
                                                   data = missdf,
                                                   list.out = TRUE,
                                                   pr = FALSE,
                                                   check = FALSE)))
}


impute_areg <- function(missdf) {
  impute_areg_tmp(missdf, type_imp = "pmm")
}

impute_areg_regression <- function(missdf) {
  impute_areg_tmp(missdf, type_imp = "regression")
}

impute_areg_normpmm <- function(missdf) {
  impute_areg_tmp(missdf, type_imp = "normpmm")
}