
eval_mice_call <- function(missdf, method, ...) {
  
  args <- list(...)
  args <- args[setdiff(names(args), c("data", "method", "printFlag"))]
  
  args <- c(list(data = missdf, method = method, printFlag = FALSE), args)
  
  suppressMessages(suppressWarnings(
    imputed <- do.call(mice::mice, args)
  ))
  
  mice::complete(imputed)
}


impute_mice_gamlss <- function(missdf, m = 1, ...) 
  eval_mice_call(missdf = missdf, method = "gamlss", m = m, maxit=1, n.cyc=1, 
                 bf.cyc=1, cyc=1, ...)

impute_mice_norm <- function(missdf, m = 1, ...)
  eval_mice_call(missdf = missdf, method = "norm", m = m, ...)

impute_mice_norm_predict <- function(missdf, m = 1, ...) 
  eval_mice_call(missdf = missdf, method = "norm.predict", m = m,...)

impute_mice_norm_nob <- function(missdf, m = 1, ...) 
  eval_mice_call(missdf = missdf, method = "norm.nob", m = m, ...)

impute_mice_midastouch <- function(missdf, m = 1, ...) 
  eval_mice_call(missdf = missdf, method = "midastouch", m = m, ...)

impute_mice_boot <- function(missdf, m = 1, ...) 
  eval_mice_call(missdf = missdf, method = "norm.boot", m = m, ...)

impute_mice_CALIBER <- function(missdf, m = 1, ...){
  col_is_factor <- sapply(missdf, function(x) is.factor(x))
  methods <- c("rfcont", "rfcat")
  methods_cols <- methods[col_is_factor + 1]
  eval_mice_call(missdf = missdf, method = methods_cols, m = m, ...)
}


impute_mice_pmm <- function(missdf, ...)
  imputomics::impute_mice_pmm(missdf = missdf, m = 1, ...)

impute_mice_cart <- function(missdf, ...)
  imputomics::impute_mice_cart(missdf = missdf, m = 1, ...)

impute_mice_rf <- function(missdf, ...)
  imputomics::impute_mice_rf(missdf = missdf, m = 1, ...)


