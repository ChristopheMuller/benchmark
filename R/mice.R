
eval_mice_call <- function(missdf, method, ...) {
  
  args <- list(...)
  args <- args[setdiff(names(args), c("data", "method", "printFlag"))]
  
  args <- c(list(data = missdf, method = method, printFlag = FALSE), args)
  
  suppressMessages(suppressWarnings(
    imputed <- do.call(mice::mice, args)
  ))
  
  mice::complete(imputed)
}


impute_mice_gamlss <- function(missdf, m = 1, threshold=0.9999, ...) 
  eval_mice_call(missdf = missdf, method = "gamlss", m = m, maxit=1, n.cyc=1, 
                 bf.cyc=1, cyc=1, threshold=threshold, ...)

impute_mice_norm <- function(missdf, m = 1, threshold=0.9999, ...)
  eval_mice_call(missdf = missdf, method = "norm", m = m, threshold=threshold, ...)

impute_mice_norm_predict <- function(missdf, m = 1, threshold=0.9999, ...) 
  eval_mice_call(missdf = missdf, method = "norm.predict", m = m, threshold=threshold, ...)

impute_mice_norm_nob <- function(missdf, m = 1, threshold=0.9999, ...) 
  eval_mice_call(missdf = missdf, method = "norm.nob", m = m, threshold=threshold, ...)

impute_mice_midastouch <- function(missdf, m = 1, threshold=0.9999, ...) 
  eval_mice_call(missdf = missdf, method = "midastouch", m = m, threshold=threshold, ...)

impute_mice_boot <- function(missdf, m = 1, threshold=0.9999, ...) 
  eval_mice_call(missdf = missdf, method = "norm.boot", m = m, threshold=threshold, ...)

impute_mice_CALIBER <- function(missdf, m = 1, threshold=0.9999, ...){
  col_is_factor <- sapply(missdf, function(x) is.factor(x))
  methods <- c("rfcont", "rfcat")
  methods_cols <- methods[col_is_factor + 1]
  eval_mice_call(missdf = missdf, method = methods_cols, m = m, threshold=threshold, ...)
}

# impute_mice_polyreg <- function(missdf, m = 1, threshold=0.9999, ...) 
#   eval_mice_call(missdf = missdf, method = "polyreg", m = m, threshold=threshold, ...)
# 
# impute_mice_lda <- function(missdf, m = 1, threshold=0.9999, ...) 
#   eval_mice_call(missdf = missdf, method = "lda", m = m, threshold=threshold, ...)

impute_mice_pmm <- function(missdf, m = 1, threshold=0.9999, ...)
  eval_mice_call(missdf = missdf, method = "pmm", m = m, threshold=threshold, ...)

impute_mice_cart <- function(missdf, m = 1, threshold=0.9999, ...)
  eval_mice_call(missdf = missdf, method = "cart", m = m, threshold=threshold, ...)

impute_mice_rf <- function(missdf, m = 1, threshold=0.9999, ...)
  eval_mice_call(missdf = missdf, method = "rf", m = m, threshold=threshold, ...)

impute_mice_default <- function(missdf, m = 1, threshold=0.9999, ...) 
  eval_mice_call(missdf = missdf, method = NULL, m = m, threshold=threshold, ...)

impute_mice_cart50 <- function(missdf, m = 1, threshold=0.9999, ...)
  impute_mice_cart(missdf, m = 1, maxit = 50, threshold = threshold, ...)

impute_mice_cart100 <- function(missdf, m = 1, threshold=0.9999, ...)
  impute_mice_cart(missdf, m = 1, maxit = 100, threshold = threshold, ...)

