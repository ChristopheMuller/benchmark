
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
  eval_mice_call(missdf = missdf, method = "gamlss", m = m, ...)

impute_mice_norm <- function(missdf, m = 1, ...)
  eval_mice_call(missdf = missdf, method = "norm", m = m, ...)

impute_mice_norm_predict <- function(missdf, m = 1, ...) 
  eval_mice_call(missdf = missdf, method = "norm.predict", m = m,...)

impute_mice_norm_nob <- function(missdf, m = 1, ...) 
  eval_mice_call(missdf = missdf, method = "norm.nob", m = m, ...)


