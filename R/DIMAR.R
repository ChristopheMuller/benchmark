
eval_dimar_call <- function(missdf, methods, ...) {
  
  missdf <- as.matrix(missdf)
  
  capture.output(suppressMessages(suppressWarnings(
    imputed <- DIMAR::dimar(mtx = missdf, methods = methods)
  )))
  
  imputed[["Imputation"]]
}

impute_dimar <- function(missdf, ...) 
  eval_dimar_call(missdf, methods = NULL, ...)

impute_dimar_fast <- function(missdf, ...) 
  eval_dimar_call(missdf, methods = "fast", ...)



