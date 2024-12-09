
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

impute_impSeqRob <- function(missdf, ...) 
  eval_dimar_call(missdf, methods = "impSeqRob", ...)

impute_impSeq <- function(missdf, ...) 
  eval_dimar_call(missdf, methods = "impSeq", ...)

impute_minProb <- function(missdf, ...) 
  eval_dimar_call(missdf, methods = "MinProb", ...)

impute_norm <- function(missdf, ...)
  eval_dimar_call(missdf, methods = "norm", ...)

impute_SVTImpute <- function(missdf, ...) 
  eval_dimar_call(missdf, methods = "SVTImpute", ...)



