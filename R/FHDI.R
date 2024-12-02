
eval_fhdi_call <- function(missdf, method, ...) {
  capture.output(suppressMessages(
    imputed <- FHDI::FHDI_Driver(missdf, s_op_imputation = method, ...)
  ))
  
  imputed[["simp.data"]]
}

impute_FEFI <- function(missdf, ...) 
  eval_fhdi_call(missdf, method = "FEFI", ...)


impute_FHDI <- function(missdf, ...) 
  eval_fhdi_call(missdf, method = "FHDI", ...)