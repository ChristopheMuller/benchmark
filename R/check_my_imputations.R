
# --------------------------------------------------------------------- multiple

check_mi <- function(imputation_methods) {
  
  missdf <- airquality
  
  res <- as.vector(sapply(imputation_methods[["imputation_fun"]], function(ith_imp) {
    imp1 <- try({get(ith_imp)(missdf)})
    imp2 <- try({get(ith_imp)(missdf)})
    
    if(inherits(imp1, "try-error"))
      return(NA)
    
    if(inherits(imp2, "try-error"))
      return(NA)
    
    same <- identical(imp1, imp2)
    !same
  }))
  
  imputation_methods %>% 
    mutate(MI = res)
}