library(dplyr)
library(tidyr)

source("./R/imputation.R")

load_imputations_env()

source("./data/imputation_methods/categorical_imputation/supp.R")


imp_methods <- pull(readRDS("./data/functions.RDS"), `Function name`)

missdf <- airquality

res <- lapply(imp_methods, function(ith_imp) {
  print(ith_imp)
  
  imp1 <- try({get(ith_imp)(missdf)})
  
  imp2 <- try({get(ith_imp)(missdf)})
  
  if(inherits(imp1, "try-error"))
    return(  data.frame(method = ith_imp, MI = NA))
  
  if(inherits(imp2, "try-error"))
    return( data.frame(method = ith_imp, MI = NA))
  
  same <- identical(imp1, imp2)
  data.frame(method = ith_imp, MI = !same)
})

res_bind <- res %>%  bind_rows()


ith_imp <- "impute_missmda_MIFAMD_em"
