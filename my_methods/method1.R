

impute_method1 <- function(missdf) {
  
  missdf[is.na(missdf)] <- runif(1)
  
  missdf
}