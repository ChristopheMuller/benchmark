# put all the dependencies here:

library(mice) # example

# this is an example
# the name of the imputing functioin or it's wrapper should start wth "impute_"
# INPUT: incomplete data as an argument
# OUTPUT: return an imputed data 

impute_my_method <- function(missdf, ...) {
  
  missdf[is.na(missdf)] <- 17 # example imputation
  
  missdf # return imputed dataset
  
}
