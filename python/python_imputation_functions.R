
# THIS IS AN EXAMPLE!
# the name of the imputing function or its wrapper should start with "impute_"
# INPUT: incomplete data as an argument
# OUTPUT: return an imputed data 

# do not remove the line below!
reticulate::source_python("python/python_imputation_functions.py") 

# the name of the imputing function or its wrapper should start with "impute_"
# INPUT: incomplete data as an argument
# OUTPUT: return an imputed data 

impute_my_python_method <- function(missdf, ...) {
  missdf <- make_integer_double(missdf)
  
  result <- my_python_method(missdf)
  
  result 
}




# ***************************** small helper ***********************************

# Helper function converting integer columns to double. This is required before 
# exporting data to Python, because Python may not correctly handle R's integer 
# NA values and replaces them with -2147483648 (INT32 minimum) instead of proper 
# missing values.

make_integer_double <- function(dat){
  
  for (i in 1:ncol(dat)){
    if (is.integer(dat[[i]])){
      dat[[i]] <- as.double(dat[[i]])
    }
  }
  return(dat)
}
