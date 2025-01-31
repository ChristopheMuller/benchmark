
### TO USE PYTHON FUNCTIONS
# 1. conda create --name benchmark python=3.10
# 2. conda activate benchmark
# 2. pip install -r python/python_requirements.txt
# 3. which python #=> Fill PATH below with the output

### OR (IOS dependent)
# sh python/windows_setup.sh
# or
# sh python/linux_setup.sh
####

# library(reticulate)
# PATH_PYTHON <- "./.venv/Scripts/python.exe"
# PATH_PYTHON <- "C:\\Users\\Chris\\anaconda3\\envs\\benchmark\\python.exe"


# use_python(PATH_PYTHON, required = TRUE)


reticulate::source_python("python/python_imputation_functions.py")

make_integer_double <- function(data.frame){
  
  for (i in 1:ncol(data.frame)){
    if (is.integer(data.frame[[i]])){
      data.frame[[i]] <- as.double(data.frame[[i]])
    }
  }
  
  return(data.frame)
  
}

call_hyperimpute_fun <- function(missdf, method, ...) {
  seed <- sample(1:100000, 1)
  column_names <- colnames(missdf)
  imputed <- hyperimpute_imp(missdf, method = method, seed=seed, ...)
  colnames(imputed) <- column_names
  imputed
}


impute_sinkhorn <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  call_hyperimpute_fun(missdf, method = "sinkhorn", ...)
}

impute_hyperimpute <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  call_hyperimpute_fun(missdf, method = "hyperimpute", ...)
}

impute_miwae <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  call_hyperimpute_fun(missdf, method = "miwae", ...)
}

impute_miracle <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  call_hyperimpute_fun(missdf, method = "miracle", ...)
}

impute_gain <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  call_hyperimpute_fun(missdf, method = "gain", ...)
}

impute_hyperimpute_em <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  call_hyperimpute_fun(missdf, method = "EM", ...)
}

impute_sklearn_iterative_post <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  iterative_imp(missdf, post=TRUE, ...)
}

impute_sklearn_iterative <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  iterative_imp(missdf, post=FALSE, ...)
}

impute_remasker <- function(missdf, ...){
  missdf <- make_integer_double(missdf)
  seed <- sample(1:100000, 1)
  remasker_imp(missdf, seed, ...)
}