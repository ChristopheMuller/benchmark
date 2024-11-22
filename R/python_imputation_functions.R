
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

reticulate::source_python("python/python_imputation_functions.py")

call_hyperimpute_fun <- function(missdf, method, ...) {
  column_names <- colnames(missdf)
  imputed <- hyperimpute_imp(missdf, method = method, ...)
  colnames(imputed) <- column_names
  imputed
}


impute_sinkhorn <- function(missdf, ...){
  call_hyperimpute_fun(missdf, method = "sinkhorn", ...)
}

impute_hyperimpute <- function(missdf, ...){
  call_hyperimpute_fun(missdf, method = "hyperimpute", ...)
}

impute_miwae <- function(missdf, ...){
  call_hyperimpute_fun(missdf, method = "miwae", ...)
}

impute_miracle <- function(missdf, ...){
  call_hyperimpute_fun(missdf, method = "miracle", ...)
}

impute_gain <- function(missdf, ...){
  call_hyperimpute_fun(missdf, method = "gain", ...)
}

impute_hyperimpute_em <- function(missdf, ...){
  call_hyperimpute_fun(missdf, method = "EM", ...)
}
