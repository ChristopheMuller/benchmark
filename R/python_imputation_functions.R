
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


library(reticulate)
PATH_PYTHON <- "./.venv/Scripts/python.exe"
# PATH_PYTHON <- "C:\\Users\\Chris\\anaconda3\\envs\\benchmark\\python.exe"


use_python(PATH_PYTHON, required = TRUE)
source_python("python/python_imputation_functions.py")

impute_sinkhorn <- function(missdf, ...){
  hyperimpute_imp(missdf, method="sinkhorn")
}

impute_hyperimpute <- function(missdf, ...){
  hyperimpute_imp(missdf, method="hyperimpute")
}

impute_miwae <- function(missdf, ...){
  hyperimpute_imp(missdf, method="miwae")
}

impute_miracle <- function(missdf, ...){
  hyperimpute_imp(missdf, method="miracle")
}

impute_gain <- function(missdf, ...){
  hyperimpute_imp(missdf, method="gain")
}

impute_hyperimpute_em <- function(missdf, ...){
  hyperimpute_imp(missdf, method="EM")
}