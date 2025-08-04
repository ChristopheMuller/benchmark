####
# Verify if M.I.
####

library(targets)
library(tarchetypes)
library(purrr)
library(dplyr)
library(imputomics)
library(miceDRF)
library(ImputeRobust)
library(stringr)
library(energy)
library(reticulate)
library(tidyr)
library(mice)
library(glmnet)

tar_option_set(
  resources = list(
    RETICULATE_PYTHON = "./.venv/Scripts/python.exe"
  )
)
tar_source()


# Fake Dataset with NAs
set.seed(123)
n <- 500
d <- 3
X <- matrix(rnorm(n*d), n, d)

M <- matrix(rnorm(n*d) < -0.50, n, d)
sum(M) / (n*d)

X[M] <- NA

id_rows_all_NAs <- which(rowSums(is.na(X)) == d)
if (length(id_rows_all_NAs) > 0){
X <- X[-id_rows_all_NAs,]
}

X <- as.data.frame(X)

##################################################################

imputation_functions <- c("impute_sklearn_iterative_post")

# for each imputation function, impute 3 times

list_results_MI <- list()

for (i in 1:length(imputation_functions)) {

  imputation_fun <- imputation_functions[i]
  function_imp <- get(imputation_fun)
  temp <- replicate(3, function_imp(X), simplify = FALSE)
  
  # check if the 3 replicates are identical, if yes => list_results_MI[[i]] = TRUE
  res <- all.equal(temp[[1]], temp[[2]], tolerance=0.001) == TRUE &
    all.equal(temp[[1]], temp[[3]], tolerance=0.001) == TRUE &
    all.equal(temp[[2]], temp[[3]], tolerance=0.001) == TRUE
  
  list_results_MI[[imputation_fun]] <- res
  
  print(paste0(imputation_fun, " : ", list_results_MI[[imputation_fun]]))
  
}

