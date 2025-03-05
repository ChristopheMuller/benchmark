####
# Verify if M.I.
####

n <- 200
d <- 2
d.cat <- 2

X_num <- matrix(rnorm(n*d), n, d) + 50
X_cat <- matrix(sample(c(letters[1:3]), n*d.cat, replace = TRUE), n, d.cat)

X_orig <- cbind(as.data.frame(X_num), as.data.frame(X_cat))
colnames(X_orig) <- c(paste0("num", 1:d), paste0("cat", 1:d.cat))

for (col in (d+1):(d+d.cat)) {
  X_orig[[col]] <- as.factor(X_orig[[col]])
}

X <- model.matrix(~ . - 1, data = X_orig, contrasts.arg = lapply(X_orig[, (d+1):(d+d.cat)], contrasts, contrasts = FALSE))


### MISSINGNESS

M <- matrix(rnorm(n * (d+d.cat)) < -0.50, n, d+d.cat)
(sum(M) / (n * (d+d.cat)) * 100)

X_na <- X  # Create a copy to introduce missing values

# Indices of categorical variables in the original data
cat_cols <- (d + 1):(d + d.cat)  # Columns of categorical variables in X_orig

# Iterate through each column of M
for (j in 1:(d + d.cat)) {
  if (j <= d) {
    # Numeric variable: directly apply M
    X_na[, j] <- ifelse(M[, j], NA, X_na[, j])
  } else {
    # Categorical variable: set all related dummy variables to NA
    cat_levels <- grep(paste0("cat", j - d), colnames(X), value = TRUE)
    for (level in cat_levels) {
      X_na[, level] <- ifelse(M[, j], NA, X_na[, level])
    }
  }
}

# Remove rows with all NAs
id_rows_all_NAs <- which(rowSums(is.na(X_na)) == (d + d.cat))
if (length(id_rows_all_NAs) > 0) {
  X_na <- X_na[-id_rows_all_NAs, ]
}
X_na <- as.data.frame(X_na)


###############################################

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
source("python/python_imputation_functions.R")


all_methods <- readRDS("~/INRIA/R_scripts/benchmark/data/functions.RDS")

(list_methods <- all_methods[66])

methods_cat_save <- list()
for (meth in list_methods){
  
  print(meth)
  
  function_imp <- get(meth)

  temp <- function_imp(X_na)

  # Aggregate all columns categorical
  
  all_cat <- temp[,(d+1):(dim(temp)[2])]
  # make one unique vector out of it 
  all_cat <- as.vector(t(all_cat))
  
  # save the unique values
  methods_cat_save[[meth]] <- temp[,(d+1):(dim(temp)[2])]
  
}


for (meth in list_methods){
  print("")
  print("")
  print(meth)
  
  # plot histograms of the unique values
  
  all_cat <- as.vector(t(methods_cat_save[[meth]]))
  
  hist(all_cat, main = meth)
  
  # value counts
  print(table(all_cat)[1:5])
  hist(apply(methods_cat_save[[meth]], 1, function(x) sum(x)), main = meth)
  print(table(apply(methods_cat_save[[meth]], 1, function(x) sum(x)))[1:5])
  
}


