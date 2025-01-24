
library(targets)
library(tarchetypes)
library(purrr)
library(dplyr)
library(stringr)
library(energy)
library(reticulate)
library(tidyr)
library(clustermq)
library(parallel)

#imputations
library(imputomics)
library(miceDRF)
library(ImputeRobust)
library(mice)
library(glmnet)

# for vis
library(ggplot2)
library(patchwork)


source("./python/python_imputation_functions.R")

tar_source()



identical_outside_missing <- function(data, imputed_data, mask) {
  # Check if data and imputed_data have the same dimensions
  if (!all.equal(dim(data), dim(imputed_data))) {
    stop("Data and imputed data have different dimensions.")
  }
  
  # Check if mask has the same dimensions as data
  if (!all.equal(dim(data), dim(mask))) {
    stop("Mask and data have different dimensions.")
  }
  
  # Convert mask to logical
  mask <- mask == 1
  
  # Extract complete cases from data and imputed_data
  complete_data <- data[!mask]
  complete_imputed_data <- imputed_data[!mask]
  
  # Check if complete cases are identical
  return(all.equal(complete_data, complete_imputed_data))
}

# Function to verify output type and value set
verify_output <- function(imputed_data, original_data, k_values) {

    for (i in 1:ncol(imputed_data)) {
    if (any(!sapply(imputed_data[, i], function(x) any(abs(x - 1:k_values[i]) <= 0.02)))) {
      return(FALSE) 
    }
  }
  
  # Check if non-missing original values are preserved
  if (!identical_outside_missing(original_data, imputed_data, M)) {
    return(FALSE)
  }
  
  return(TRUE)
}


path_to_methods <- "./data/functions.RDS"


imputation_methods <- readRDS(path_to_methods) %>% 
  rename(imputation_fun = `Function name`) %>% 
  mutate(method = str_remove(imputation_fun, "impute_")) %>% 
  filter(method %in% c("eucknn", "mice_CALIBER"))


results <- data.frame(method = imputation_methods$method,
                      imputation_fun = imputation_methods$imputation_fun,
                      categorical_num = rep(0, nrow(imputation_methods)))


# 1. Categorical as numerical (only cat)

# Data set

n <- 100
p <- 4
k <- c(2, 3, 4, 5)

set.seed(123)

data <- data.frame(
  matrix(
    sapply(1:p, function(i) sample(1:k[i], n, replace = TRUE)), 
    ncol = p
  )
)


colnames(data) <- paste0("V", 1:p) 
print(head(data)) 

# Missing data

data <- mcar(data, 0.1)
# mask dummy
M <- 1 * is.na(data)

data <- data[apply(M, 1, sum) <= 3,]
M <- M[apply(M, 1, sum) <= 3,]

# remove rows with only 
  
# Imputation
  
  
for (i in 1:nrow(results)) {
  method <- results$method[i]
  imputation_fun <- results$imputation_fun[i]
  
  print(method)
  print(imputation_fun)
  
  if (method %in% c("sinkhorn")){
    mat_data <- as.matrix(data)
    data_temp <- matrix(as.double(mat_data), nrow = nrow(mat_data), ncol = ncol(mat_data))
    imputed_data <- do.call(imputation_fun, list(data_temp))
  } else {
    tryCatch({
      imputed_data <- do.call(imputation_fun, list(data))
    }, error = function(e) {
      print(e)
      imputed_data <- NA
    })
    
  }
  
  print(head(imputed_data))
  
  results$imputed_data[[i]] <- imputed_data
  has_not_changed <- identical_outside_missing(data, imputed_data, M)
  right_output <- verify_output(imputed_data, data, k)
  print(right_output)
  results$right_output[[i]] <- right_output * has_not_changed
  print(paste0(""))
  print(paste0(""))
}





