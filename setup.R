invisible(lapply(list.files("./R", full.names = TRUE, pattern = "\\.R$"), source))
library(dplyr); library(stringr); library(tools)

save_params <- function() {

  path_to_methods     <- "./data/functions.RDS"
  path_to_cat_methods <- "./data/categorical_funs.RDS"

  imputation_methods <- readRDS(path_to_methods) %>%
    rename(imputation_fun = `Function name`) %>%
    mutate(method = str_remove(imputation_fun, "impute_"))

  imputation_categorical <- readRDS(path_to_cat_methods)

  params <- create_params(
    path_to_complete_datasets                = "./data/datasets/complete/",
    path_to_incomplete_datasets              = "./data/datasets/incomplete/",
    path_to_categorical_datasets             = "./data/datasets/categorical/",
    path_to_incomplete_categorical_datasets  = "./data/datasets/incomplete_categorical/",
    path_to_amputed                          = "./results/amputed/",
    path_to_imputed                          = "./results/imputed/",
    amputation_mechanisms                    = c("mcar", "mar"),
    amputation_reps                          = 2,
    missing_ratios                           = c(0.1, 0.2, 0.3),
    imputation_methods                       = imputation_methods,
    imputation_categorical                   = imputation_categorical
  )

  saveRDS(params, "./data/params.RDS")
  cat("Params saved:", nrow(params), "rows\n")
}