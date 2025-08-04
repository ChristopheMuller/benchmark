

impute_superimputer <- function(missdf) {
  methods <- c("mice_cart", "mice_pmm", "random", "mice_norm_predict", "areg")
  imp_list <- lapply(paste0("impute_", methods), get)
  SuperImputer::super_imputer(missdf, methods = methods, imp_fun_list = imp_list,
                              aggregation_method = "weighted_partition", N = 20, 
                              n_best = 3)$imputed[[1]]
}

impute_supersuperimputer <- function(missdf) {
  methods <- c("mice_cart", "areg", "hyperimpute", "regimpute", "missmda_pca_reg")
  imp_list <- lapply(paste0("impute_", methods), get)
  SuperImputer::super_imputer(missdf, methods = methods, imp_fun_list = imp_list,
                              aggregation_method = "weighted_partition", N = 3, 
                              n_best = 1)$imputed[[1]]
}


