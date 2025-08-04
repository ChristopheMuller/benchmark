library(dplyr)
library(tidyr)
library(ggplot2)

source("./R/imputation.R")

load_imputations_env()

source("./data/imputation_methods/categorical_imputation/supp.R")


imp_methods <- pull(readRDS("./data/functions.RDS"), `Function name`)

run_methods <- function(imp_methods, missdf) {
  ncols <- ncol(missdf)
  
  lapply(imp_methods, function(ith_imp) {
    print(ith_imp)
    
    imp1 <- try({get(ith_imp)(missdf)})
    
    if(inherits(imp1, "try-error"))
      return(  data.frame(method = ith_imp, successful = FALSE, ncols = ncols))
    
    data.frame(method = ith_imp, successful = TRUE, ncols = ncols)
  }) %>% 
    bind_rows()
}

# check on 2 columns

set.seed(1001)
dat <- data.frame(MASS::mvrnorm(100, c(0, 0), diag(2)))
missdf <- dat
missdf[1:20, 1] <- NA

missdf <- missdf[sample(1:nrow(missdf), nrow(missdf), replace = FALSE), ]

res_2_cols <- run_methods(imp_methods, missdf)


# check on 3 columns

set.seed(1001)
dat <- data.frame(MASS::mvrnorm(100, c(0, 0, 0), diag(3)))
missdf <- dat
missdf[1:30, 1] <- NA


res_3_cols <- run_methods(imp_methods, missdf)


# check on 4 columns

set.seed(1001)
dat <- data.frame(MASS::mvrnorm(100, c(0, 0, 0, 0), diag(4)))
missdf <- dat
missdf[1:30, 1] <- NA

res_4_cols <- run_methods(imp_methods, missdf)

# check on 5 columns

set.seed(1001)
dat <- data.frame(MASS::mvrnorm(100, c(0, 0, 0, 0, 0), diag(5)))
missdf <- dat
missdf[1:30, 1] <- NA

res_5_cols <- run_methods(imp_methods, missdf)





res <- rbind(res_2_cols, res_3_cols, res_4_cols, res_5_cols)

res %>% 
  dplyr::filter(!(method %in% c("impute_engression", "impute_missmda_MIFAMD_reg",
                         "impute_missmda_famd_reg", "impute_missmda_famd_em",
                         "impute_missmda_MIFAMD_em"))) %>% 
  group_by(method) %>% 
  mutate(mean_perf = sum(successful)) %>% 
  ggplot(aes(x = reorder(method, mean_perf), y = ncols, fill = successful)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90))

