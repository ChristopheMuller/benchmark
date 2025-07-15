
library(dplyr)
library(tidyr)

source("./R/imputation.R")

load_imputations_env()

source("./data/imputation_methods/categorical_imputation/supp.R")

set.seed(1)

n <- 40
p <- 2

base_dat <- as.data.frame(matrix(rnorm(n * p, 0, 1), ncol = p))
categorical_col <- sample(1:3, size = n, replace = TRUE, prob = c(0.2, 0.4, 0.4))

mean(categorical_col)


M <- matrix(runif(n * (p + 1)) <=  0.15, ncol = p + 1)
rowSums(M)


### case 2 integer category

base_dat2 <- mutate(base_dat, category = as.integer(categorical_col))
base_dat2[M] <- NA

### case 3 factor category

base_dat3 <- mutate(base_dat, category = factor(categorical_col, levels = 1:3))
base_dat3[M] <- NA

### case 4 one-hot encoding

base_dat4 <- mutate(base_dat2, 
                    category1 = ifelse(category == 1, 1, 0),
                    category2 = ifelse(category == 2, 1, 0),
                    category3 = ifelse(category == 3, 1, 0)) %>% 
  dplyr::select(-category)

### case 5 factor character category

base_dat5 <- mutate(base_dat, category = factor(paste0("category", categorical_col), 
                                                levels = paste0("category", 1:3)))
base_dat5[M] <- NA



###########################################################################

imp_methods <- pull(readRDS("./data/functions.RDS"), `Function name`)

imp_methods <- imp_methods[!(imp_methods %in% c("impute_mice_cart100",
                                                "impute_superimputer",
                                                "impute_mice_cart50",
                                                "impute_engression"))]

case <- c("base_dat2", "base_dat3", "base_dat4", "base_dat5")


res_all <- lapply(case, function(ith_set) {
  print(ith_set)
  
  dat <- get(ith_set)
  
  lapply(imp_methods, function(ith_method) {
    print(ith_method)
    
    names_cols <- colnames(dat)
    
    imp_dat <- try({get(ith_method)(dat)}, silent = TRUE)
    
    if(inherits(imp_dat, "try-error")) {
      if(ith_set == "base_dat4")
        return(data.frame(check = c("binary_levels", "binary_sum"), res = c(NA, NA),
                          method = c(ith_method, ith_method), case = c(ith_set, ith_set)))
      else
        return(data.frame(check = "levels", res = NA, method = ith_method, case = ith_set))
    }
    
    
    colnames(imp_dat) <- names_cols
    imp_dat <- as.data.frame(imp_dat)
    
    if(any(is.na(imp_dat))) {
      if(ith_set == "base_dat4")
        return(data.frame(check = c("binary_levels", "binary_sum"), res = c(NA, NA),
                          method = c(ith_method, ith_method), case = c(ith_set, ith_set)))
      else
        return(data.frame(check = "levels", res = NA, method = ith_method, case = ith_set))
    }
    
    if(ith_set == "base_dat4") {
      res <- data.frame(check = c("binary_levels", "binary_sum"), 
                        res = c(check_binary_levels(imp_dat), 
                                check_binary_sum(imp_dat)))
    } else {
      res <- data.frame(check = c("levels"), res = c(check_levels(imp_dat, case = ith_set)))
    }
    cbind(res, method = ith_method, case = ith_set)
  }) %>%  bind_rows()
  
}) %>%  bind_rows()

saveRDS(res_all, "./data/imputation_methods/categorical_imputation/res_check.RDS")


######################################

library(ggplot2)

res_all <- readRDS("./data/imputation_methods/categorical_imputation/res_check.RDS") %>% 
  filter(!(method %in% c("mice_cart50", "mice_cart100", "superimputer", 
                       "supersuperimputer", "engression", "missmda_em")))

res_all %>% 
  # filter(!is.na(check)) %>% 
  mutate(case = ifelse(case == "base_dat2", "Numeric", case),
         case = ifelse(case == "base_dat3", "Factor", case),
         case = ifelse(case == "base_dat4", "One-hot", case),
         case = ifelse(case == "base_dat5", "Factor-Character", case)) %>% 
  mutate(check = ifelse(case != "One-hot", "levels", check)) %>% 
  pivot_wider(names_from = check, values_from = res) %>% 
  pivot_wider(names_from = case, values_from = levels, names_repair = "unique") %>% 
  dplyr::select(-`One-hot`) %>% 
  mutate(`One-hot` = binary_levels & binary_sum) %>% 
  dplyr::select(-binary_levels, -binary_sum) %>% 
  gather("case", "res", -method) %>% 
  filter(!is.na(res)) %>% 
  tidyr::complete(method, case) %>% 
  group_by(method) %>% 
  mutate(impute_cat = sum(res, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(res = ifelse(is.na(res), "didn't work", res)) %>% 
  ggplot(aes(x = reorder(method, -impute_cat), y = case, fill = res)) +
  geom_tile(color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual("Successful", values = c("FALSE" = "white", "TRUE" = "forestgreen", "didn't work" = "gray70")) +
  xlab("method")



res_all %>% 
  # filter(!is.na(check)) %>% 
  mutate(case = ifelse(case == "base_dat2", "Numeric", case),
         case = ifelse(case == "base_dat3", "Factor", case),
         case = ifelse(case == "base_dat4", "One-hot", case)) %>% 
  mutate(check = ifelse(case != "One-hot", "levels", check)) %>% 
  pivot_wider(names_from = check, values_from = res) %>% 
  pivot_wider(names_from = case, values_from = levels, names_repair = "unique") %>% 
  dplyr::select(-`One-hot`, -base_dat5 ) %>% 
  mutate(`One-hot` = binary_levels & binary_sum) %>% 
  dplyr::select(-binary_levels, -binary_sum) %>% 
  gather("case", "res", -method) %>% 
  filter(!is.na(res)) %>% 
  tidyr::complete(method, case) %>% 
  filter(case != "One-hot", res) %>% 
  group_by(method) %>% 
  mutate(n = n()) %>% 
  filter(case == "Numeric" & n == 1 | case == "Factor") %>% 
  rename(var_type = "case") %>% 
  rename(imputation_fun = "method") %>% 
  dplyr::select(imputation_fun, var_type) %>%
  saveRDS("./data/categorical_funs.RDS")

  
  








