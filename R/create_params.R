
library(dplyr)
library(tools)

create_params <- function(path_to_complete_datasets,
                          path_to_incomplete_datasets,
                          path_to_categorical_datasets,
                          path_to_incomplete_categorical_datasets,
                          path_to_amputed,
                          path_to_imputed,
                          amputation_mechanisms,
                          amputation_reps,
                          missing_ratios,
                          imputation_methods,
                          imputation_categorical) {


  imputation_categorical <- imputation_categorical %>% filter(imputation_fun %in% imputation_methods$imputation_fun)
                          
  # completed
  c_datasets <- list.files(path_to_complete_datasets, full.names = TRUE)
  
  # incompleted
  inc_datasets <- list.files(path_to_incomplete_datasets, full.names = TRUE)
  
  # categorical
  cat_datasets <- list.files(path_to_categorical_datasets, full.names = TRUE)
  
  # incompleted categorical
  inc_cat_datasets <- list.files(path_to_incomplete_categorical_datasets, 
                                 full.names = TRUE)
  
  # create grids of amputation parameters
  params_complete <- expand.grid(mechanism = amputation_mechanisms, 
                                 ratio = missing_ratios,
                                 rep = 1:amputation_reps,
                                 filepath_original = c_datasets, 
                                 case = "complete",
                                 stringsAsFactors = FALSE)
  params_incomplete <- expand.grid(mechanism = NA, 
                                   ratio = NA,
                                   rep = NA,
                                   filepath_original = inc_datasets, 
                                   case = "incomplete",
                                   stringsAsFactors = FALSE)
  params_categorical <- expand.grid(mechanism = amputation_mechanisms, 
                                    ratio = missing_ratios,
                                    rep = 1:amputation_reps,
                                    filepath_original = cat_datasets, 
                                    case = "categorical",
                                    stringsAsFactors = FALSE)
  
  params_categorical_inc <- expand.grid(mechanism = NA, 
                                        ratio = NA,
                                        rep = NA,
                                        filepath_original = inc_cat_datasets, 
                                        case = "incomplete_categorical",
                                        stringsAsFactors = FALSE)
  
  rbind(params_complete, params_incomplete, params_categorical, params_categorical_inc) %>% 
    mutate(set_id = str_remove(basename(filepath_original), ".RDS")) %>% 
    mutate(amputed_id = paste0(mechanism,".", ratio ,".", rep, ".", set_id)) %>% 
    mutate(filepath_amputed = paste0(path_to_amputed, amputed_id, ".RDS")) %>% 
    cross_join(imputation_methods) %>% 
    mutate(imputed_id = paste0(method, ".", amputed_id)) %>% 
    mutate(filepath_imputed = paste0(path_to_imputed, imputed_id, ".RDS")) %>% 
    select(set_id, mechanism, case, method, imputation_fun, amputed_id, imputed_id, everything()) %>% 
    filter(case == "categorical" & imputation_fun %in% pull(imputation_categorical, imputation_fun) |
             case == "incomplete_categorical" & imputation_fun %in% pull(imputation_categorical, imputation_fun) |
             !(case %in% c("categorical", "incomplete_categorical"))) %>% 
    left_join(imputation_categorical) 
}





