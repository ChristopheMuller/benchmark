library(dplyr)
library(googlesheets4)

methods <- read_sheet(url, sheet = "Cleaned Methods - ALL") %>% 
  filter(benchmark) %>% 
  select(Method, imputation_function) %>% 
  rename("elegant_name" = "Method",
         "imputation_fun" = "imputation_function")

imputation_summary <- readRDS("./results/imputation_summary_M13.RDS") %>% 
  merge(methods) %>% 
  mutate(method = elegant_name)

imputation_summary <- imputation_summary %>% 
  filter(!(method %in% c("mice_cart50", "mice_cart100", "superimputer", 
                         "supersuperimputer", "engression", "missmda_em"))) %>% 
  
  filter(set_id != "oes10") %>% # Had Missing Data in it!
  filter(set_id != "pyrimidines") %>%  # weird error for collinearity
  filter(set_id != "solder") %>%   # always error
  filter(set_id != "Ozone") %>%  # always error (in score)
  filter(set_id != "colic") %>%  # always error 
  filter(set_id != "tao") %>%  # exact same as oceanbuoys
  filter(set_id != "meatspec") %>%  # high correlations
  filter(set_id != "exa") %>%  # weird
  filter(!(method %in% c("min", "cm", "halfmin",
                         "minProb")))

small_sets <- c("star", "tvdoctor", "cheddar", "eco", "leafburn", "stat500", "savings",
                "chicago", "sat", "seatpos", "fpe", "pyrimidines", "Animals_na", 
                "employee", "mammalsleep", "chredlin")


### Case 1 : NUM + COMPLETE

imputation_summary <- imputation_summary %>% 
  filter(case == "complete") %>% 
  filter(!(method %in% c("mice_default", "gbmImpute", 
                         "missmda_MIFAMD_reg", "missmda_MIFAMD_em",
                         "SVTImpute"))) %>% 
  filter(!(set_id %in% small_sets))

### Case 2 : NUM + INCOMPLETE
imputation_summary <-imputation_summary %>% 
  filter(case == "incomplete") %>% 
  filter(!(method %in% c("mice_default", "mixgb", "gbmImpute", 
                         "missmda_MIFAMD_reg", "missmda_MIFAMD_em",
                         "rmiMAE", "SVTImpute"))) %>% 
  filter(!(set_id %in% small_sets))

### Case 3 : Mixed + COMPLETE
imputation_summary <- imputation_summary %>% 
  filter(case == "categorical") %>% 
  filter(!(set_id %in% small_sets))

### Case 4 : Mixed + INCOMPLETE
imputation_summary <- imputation_summary %>% 
  filter(case == "incomplete_categorical") %>% 
  filter(!(set_id %in% small_sets)) %>% 
  filter(!(method %in% c("missmda_famd_em", "missmda_famd_reg")))



### Case 5 : ALL + Complete

methods_cat <- unique((imputation_summary %>% filter(case == "categorical"))$method)

imputation_summary <- imputation_summary %>%
  filter(case %in% c("complete", "categorical")) %>% 
  filter(method %in% methods_cat) %>% 
  filter(!(set_id %in% small_sets))
  

### Case 6 : ALL + Incomplete

methods_cat <- unique((imputation_summary %>% filter(case == "categorical"))$method)

imputation_summary <- imputation_summary %>% 
  filter(case %in% c("incomplete", "incomplete_categorical")) %>% 
  filter(method %in% methods_cat) %>% 
  mutate(measure = ifelse(measure == "IScore_cat", "IScore", measure)) %>% 
  filter(!(set_id %in% small_sets))



