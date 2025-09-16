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
  filter(!(method %in% c("min", "cm", "halfmin",
                         "minProb")))


problematic_sets <- c(
  "oes10", # Had Missing Data in it!
  "pyrimidines",  # weird error for collinearity
  "solder",   # always error
  "Ozone",  # always error (in score)
  "tao",  # exact same as oceanbuoys
  "meatspec",  # high correlations
  "exa"  # weird
)

small_sets <- c("cheddar", "chicago", "divusa", "eco", "exa", "fpe", "leafburn", 
                "pyrimidines", "sat", "savings", "seatpos", "slump", "star", "stat500", 
                "tvdoctor",
                
                "chredlin", "hayes_roth", "hips",
                
                "airquality", "Animals_na", "employee", "mammalsleep"
                )


imputation_summary <- imputation_summary %>% 
  filter(!(set_id %in% problematic_sets)) %>%
  filter(!(set_id %in% small_sets))

### Case 1 : NUM + COMPLETE

imputation_summary <- imputation_summary %>% 
  filter(case == "complete") %>% 
  filter(!(method %in% c("mice_default", "gbmImpute", 
                         "missmda_mifamd_reg", "missmda_mifamd_em",
                         "SVTImpute")))

### Case 2 : NUM + INCOMPLETE
imputation_summary <-imputation_summary %>% 
  filter(case == "incomplete") %>% 
  filter(!(method %in% c("mice_default", "mixgb", "gbmImpute", 
                         "missmda_mifamd_reg", "missmda_mifamd_em",
                         "rmiMAE", "SVTImpute")))

### Case 3 : Mixed + COMPLETE
imputation_summary <- imputation_summary %>% 
  filter(case == "categorical")

### Case 4 : Mixed + INCOMPLETE
imputation_summary <- imputation_summary %>% 
  filter(case == "incomplete_categorical") %>% 
  filter(!(method %in% c("missmda_famd_em", "missmda_famd_reg")))



### Case 5 : ALL + Complete

methods_cat <- unique((imputation_summary %>% filter(case == "categorical"))$method)

imputation_summary <- imputation_summary %>%
  filter(case %in% c("complete", "categorical")) %>% 
  filter(method %in% methods_cat)

### Case 6 : ALL + Incomplete

methods_cat <- unique((imputation_summary %>% filter(case == "categorical"))$method)

imputation_summary <- imputation_summary %>% 
  filter(case %in% c("incomplete", "incomplete_categorical")) %>% 
  filter(method %in% methods_cat) %>% 
  mutate(measure = ifelse(measure == "IScore_cat", "IScore", measure))


