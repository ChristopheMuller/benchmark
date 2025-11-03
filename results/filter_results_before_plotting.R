library(dplyr)
library(tidyr)
library(googlesheets4)


get_raw_imp_summary <- function() {
  
  url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?gid=448271747#gid=448271747"
  
  methods <- read_sheet(url, sheet = "Cleaned Methods - ALL") %>% 
    filter(benchmark) %>% 
    select(Method, imputation_function) %>% 
    rename("elegant_name" = "Method",
           "imputation_fun" = "imputation_function")
  
  imputation_summary <- readRDS("./results/imputation_summary_M13.RDS") %>% 
    merge(methods) %>% 
    mutate(method = elegant_name)
  
  imputation_summary_categorical <- readRDS("./results/imputation_summary_complete_categorical.RDS") %>%
    merge(methods) %>%
    mutate(method = elegant_name)
  
  # remove old categorical results and replace with new one
  imputation_summary <- imputation_summary %>%
    filter(case != "categorical") %>%
    rbind(imputation_summary_categorical)
  
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
  
  imputation_summary
}




### Case 1 : NUM + COMPLETE

imputation_summary <- get_raw_imp_summary()

imputation_summary <- imputation_summary %>% 
  filter(case == "complete") %>% 
  filter(!(method %in% c("mice_default", "gbmImpute", 
                         "missmda_mifamd_reg", "missmda_mifamd_em",
                         "SVTImpute"))) 

error_summary <- readRDS("./results/comp_errors_num.RDS") %>% 
  filter(!is.na(measure), !is.na(score)) %>% 
  merge(methods) %>% 
  mutate(method = elegant_name)


imputation_summary <- rbind(imputation_summary, error_summary) %>%  
  group_by(method, set_id, mechanism, ratio, rep, measure) %>% 
  filter(if (n() > 1) !is.na(score) else TRUE) %>% 
  filter(measure != "IScore")

imputation_summary_numerical <- imputation_summary

### Case 2 : NUM + INCOMPLETE
imputation_summary <- get_raw_imp_summary()

imputation_summary <- imputation_summary %>% 
  filter(case == "incomplete") %>% 
  filter(!(method %in% c("gbmImpute", "missmda_mifamd_reg", "missmda_mifamd_em",
                         "rmiMAE", "SVTImpute")))

### Case 3 : Mixed + COMPLETE
imputation_summary <- get_raw_imp_summary()

imputation_summary <- imputation_summary %>% 
  filter(case == "categorical") %>% 
  filter(!(method %in% c("missmda_famd_em", "missmda_famd_reg")))

imputation_summary_categorical <- imputation_summary

### Case 4 : Mixed + INCOMPLETE
imputation_summary <- get_raw_imp_summary()

imputation_summary <- imputation_summary %>% 
  filter(case == "incomplete_categorical") %>% 
  filter(!(method %in% c("missmda_famd_em", "missmda_famd_reg")))



### Case 5 : ALL + Complete
imputation_summary <- get_raw_imp_summary()

methods_cat <- unique((imputation_summary %>% filter(case == "categorical"))$method)

imputation_summary <- imputation_summary %>%
  filter(case %in% c("complete", "categorical")) %>% 
  filter(method %in% methods_cat) %>% 
  filter(!(method %in% c("gbmImpute", "missmda_mifamd_reg", "missmda_mifamd_em",
                         "SVTImpute", "missmda_famd_em", "missmda_famd_reg"))) %>% 
  filter(measure == "energy_std")

error_summary <- readRDS("./results/comp_errors_num.RDS") %>% 
  filter(!is.na(measure), !is.na(score)) %>% 
  merge(methods) %>% 
  mutate(method = elegant_name) %>% 
  filter(method %in% methods_cat)

imputation_summary <- rbind(imputation_summary, error_summary) %>%  
  group_by(method, set_id, mechanism, ratio, rep, measure) %>% 
  filter(if (n() > 1) !is.na(score) else TRUE) %>% 
  filter(measure != "IScore")


### Case 6 : ALL + Incomplete
imputation_summary <- get_raw_imp_summary()


numerical_incomplete <- readRDS("./results/imputation_summary_incomplete_categorical.RDS") %>% 
  merge(methods) %>% 
  mutate(method = elegant_name)

imputation_summary_incomplete <- rbind(numerical_incomplete,
                                       readRDS("./results/imputation_summary_incomplete_numerical.RDS"))


methods_cat <- unique((imputation_summary %>% filter(case == "categorical"))$method)

imputation_summary_incomplete <- imputation_summary_incomplete %>% 
  filter(case %in% c("incomplete", "incomplete_categorical")) %>% 
  filter(method %in% methods_cat) %>% 
  mutate(measure = ifelse(measure == "IScore_cat", "IScore", measure)) %>% 
  filter(measure == "IScore")

imputation_summary <- rbind(imputation_summary_incomplete) %>% 
  mutate(set_id = ifelse(set_id == "colic_again", "colic", set_id)) %>% 
  filter(case %in%  c("incomplete", "incomplete_categorical")) 

imputation_summary %>%  filter(set_id %in% c("colic", "colic_again"))

# adds missing results

imputation_summary_raw <- get_raw_imp_summary() %>% 
  filter(!(set_id %in% c("colic", "tbc", "kanga")))

imputation_summary_mixgb <- imputation_summary_raw %>%  
  rbind(imputation_summary) %>% 
  filter(case %in% c("incomplete", "incomplete_categorical"),
         method == "mixgb", measure %in% c("IScore", "IScore_cat"), 
         !(set_id == "walking" & is.na(score))) %>% 
  unique()

imputation_summary_mice_caliber <- imputation_summary_raw %>%  
  rbind(imputation_summary) %>% 
  filter(case %in% c("incomplete", "incomplete_categorical"),
         method == "mice_caliber", measure %in% c("IScore", "IScore_cat")) %>% 
  unique() 

imputation_summary_mice_pmm  <- imputation_summary %>%  
  filter(method == "mice_pmm", case == "incomplete") %>% 
  mutate(method = "mice_default", elegant_name = "mice_default")


imputation_summary <- imputation_summary %>% 
  rbind(imputation_summary_mixgb, 
        imputation_summary_mice_caliber, 
        imputation_summary_mice_pmm) %>% 
  filter(!(method %in% c(
    "missmda_famd_em", "missmda_famd_reg", # not working for categorical
    "gbmImpute", 
    "missmda_mifamd_reg", "missmda_mifamd_em", # not working for numerical
    "rmiMAE", "SVTImpute"))) %>%  
  unique() %>% 
  mutate(measure = ifelse(measure %in% c("IScore", "IScore_cat"), "IScore", measure)) %>% 
  complete(method, set_id) %>%  unique() %>% 
  mutate(error = ifelse(is.na(score) & is.na(error), "computational", error),
         measure = "IScore")

imputation_summary_incomplete <- imputation_summary


### Case 5 : ALL

#copy mice_default results

imputation_summary_mice_pmm  <- imputation_summary_numerical %>%  
  filter(method == "mice_pmm") %>% 
  mutate(method = "mice_default", elegant_name = "mice_default")


imputation_summary <- rbind(imputation_summary_incomplete, 
                            imputation_summary_numerical, 
                            imputation_summary_categorical,
                            imputation_summary_mice_pmm) %>% 
  mutate(case = ifelse(is.na(case), "incomplete", case)) %>% 
  unique()


