
library(dplyr)
library(tidyr)


tbl <- expand.grid(error = c("computational",  "modification", 
                             "timeout", "missings", "none"))

imputation_summary %>% 
  filter(!is.na(measure)) %>% 
  select(-measure, -score) %>% 
  unique() %>% 
  group_by(method) %>% 
  mutate(n_attempts = n()) %>% 
  mutate(error = ifelse(is.na(error), "none", error)) %>% 
  group_by(error) %>% 
  reframe(n = 100* n() / nrow(.)) %>% 
  filter(error != "none") %>%  pull(n) %>%  sum()


imputation_summary %>% 
  filter(!is.na(error)) %>% 
  pull(method) %>%  unique() %>%  length()



imputation_summary %>% 
  filter(!is.na(measure)) %>% 
  select(-measure, -score) %>% 
  unique() %>% 
  group_by(method) %>% 
  mutate(n_attempts = n()) %>% 
  mutate(error = ifelse(is.na(error), "none", error)) %>% 
  filter(attempts == 2, error == "none") %>% 
  pull(method) %>%  unique()


imputation_summary %>% 
  # filter(case == "complete") %>% 
  filter(!(method %in% c("mice_default", "gbmImpute", 
                         "missmda_MIFAMD_reg", "missmda_MIFAMD_em",
                         "SVTImpute"))) %>% 
  filter(!(set_id %in% small_sets)) %>% 
  get_errors_and_plot_per_dataset()
  


##### be aware - the following code is ugly and copy pasted

get_errors_and_plot_per_dataset <- function(imputation_summary) {
  imputation_summary %>% 
    filter(!is.na(measure)) %>% 
    select(-measure, -score) %>% 
    unique() %>% 
    group_by(method) %>% 
    mutate(n_attempts = n()) %>% 
    mutate(error = ifelse(is.na(error), "none", error)) %>% 
    group_by(set_id, error) %>% 
    reframe(n = (n() / nrow(.) )* 100) %>% 
    filter(error != "none") %>% 
    group_by(set_id) %>%
    mutate(n_total = sum(n, na.rm = TRUE) * 100) %>%
    complete(error = c("computational", "modification", "timeout", "missings",
             "modification+wrong_levels", "wrong_levels"), fill = list(n_total = 0)) %>% 
    ggplot() +
    geom_col(aes(x = reorder(set_id, n_total), y = n, fill = error), width = 0.7) +
    xlab("Dataset") +
    ylab("Errors [%]") +
    scale_fill_manual(name = "Type of error", values = get_colors_errors(),
                      labels = get_labels_errors()) +
    coord_flip() +
    theme_minimal() +
    theme(axis.title.y = element_blank())
}


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

p1 <- imputation_summary %>% 
  filter(case == "complete") %>% 
  filter(!(method %in% c("mice_default", "gbmImpute", 
                         "missmda_MIFAMD_reg", "missmda_MIFAMD_em",
                         "SVTImpute"))) %>% 
  filter(!(set_id %in% small_sets)) %>% 
  get_errors_and_plot_per_dataset() +
  ggtitle("Complete and Numeric")

### Case 3 : Mixed + COMPLETE
p3 <- imputation_summary %>% 
  filter(case == "categorical") %>% 
  filter(!(set_id %in% small_sets))%>% 
  get_errors_and_plot_per_dataset() +
  ggtitle("Complete and Mixed")



### Case 2 : NUM + INCOMPLETE
p2 <-imputation_summary %>% 
  filter(case == "incomplete") %>% 
  filter(!(method %in% c("mice_default", "mixgb", "gbmImpute", 
                         "missmda_MIFAMD_reg", "missmda_MIFAMD_em",
                         "rmiMAE", "SVTImpute"))) %>% 
  filter(!(set_id %in% small_sets)) %>% 
  get_errors_and_plot_per_dataset() +
  ggtitle("Inomplete and Numeric")

### Case 4 : Mixed + INCOMPLETE
p4 <- imputation_summary %>% 
  filter(case == "incomplete_categorical") %>% 
  filter(!(set_id %in% small_sets)) %>% 
  filter(!(method %in% c("missmda_famd_em", "missmda_famd_reg")))%>% 
  get_errors_and_plot_per_dataset() +
  ggtitle("Inomplete and Mixed")


(p1 | p2 | p3 | p4 | plot_layout(guides = "collect")) & 
  theme(legend.position = "bottom", legend.direction = "horizontal") &
  guides(fill = guide_legend(nrow = 1))

