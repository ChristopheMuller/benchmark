library(googlesheets4)
library(dplyr)
library(xtable)

url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"


########################
# INCLUDED
########################

methods <- read_sheet(url, sheet = "table_datasets") %>% 
  filter(n >= 200) %>% 
  mutate("Category" = ifelse((d_cat == 0) & (propNA_max == 0), "Complete, Numeric",
                             ifelse((d_cat == 0) & (propNA_max > 0), "Incomplete, Numeric",
                                    ifelse((d_cat > 0) & (propNA_max == 0), "Complete, Mixed",
                                           "Incomplete, Mixed")))) %>%
  mutate(
    "Name" = Name,
    "Rows" = as.integer(n), 
    "Num Cols" = as.integer(d_num),
    "Cat Cols" = as.integer(d_cat),
    "Mean NA (%)" = round(propNA_mean * 100, 2),
    "Max NA (%)" = round(propNA_max * 100, 2),
    "Source" = source
  ) %>% 
  select(Name, `Rows`, `Num Cols`, `Cat Cols`, 
         `Mean NA (%)`, `Max NA (%)`, Source)
  
table_caption <- paste0(
  "Some caption."
)

xtable::xtable(
  methods,
  label = "tab:datasets",
  caption = table_caption
) %>%
  print(
    file = "latex/datasets.tex",
    # sanitize.text.function = identity,
    # floating = FALSE,
    size = "small"
  )

