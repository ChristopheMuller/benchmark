library(googlesheets4)
library(dplyr)
library(xtable)

url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"

methods <- read_sheet(url, sheet = "table_datasets") %>%
  filter(n >= 200) %>%
  mutate(
    Category = ifelse((d_cat == 0) & (propNA_max == 0), "Complete, Numeric",
                      ifelse((d_cat == 0) & (propNA_max > 0), "Incomplete, Numeric",
                             ifelse((d_cat > 0) & (propNA_max == 0), "Complete, Mixed",
                                    "Incomplete, Mixed"))),
    Rows = as.integer(n),
    `Col. (Num./Cat.)` = paste0(as.integer(d_num), " / ", as.integer(d_cat)),
    `Mean/Max Missing.` = paste0(round(propNA_mean * 100, 2), " / ", round(propNA_max * 100, 2)),
    citation = ifelse(is.na(citation), "", paste0(" \\citet{", citation, "}"))
  ) %>%
  select(Name, Rows, `Col. (Num./Cat.)`, `Mean/Max Missing.`, Source = citation)

table_caption <- paste0(
  "Some caption."
)

xtable::xtable(
  methods,
  label = "tab:datasets",
  caption = table_caption,
) %>%
  print(
    file = "latex/datasets.tex",
    size = "small",
    include.rownames = FALSE,
    sanitize.text.function = identity
    
      )

