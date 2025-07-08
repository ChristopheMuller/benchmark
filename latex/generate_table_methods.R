library(googlesheets4)
library(dplyr)
library(xtable)

url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"


########################
# INCLUDED
########################

methods <- read_sheet(url, sheet = "Cleaned Methods - ALL") %>%
  filter(benchmark == 1) %>%
  mutate(
    Citation_imp = ifelse(is.na(Citation_imp), "", Citation_imp),
    isgit = (Repo == "Git"),
    
    Implementation = case_when(
      !is.na(Repo) ~ paste0(Repo, ":", Implementation),
      TRUE ~ Implementation
    ),
    
    Implementation = case_when(
      isgit ~ paste0(
        "\\href{https://github.com/",
        sub("^Git:", "", Implementation),
        "}{Git:",
        sub("^Git:", "", Implementation),
        "}"
      ),
      TRUE ~ Implementation
    ),
    
    Methods = paste0(
      "\\textbf{", Method, "}",
      ifelse(is.na(Citation) | Citation == "", "", paste0(" \\citep{", Citation, "}"))
    ),
    Languages = paste0("\\texttt{", Language, "}"),
    
    wrapper_superscript = paste0(
      ifelse(!is.na(Wrapper_imputomics) & Wrapper_imputomics == 1, "$^{\\ast}$", ""),
      ifelse(!is.na(Wrapper_hyperimpute) & Wrapper_hyperimpute == 1, "$^{\\dagger}$", "")
    ),
    
    Implementations = paste0(
      "\\texttt{", Implementation, "}",
      ifelse(Citation_imp != "", paste0(" \\citep{", Citation_imp, "}"), ""),
      wrapper_superscript
    ),
    
    Methods = gsub("_", "-", Methods)
  ) %>%
  select(Methods, Languages, Implementations)

table_caption <- paste0(
  "List of imputation methods included in our work. ",
  "$^{\\ast}$Implemented via imputomics wrapper. ",
  "$^{\\dagger}$Implemented via hyperimpute wrapper. "
)

xtable::xtable(
  methods,
  label = "tab:methods",
  caption = table_caption
) %>%
  print(
    file = "latex/methods.tex",
    sanitize.text.function = identity,
    tabular.environment = 'longtable',
    floating = FALSE,
    size = "scriptsize"
  )


#############################
# Not included
#############################


methods <- read_sheet(url, sheet = "Cleaned Methods - ALL") %>% 
  select(benchmark, Citation, Method, Reason) %>%
  filter(benchmark == 0) %>% 
  mutate(
    
    Methods = paste0("\\textbf{", Method, "}"),
    Citations = ifelse(is.na(Citation) | Citation == "", "\\xmark", 
                       paste0("\\cite{", Citation, "}")),
    
    Methods = gsub("_", "-", Methods)
  ) %>%
  arrange(Methods) %>%
  select(Methods, Citations, Reason)

table_caption <- paste0(
  "Some caption"
)

xtable::xtable(
  methods,
  label = "tab:methods_not_in",
  caption = table_caption
) %>%
  print(
    file = "latex/methods_not_in.tex",
    sanitize.text.function = identity,
    tabular.environment = 'longtable', 
    floating = FALSE,
    size = "tiny"
  )


