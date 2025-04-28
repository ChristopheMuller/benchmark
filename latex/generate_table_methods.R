library(googlesheets4)
library(dplyr)
library(xtable)

# Define the Google Sheets URL
url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"

# Read relevant columns from the sheet
methods <- read_sheet(url, sheet = "Cleaned Methods - ALL") %>% 
  select(benchmark, Citation_imp, Citation, Method, Language, Implementation, Repo,
         Wrapper_imputomics, Wrapper_hyperimpute) %>%
  filter(benchmark == 1) %>%
  mutate(
    # Handle missing implementation citation
    Citation_imp = ifelse(is.na(Citation_imp), "", Citation_imp),
    isgit = (Repo == "Git"),
    
    # Format implementation information
    Implementation = case_when(
      !is.na(Repo) ~ paste0(Repo, ":", Implementation),
      TRUE ~ Implementation
    ),
    
    # Replace GitHub implementations with \href links
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
    
    # Build formatted fields for LaTeX
    Methods = paste0("\\textbf{", Method, "}"),
    Citations = ifelse(is.na(Citation) | Citation == "", "\\xmark", 
                       paste0("\\cite{", Citation, "}")),
    Language = paste0("\\texttt{", Language, "}"),
    
    # Create superscript marks based on Wrapper columns
    wrapper_superscript = paste0(
      ifelse(!is.na(Wrapper_imputomics) & Wrapper_imputomics == 1, "$^{\\ast}$", ""),
      ifelse(!is.na(Wrapper_hyperimpute) & Wrapper_hyperimpute == 1, "$^{\\dagger}$", "")
    ),
    
    # Add superscripts to Implementation
    Implementation = paste0(
      "\\texttt{", Implementation, "}", 
      ifelse(Citation_imp != "", paste0("\\citep{", Citation_imp, "}"), ""),
      wrapper_superscript
    ),
    
    # Replace _ by - in method names
    Methods = gsub("_", "-", Methods)
  ) %>%
  # Select final columns for LaTeX table
  select(Methods, Citations, Language, Implementation)

# Add footnote explanations to the table caption
table_caption <- paste0(
  "Some caption. ",
  "$^{\\ast}$Implemented via imputomics wrapper. ",
  "$^{\\dagger}$Implemented via hyperimpute wrapper. "
)

# Create and export the table
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
    size = "small"
  )
