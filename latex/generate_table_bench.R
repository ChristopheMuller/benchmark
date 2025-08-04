
library(googlesheets4)
library(dplyr)
library(xtable)

url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"


benchmarks <- read_sheet(url, sheet = "litrev") %>% 
  mutate(
    Real_Point = real_pointwise,
    Real_Distr = real_distributional,
    Real_DT = real_downstream,
    Artificial_Point = artificial_pointwise,
    Artificial_Distr = artificial_distributional,
    Artificial_DT = artificial_downstream,
    Citation = ifelse(citation == "This Work", citation, paste0("\\citet{", citation, "}"))
  ) %>% 
  select(
    Citation,
    Real_Point,
    Real_Distr,
    Real_DT,
    Artificial_Point,
    Artificial_Distr,
    Artificial_DT
  )%>%
  mutate(across(-Citation, ~ ifelse(.x, "\\checkmark", "--"))) %>% 
  select(-Real_Point)

# Print xtable LaTeX
xt <- xtable(benchmarks,
             align = c("l", "l|", rep("c", 2), "|", rep("c", 3)),
             caption = "Overview of benchmark studies and evaluation criteria.",
             label = "tab:benchmark_overview")

print(xt, include.rownames = FALSE,
      sanitize.text.function = identity,
      add.to.row = list(
        pos = list(0, nrow(benchmarks)),
        command = c(
          "\\toprule\n\\textbf{Benchmark} & \\multicolumn{2}{c|}{\\textbf{Real Missingness}} & \\multicolumn{2}{c}{\\textbf{Artificial Missingness}} \\\\\n & Distributional & Downstream & Pointwise & Distributional & Downstream \\\\\n\\midrule\n",
          "\\bottomrule\n"
        )
      ),
      
      include.colnames = FALSE,
      file = "latex/benchmarks.tex",
      size = "tiny")
