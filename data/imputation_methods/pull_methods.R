library(googlesheets4)
library(dplyr)

url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"

imp_table <- read_sheet(url, sheet = "Kept only")

imp_table %>% 
  filter(KEPT, IMPLEMENTED) %>% 
  dplyr::select(`Function name`, MI) %>% 
  saveRDS("data/functions.RDS")


