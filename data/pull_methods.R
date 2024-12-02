

library(googlesheets4)
library(dplyr)

url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"

imp_table <- read_sheet(url)

imp_table %>% 
  filter(IMPLEMENTED) %>% 
  pull(`Function name`) %>% 
  saveRDS("data/functions.RDS")


