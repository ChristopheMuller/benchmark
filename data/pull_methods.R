

library(googlesheets4)
library(dplyr)

# url <- ...

imp_table <- read_sheet(url)

imp_table %>% 
  filter(IMPLEMENTED) %>% 
  pull(`Function name`) %>% 
  saveRDS("data/functions.RDS")


