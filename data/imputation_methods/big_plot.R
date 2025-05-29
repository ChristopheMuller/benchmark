
library(googlesheets4)
library(dplyr)
library(ggplot2)

url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"


plt_table <- read_sheet(url, sheet = "methods_plot")


library(tidyverse)
library(patchwork)

long_tbl <- plt_table %>%
  select(-comment) %>% 
  mutate_all(as.character) %>% 
  mutate(type = ifelse(type == "Conditional Expectation", 
                       "Conditional \nExpectation",
                       "Distributional \nImputation")) %>% 
  pivot_longer(
    cols = c(MI, type, parametricity, strategy, Categorical),
    names_to = "feature",
    values_to = "value"
  )

# features <- c("MI", "type", "parametricity", "strategy", "Categorical")
features <- c("type", "parametricity", "strategy", "Categorical")

plt_list <- lapply(features, function(ith_feature) {
  
  if(ith_feature == "Categorical") {
    long_tbl %>% 
      filter(feature == ith_feature) %>% 
      ggplot( aes(y  = feature, x = forcats::fct_rev(NAME), fill = value)) +
      geom_tile(color = "white") +
      theme_void() +
      theme(legend.position = "right",
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank()) +
      scale_fill_manual(ith_feature, values = c("steelblue1", "royalblue4"))
  } else {
    long_tbl %>% 
      filter(feature == ith_feature) %>% 
      ggplot( aes(y  = feature, x = forcats::fct_rev(NAME), fill = value)) +
      geom_tile(color = "white") +
      theme_void() +
      theme(legend.position = "right",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank()) +
      scale_fill_manual(ith_feature, values = c("steelblue1", "royalblue4"))
  }
  

})


wrap_plots(plt_list, ncol = 1) + plot_layout(guides = "collect")



