
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(tidyr)

url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"

dat <- read_sheet(url, sheet = "Bench_plot")


supp_dat <-  data.frame(t(dat[1:2, ])[-(2:3), ])
supp_dat[["benchmark"]] <- rownames(supp_dat)
colnames(supp_dat) <- supp_dat[1, ]
supp_dat <- supp_dat[-1, ]
supp_dat <- rename(supp_dat, benchmarks = "Methods \\ Benchmarks")


plt_dat <- dat %>% 
  select(all_of(colnames(dat)[-c(2, 3)])) %>% 
  rename(methods = "Methods \\ Benchmarks") %>% 
  filter(methods != "is objective?", 
         methods != "use distributional metrics?") %>% 
  pivot_longer(OURS:`Sun et al.`, names_to = "benchmarks", values_to = "present") %>% 
  merge(supp_dat) %>% 
  rename(distributional = "use distributional metrics?") %>% 
  mutate(distributional = as.numeric(distributional)) %>% 
  group_by(benchmarks) %>% 
  mutate(sum_benchmarks= sum(present)) %>% 
  ungroup() %>% 
  group_by(methods) %>% 
  mutate(sum_methods = sum(present))


plt_dat %>% 
  mutate(fill = as.character((present + distributional) * present)) %>% 
  ggplot() +
  geom_tile(aes(x = reorder(benchmarks, -sum_benchmarks), y = reorder(methods, -sum_methods), fill = fill), col = "black") +
  scale_fill_manual("used distributional metrics?", values = c("0" = "white", "1" = "lightsteelblue3", "2" = "palegreen4"),
                    labels = c("1" = "No", "2" = "Yes"),
                    breaks = c("1", "2")) +
  xlab("benchmarks") +
  ylab("methods") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "top") +
  coord_flip() +
  ggsave("~/INRIA/R_scripts/benchmark/results/analysis/benchmark_papers/benchmarks.pdf", width = 15, height = 6, units = "in")



