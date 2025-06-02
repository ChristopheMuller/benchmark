

imputation_summary12 <- readRDS("~/INRIA/R_scripts/benchmark/results/imputation_summary_M12.RDS")



results_mice_pmm <- imputation_summary12 %>% 
  filter(method == "mice_pmm") %>% 
  filter(case %in% c("complete", "incomplete_categorical")) %>% 
  mutate(
    method = "mice_default",
    imputation_fun = "impute_mice_default"
  )


imputation_summary <- rbind(
  imputation_summary12,
  results_mice_pmm
)

saveRDS(imputation_summary, "~/INRIA/R_scripts/benchmark/results/imputation_summary_M13.RDS")
