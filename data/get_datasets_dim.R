

path_to_complete_datasets <- "./data/datasets/complete_backup/only_num/"


files <- list.files(path_to_complete_datasets, full.names = TRUE)


dimensions <- lapply(files, function(ith_file) {
  dim_vec <- dim(readRDS(ith_file))
  data.frame(set_id = tools::file_path_sans_ext(basename(ith_file)), n_row = dim_vec[1], n_col = dim_vec[2])
}) %>% 
  bind_rows()


saveRDS(dimensions, "./data/datasets/sets_dim.RDS")


dimensions %>% 
  filter(set_id != "oes10") %>% # Had Missing Data in it!
  filter(set_id != "pyrimidines") %>%  # weird error for collinearity
  filter(set_id != "solder") %>%   # always error
  filter(set_id != "Ozone") %>%  # always error (in score)
  filter(set_id != "colic") %>%  # always error 
  filter(set_id != "tao") %>%  # exact same as oceanbuoys
  filter(set_id != "meatspec") %>%  # collinearity
  ggplot() +
  geom_label(aes(x = n_col, y = n_row, label = set_id))



