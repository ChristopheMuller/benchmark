

path_to_complete_datasets <- "./data/datasets/complete/"


files <- list.files(path_to_complete_datasets, full.names = TRUE)


dimensions <- lapply(files, function(ith_file) {
  dim_vec <- dim(readRDS(ith_file))
  data.frame(set_id = tools::file_path_sans_ext(basename(ith_file)), n_row = dim_vec[1], n_col = dim_vec[2])
}) %>% 
  bind_rows()


saveRDS(dimensions, "./data/datasets/sets_dim.RDS")






