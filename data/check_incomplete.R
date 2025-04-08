

sapply(list.files("./data/datasets/incomplete_backup/", full.names = T), function(ith) {
  dat <- readRDS(ith)
  print(ith)
  any(colSums(is.na(dat)) > 10) & any(colSums(!is.na(dat)) > 10)
})
