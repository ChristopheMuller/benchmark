
library(tools)
library(foreign)
library(readxl)
library(readr)
library(dplyr)

get_small_summary <- function(files) {
  
  res <- lapply(files, function(ith_file) {
    print(ith_file)
    
    ext <- file_ext(ith_file)
    
    if(ext == "")
      ext <- "data"
    
    dat <- switch (ext,
                   arff = foreign::read.arff(ith_file),
                   data = read.table(ith_file),
                   dat = read.table(ith_file),
                   xls = read_xls(ith_file),
                   csv = read.csv(ith_file),
                   tsv = readr::read_tsv(ith_file),
                   rds = readRDS(ith_file),
                   RDS = readRDS(ith_file)
    )
    
    if(ncol(dat) == 1 && (ext == "data"))
      dat <- read.table(ith_file, sep = ",")
    
    nonnumerical <- sum(!sapply(dat, is.numeric))
    
    data.frame(name = basename(ith_file),
               nrow = nrow(dat),
               ncol = ncol(dat),
               non_numerical = nonnumerical,
               missings = sum(is.na(dat))/prod(dim(dat)))
  }) %>% 
    bind_rows()
}


files <- list.files("./data/raw_datasets/Complete", full.names = TRUE)
res_complete <- get_small_summary(files)
res_complete %>% 
  arrange(-nrow, -ncol)


files <- list.files("./data/raw_datasets/Incomplete", full.names = TRUE)
res_incomplete <- get_small_summary(files)

res_incomplete %>% 
  arrange(-nrow, -ncol)





