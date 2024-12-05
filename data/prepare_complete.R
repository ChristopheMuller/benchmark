library(tools)
library(foreign)
library(readxl)
library(readr)
library(dplyr)

# this is a supplementary script for saving data in one format

read_and_save <- function(files) {
  
  lapply(files, function(ith_file) {
    
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
    
    fixed_name <- gsub("-", "_", basename(ith_file))
    
    path <- paste0("./data/datasets/complete/", 
                   str_remove(fixed_name, paste0(".", ext)), 
                   ".RDS")
    
    saveRDS(dat, path)
  }) 
}




files <- list.files("./data/raw_datasets/Complete_chosen", full.names = TRUE)


res_complete <- read_and_save(files)


