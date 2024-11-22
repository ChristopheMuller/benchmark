
library(dplyr)

# general amputation function
ampute_dataset <- function(filepath, mechanism) {
  
  dat <- readRDS(filepath)
  
  if(!is.na(mechanism)) {
    dat <- get(mechanism)(dat)
  }
  
  dat  
}


# here we could define mechanisms
# some dummy MCAR examples below

mechanism1 <- function(dat, ...) {
  
  missing_cols <- sample(1:ncol(dat), 3, replace = FALSE)
  dat <- as.matrix(dat)
  dat[, missing_cols][runif(nrow(dat) * 3) < 0.5] <- NA
  data.frame(dat, check.names = FALSE)
  
}

mechanism2 <- function(dat, ...) {
  missing_cols <- sample(1:ncol(dat), 4, replace = FALSE)
  dat <- as.matrix(dat)
  dat[, missing_cols][runif(nrow(dat) * 4) < 0.3] <- NA
  data.frame(dat, check.names = FALSE)
}

mechanism3 <- function(dat, ...) {
  missing_cols <- sample(1:ncol(dat), 5, replace = FALSE)
  dat <- as.matrix(dat)
  dat[, missing_cols][runif(nrow(dat) * 5) < 0.7] <- NA
  data.frame(dat, check.names = FALSE)
}


# amputation summary

summarize_amputation <- function(amputed_all) {
  # was the amputation successful? What would we like to know about amputed datasets?
  # did all the mechanisms work?
  
  
  return("summary here!")
}


