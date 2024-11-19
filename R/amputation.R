
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
  dat[runif(nrow(dat)*ncol(dat)) < 0.2] <- NA
  dat
}

mechanism2 <- function(dat, ...) {
  dat[runif(nrow(dat)*ncol(dat)) < 0.3] <- NA
  dat
}

mechanism3 <- function(dat, ...) {
  dat[runif(nrow(dat)*ncol(dat)) < 0.3] <- NA
  dat
}

# amputation summary

summarize_amputation <- function(amputed_all) {
  # was the amputation successful? What would we like to know about amputed datasets?
  # did all the mechanisms work?
  
  
  return("summary here!")
}


