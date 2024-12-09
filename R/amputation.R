
library(dplyr)

# general amputation function
ampute_dataset <- function(filepath, mechanism, ratio) {
  
  dat <- readRDS(filepath)
  
  if(!is.na(mechanism)) {
    dat <- try({
      get(mechanism)(dat, ratio)
    })
    
    if(inherits(dat, "try-error"))
      dat <- NA
  }
  
  dat  
}


mar <- function(dat, ratio, ...) {
  produce_NA(data = dat, mechanism = "MAR", perc.missing = ratio, ...)$data.incomp
}


mcar <- function(dat, ratio, ...) {
  produce_NA(data = dat, mechanism = "MCAR", perc.missing = ratio, ...)$data.incomp
}



# amputation summary

summarize_amputation <- function(amputed_all, params) {
  # was the amputation successful? What would we like to know about amputed datasets?
  # did all the mechanisms work?
  
  amputation_ids <- names(amputed_all)
  
  amputation_res <- lapply(amputation_ids, function(i) {
    ith_amputed <- amputed_all[[i]]
    
    data.frame(amputed_id = str_remove(i, "amputed_dat_"),
               amputed_ratio = sum(is.na(ith_amputed))/prod(dim(ith_amputed)))
  }) %>% 
    bind_rows() 
  
  params %>% 
    select(set_id, amputed_id, case, mechanism, rep, ratio) %>% 
    unique() %>% 
    left_join(amputation_res, by = "amputed_id") %>% 
    select(-amputed_id) %>% 
    mutate(ratio = ratio,
           diff = round(abs(ratio - amputed_ratio), 2))
}


