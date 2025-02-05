
library(dplyr)

# general amputation function
ampute_dataset <- function(filepath, mechanism, ratio) {
  
  dat <- readRDS(filepath)
  missdf <- dat
  
  amputed_switch <- FALSE
  
  if(!is.na(mechanism)) {
    
    i <- 1
    
    # safe amputation here
    while(i < 4 && !amputed_switch){
      
      i <- i + 1
      
      missdf <- try({
        get(mechanism)(dat, ratio)
      })
      
      if(!(inherits(missdf, "try-error") || any(rowSums(!is.na(missdf)) == 0))) {
        amputed_switch <- TRUE
      }
    }
  } 
  
  if(!amputed_switch)
    stop("amputation error")
  
  missdf  
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
    dplyr::select(set_id, amputed_id, case, mechanism, rep, ratio) %>% 
    unique() %>% 
    left_join(amputation_res, by = "amputed_id") %>% 
    dplyr::select(-amputed_id) %>% 
    mutate(ratio = ratio,
           diff = round(abs(ratio - amputed_ratio), 2))
}


