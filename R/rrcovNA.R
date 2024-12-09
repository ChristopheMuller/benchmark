

impute_impSeq <- function(missdf) 
  rrcovNA::impSeq(missdf)

impute_impSeqRob <- function(missdf, ...) 
  rrcovNA::impSeqRob(missdf, ...)[["x"]]