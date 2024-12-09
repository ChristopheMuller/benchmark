

impute_impSeq <- function(missdf) 
  rrcovNA::impSeq(missdf)

mpute_impSeqRob <- function(missdf, ...) 
  rrcovNA::impSeqRob(missdf, ...)[["x"]]