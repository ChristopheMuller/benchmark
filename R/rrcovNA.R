
impute_impSeq <- function(missdf) 
  rrcovNA::impSeq(missdf)

impute_impSeqRob <- function(missdf) {
  # workaround for rows reordering
  rownames(missdf) <- 1:nrow(missdf)
  
  # sometimes the output is x and sometimes it's xseq lol
  imputed <- as.data.frame(rrcovNA::impSeqRob(missdf)[["x", exact = FALSE]])
  
  imputed <- imputed[ order(as.numeric(row.names(imputed))), ]
  rownames(imputed) <- NULL
  imputed
}
