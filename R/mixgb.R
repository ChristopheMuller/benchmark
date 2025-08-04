
impute_mixgb <- function(missdf, ...){

  mixgb.data <- mixgb::mixgb(data = missdf, m = 1)
  mixgb.data <- mixgb.data[[1]]
  
  return(as.data.frame(mixgb.data))
  
}

