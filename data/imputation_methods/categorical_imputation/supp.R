

check_levels <- function(imp_dat, lev = 1:3, case) {
  
  if(case == "base_dat5")
    lev <- paste0("category", 1:3)
  
  if(all(unique(unlist(imp_dat[["category"]])) %in% lev))
    return(TRUE)
  
  FALSE
}


check_binary_sum <- function(imp_dat) {
  
  if(all(imp_dat[["category1"]] + imp_dat[["category2"]] + imp_dat[["category3"]] == 1))
    return(TRUE)
  
  FALSE
}


check_binary_levels <- function(imp_dat) {
  
  var1 <- all(unique(unlist(imp_dat[["category1"]])) %in% c(0, 1))
  var2 <- all(unique(unlist(imp_dat[["category2"]])) %in% c(0, 1))
  var3 <- all(unique(unlist(imp_dat[["category3"]])) %in% c(0, 1))
  
  if(all(var1, var2, var3))
    return(TRUE)
  
  FALSE
}


