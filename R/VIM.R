
eval_vim_call <- function(missdf, method, ...) {
  
  args <- list(...)
  args <- args[setdiff(names(args), c("x", "method"))]
  args <- c(list(x = missdf, method = method), args)
  do.call(VIM::impPCA, args)
}

impute_vim_pca <- function(missdf, m = 1, ...) 
  eval_vim_call(missdf = missdf, method = "classical", m = m)

impute_vim_pca_robust <- function(missdf, m = 1, ...) 
  eval_vim_call(missdf = missdf, method = "mcd", m = m)