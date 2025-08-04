
library(ggplot2)
library(dplyr)
library(patchwork)



insert_MAR <- function(dat, ratio = 0.3, shift = "right", shift_size = 0.5) {
  
  # shift_size is a number greater than 0
  
  if(shift_size == 0)
    stop("shift is wrong")
  
  shift <- match.arg(shift, c("right", "left"))
  
  min_val <- 0.1
  max_val <- 0.5
  
  min_val <- min_val + shift_size
  max_val <-  max_val + shift_size
  
  if(shift == "left") {
    min_tmp <- min_val
    min_val <- - max_val
    max_val <- - min_tmp
  }
  
  n <- nrow(dat)
  p <- ncol(dat)
  total_missing <- round(n * p * ratio, 0)
  
  if((p - 1) * n <= p * n * ratio) stop("Not enough data to ampute")
  
  if(p < 2) {
    stop(paste0("The data should contain at least two columns!",
                "Your data contains ", p, "."))
  }
  
  tmp_missing_per_column <- rmultinom(1, total_missing, rep(1/p, p - 1))[, 1]
  # random_complete_col <- sample(1:p, size = 1)
  
  random_complete_col <- 1
  
  missing_per_column <- numeric(p)
  missing_per_column[-random_complete_col] <- tmp_missing_per_column
  
  ids_0 <- which(missing_per_column == 0)
  ids_non_0 <- which(missing_per_column != 0)
  
  for(i in ids_non_0) {
    
    n_cols_to_sample <- sample(1:length(ids_0), size = 1)
    sampled_cols_ind <- sample(ids_0, n_cols_to_sample)
    
    sampled_cols <- dat[, sampled_cols_ind]
    sampled_scales <- runif(n_cols_to_sample, min = min_val, max = max_val)
    scaled_sum <- as.matrix(sampled_cols) %*% sampled_scales
    
    probs <- 1/(1 + exp(-scaled_sum))
    
    ids <- which(as.logical(sapply(probs, function(p) rbinom(1, 1, p))))
    
    missing_per_column[i] <- min(length(ids), missing_per_column[i])
    
    dat[sample(ids, missing_per_column[i]), i] <- NA
  }
  
  dat
}


set.seed(123)

n <- 1000

x1 <- rnorm(n)
x2 <- x1 + rnorm(n)

dat <- data.frame(x1, x2)


shift_sizes <- c(0.1, 0.5, 1, 2) 

plts1 <- lapply(shift_sizes, function(i) {
  amputed <- insert_MAR(dat, shift_size = i) %>% 
    mutate(x2_obs = dat[["x2"]]) %>% 
    mutate(x2_missing = is.na(x2))
  
  p1 <- amputed  %>% 
    ggplot() +
    geom_density(aes(x = x2_obs, fill = x2_missing, col = x2_missing), alpha = 0.6) +
    ggtitle(paste0("distributional shift param = ", i)) +
    xlab("x2")
  
  p2 <- amputed  %>% 
    ggplot() +
    geom_point(aes(x = x1, y = x2_obs, col = x2_missing)) +
    theme(legend.position = "none") +
    ylab("x2")
  
  p1/p2
    
})

patchwork::wrap_plots(plts1, nrow = 1) + plot_layout(guides = "collect") + plot_annotation(title  = "Shift towards right")


plts2 <- lapply(shift_sizes, function(i) {
  amputed <- insert_MAR(dat, shift_size = i, shift = "left") %>% 
    mutate(x2_obs = dat[["x2"]]) %>% 
    mutate(x2_missing = is.na(x2))
  
  p1 <- amputed  %>% 
    ggplot() +
    geom_density(aes(x = x2_obs, fill = x2_missing, col = x2_missing), alpha = 0.6) +
    ggtitle(paste0("distributional shift param = ", i)) +
    xlab("x2")
  
  p2 <- amputed  %>% 
    ggplot() +
    geom_point(aes(x = x1, y = x2_obs, col = x2_missing)) +
    theme(legend.position = "none") +
    ylab("x2")
  
  p1/p2
  
})

patchwork::wrap_plots(plts2, nrow = 1) + plot_layout(guides = "collect") + plot_annotation(title  = "Shift towards left")


  

#   
#   
# insert_MAR(dat) %>% 
#   mutate(x2_obs = dat[["x2"]]) %>% 
#   mutate(x2_missing = is.na(x2)) %>% 
#   ggplot() +
#   geom_point(aes(x = x1, y = x2_obs, col = x2_missing))
# 
# 
# 
# dat %>% 
#   ggplot() +
#   geom_point(aes(x = x1, y = x2))

  