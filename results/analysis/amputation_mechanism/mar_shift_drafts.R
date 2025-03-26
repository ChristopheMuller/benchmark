
set.seed(123)

n <- 1000

x1 <- rnorm(n)
x2 <- x1 + rnorm(n)

dat <- data.frame(x1, x2)


shift_sizes <- c(0.1, 1) 

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

