
library(ggplot2)
library(patchwork)
library(ggcorrplot)

source("./R/amputation.R")
source("./R/amputation_mar.R")


plot_shift <- function(X, X.NA, col_id) {
  plt_dat <- rbind(data.frame(value = X[!is.na(X.NA[,col_id]), col_id],
                              missing = FALSE),
                   data.frame(value = X[is.na(X.NA[,col_id]), col_id], 
                              missing = TRUE))
  ggplot(plt_dat, aes(x = value, fill = missing)) +
    geom_density(alpha = 0.5) + 
    theme_bw() + 
    scale_fill_manual(values = c("#0D3B66", "#D0E37F")) +
    ggtitle(paste0("Variable: ", col_id))
}

set.seed(123)
p <- 3
n <- 1000

X <- matrix(rnorm(n*p), ncol = p)
X[,1] <- 2*X[,2] + 1*X[,3] + rnorm(n)

X.NA <- mar(X, 0.3)

p1 <- plot_shift(X, X.NA, 1)
p2 <- plot_shift(X, X.NA, 2)
p3 <- plot_shift(X, X.NA, 3)

p4 <- ggcorrplot(cor(X), outline.col = "white",
                 colors = c("#D0E37F", "white", "#0D3B66"))

(((p1 + p2 + p3) + plot_layout(guides = "collect")) +  p4 )


X.NA <- mar(X, 0.1)

p1 <- plot_shift(X, X.NA, 1)
p2 <- plot_shift(X, X.NA, 2)
p3 <- plot_shift(X, X.NA, 3)

p4 <- ggcorrplot(cor(X), outline.col = "white",
                 colors = c("#D0E37F", "white", "#0D3B66"))

(((p1 + p2 + p3) + plot_layout(guides = "collect")) +  p4 )


