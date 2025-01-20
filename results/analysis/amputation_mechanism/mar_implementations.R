
set.seed(123)

n <- 100

x1 <- runif(n)
x2 <- x1 + runif(n)

dat <- data.frame(x1, x2)

get_prob <- function(ith_column) {
  rank_vec <- rank(ith_column)
  rank_vec/sum(rank_vec)
}

# let's ampute only x1 for now
probs <- get_prob(dat[["x2"]])

dat[["x1_missing"]] <- sapply(probs, function(p) rbinom(1, 1, p))

# no missings in dat

sum(dat[["x1_missing"]])


missing_ratio <- function(n) {
  mean((1:n)/sum(1:n))
}

library(ggplot2)

d <- data.frame(n = 1:200, ratio = sapply(1:200, missing_ratio))

ggplot(d, aes(x = n, y = ratio)) +
  geom_line()




