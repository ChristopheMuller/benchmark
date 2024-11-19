
set.seed(123)

dataset1 <- matrix(rnorm(1000), 100, 10)
saveRDS(dataset1, "./data/datasets/complete/dataset1.RDS")
dataset1[runif(1000) < 0.2] <- NA
saveRDS(dataset1, "./data/datasets/incomplete/incomplete_dataset1.RDS")


dataset1 <- matrix(rnorm(1000), 100, 10)
saveRDS(dataset1, "./data/datasets/complete/dataset2.RDS")
dataset1[runif(1000) < 0.2] <- NA
saveRDS(dataset1, "./data/datasets/incomplete/incomplete_dataset2.RDS")