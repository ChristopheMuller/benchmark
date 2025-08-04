

one_hot_encoding <- function(dat) {
  data.frame(mltools::one_hot(data.table::as.data.table(dat)))
}


set.seed(10)

n <- 100
p <- 2


categorical_var <- data.frame(cat = factor(sample(1:4, 100, replace = TRUE), levels = 1:4))

num_dat <- matrix(rnorm(n * p), nrow = n, ncol = p)

colnames(num_dat) <- c("X1", "X2")

dat <- cbind(num_dat, categorical_var)

missdf <- dat
missdf$cat[runif(n) < 0.3] <- NA

imputed <- impute_mice_default(missdf)


one_hot_encoding(imputed)

one_hot_cat <- one_hot_encoding(categorical_var)
cor(one_hot_cat)



############# one dim example


categorical_var1 <- categorical_var
categorical_var1[2, 1] <- 2

miceDRF::energy_dist(categorical_var1, categorical_var)

categorical_var_oh <- one_hot_encoding(categorical_var)
categorical_var1_oh <- one_hot_encoding(categorical_var1)

miceDRF::energy_dist(categorical_var1_oh, categorical_var_oh)


#############  two dim case


# categorical_second <- ifelse(as.numeric(unlist(categorical_var)) < 3,  
#                              factor(sample(4:5, 100, replace = TRUE), levels = c(1, 2, 4, 5)), 
#                              factor(sample(1:2, 100, replace = TRUE), levels = c(1, 2, 4, 5)))

set.seed(10)

categorical_var <- data.frame(cat = factor(sample(1:4, 100, replace = TRUE), levels = 1:4))

categorical_second <- as.numeric(unlist(categorical_var))
categorical_second[as.numeric(unlist(categorical_var)) < 3] <- rep(1, 100)[as.numeric(unlist(categorical_var)) < 3]
categorical_second[as.numeric(unlist(categorical_var)) > 2] <- rep(2, 100)[as.numeric(unlist(categorical_var)) > 2]



dat1 <- cbind(categorical_var, cat2 = as.factor(categorical_second))

ggplot2::ggplot(dat1, aes(x = cat, y = cat2)) +
  geom_point()

cor(one_hot_encoding(dat1))


dat11 <- dat1
dat11[2, 1] <- 4

miceDRF::energy_dist(dat11, dat1)

categorical_var_oh <- one_hot_encoding(dat1)
categorical_var1_oh <- one_hot_encoding(dat11)

miceDRF::energy_dist(categorical_var1_oh, categorical_var_oh)

cor(categorical_var_oh)


ggplot2::ggplot(dat1, aes(x = cat, y = cat2)) +
  geom_point() +
  geom_point(aes(x = 1, y = 1), size = 3, col = "red")


res1 <- lapply(1:4, function(cat1) {
  dat11 <- dat1
  dat11[2, 1] <- cat1
  
  energy_cat <- miceDRF::energy_dist(dat11, dat1)
  
  categorical_var_oh <- one_hot_encoding(dat1)
  categorical_var1_oh <- one_hot_encoding(dat11)
  
  energy_one_hot <- miceDRF::energy_dist(categorical_var1_oh, categorical_var_oh)
  data.frame(
    point = cat1, 
    score = c("energy_cat", "energy_one_hot"),
    val = c(energy_cat, energy_one_hot))
}) %>%  bind_rows()


p0 <- ggplot2::ggplot(dat1, aes(x = cat, y = cat2)) +
  geom_point()


p1 <- ggplot2::ggplot(dat1, aes(x = cat, y = cat2)) +
  geom_point() +
  geom_point(aes(x = 1, y = 1), size = 3, col = "red") +
  geom_text(filter(res1, score == "energy_cat"), mapping = aes(x = point, y = 1.05, label = paste0("factor: ", round(val, 3))), col = "darkgreen") +
  geom_text(filter(res1, score == "energy_one_hot"), mapping = aes(x = point, y = 0.95, label = paste0("one-hot: ", round(val, 3))), col = "darkblue")

library(patchwork)

p0 + p1


#############################

set.seed(1)

X1 <- sample(c(rnorm(50, -100, 1), rnorm(50, 100, 1)))

X2 <- ifelse(X1 < 0, 
             sample(c(rnorm(50, -5, 1), rnorm(50, 5, 1))), 
             sample(c(rnorm(50, -10, 1), rnorm(50, 10, 1))))


hist(X2, breaks = 20)
hist(X1, breaks = 20)

dat <- data.frame(X1 = X1, X2 = X2)

dat1 <- rbind(dat, c(-100, -5))
dat2 <- rbind(dat, c(-100, 5))
dat3 <- rbind(dat, c(-100, -10))
dat4 <- rbind(dat, c(-100, 10))
dat5 <- rbind(dat, c(-100, 0))


ggplot(dat, aes(x = X1, y = X2)) +
  geom_point() +
  geom_point(aes(x = -100, y = -5), col = "red", size = 2) +
  geom_point(aes(x = -100, y = 5), col = "blue", size = 2) +
  geom_text(aes(x = -100 + 10, y = 5, label = round(energy_dist(dat2, dat1), 3)), col = "blue", size = 6) +
  geom_point(aes(x = -100, y = -10), col = "blue", size = 2) +
  geom_text(aes(x = -100 + 10, y = -10, label = round(energy_dist(dat3, dat1), 3)), col = "blue", size = 6) +
  geom_point(aes(x = -100, y = 10), col = "blue", size = 2) +
  geom_text(aes(x = -100 + 10, y = 10, label = round(energy_dist(dat4, dat1), 3)), col = "blue", size = 6) +
  geom_point(aes(x = -100, y = 0), col = "blue", size = 2) +
  geom_text(aes(x = -100 + 10, y = 0, label = round(energy_dist(dat5, dat1), 3)), col = "blue", size = 6)


#######################

set.seed(1)

X1 <- c(runif(1000))
X2 <- ifelse(X1 > 0.5, runif(1000, 0, 1), runif(1000, 1, 2))

dat <- data.frame(X1 = X1, X2 = X2)

ggplot(dat, aes(x = X1, y = X2)) +
  geom_point() 


X11 <- c(runif(1000))
X22 <- ifelse(X11 < 0.5, runif(1000, 0, 1), runif(1000, 1, 2))

dat12 <- data.frame(X1 = X11, X2 = X22)

ggplot(dat12, aes(x = X1, y = X2)) +
  geom_point() 


energy_dist(dat, dat12)


