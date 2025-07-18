

library(miceDRF)

n <- 100

X1 <- c(rnorm(n/2, -25, 1), rnorm(n/2, 25, 1))

X2 <- rep(0, n)
for (i in 1:n){
  
  if (X1[i] < 0){
    a <- rnorm(1,-5,1)
    b <- rnorm(1,5,1)
    unif <- runif(1)
    if (unif < 0.5){
      X2[i] <- a
    } else {
      X2[i] <- b
    }
  } else {
    a <- rnorm(1,-15,1)
    b <- rnorm(1,15,1)
    unif <- runif(1)
    if (unif < 0.5){
      X2[i] <- b
    } else {
      X2[i] <- a
    }
  }
}


plot(X2, X1)

original_data <- data.frame(X1, X2)

# where X1 < 0 and X2 > 0
idx_of_interest <- which(original_data$X1 < 0 & original_data$X2 > 4.8 & original_data$X2 < 5.2)
id_chosen <- sample(idx_of_interest, 1)
chosen_X1 <- original_data$X1[id_chosen]

points(original_data$X2[id_chosen], original_data$X1[id_chosen], col = "red", pch = 19)
text(original_data$X2[id_chosen], original_data$X1[id_chosen]-2.5, "True point", pos = 3, col="red")


#1. Impute by [-100,5], almost true value (centered)

data_imp_1 <- original_data
data_imp_1[id_chosen,] <- c(chosen_X1, 5)
score_1 <- miceDRF::energy_dist(original_data, data_imp_1)
points(data_imp_1$X2[id_chosen], data_imp_1$X1[id_chosen], col = "green", pch = 19)
text(data_imp_1$X2[id_chosen], data_imp_1$X1[id_chosen], round(score_1, 2), pos = 3, col="green")

#2. Impute by [-100,-5], as likely as true value (centered)

data_imp_2 <- original_data
data_imp_2[id_chosen,] <- c(chosen_X1, -5)
score_2 <- miceDRF::energy_dist(original_data, data_imp_2)
points(data_imp_2$X2[id_chosen], data_imp_2$X1[id_chosen], col = "blue", pch = 19)
text(data_imp_2$X2[id_chosen], data_imp_2$X1[id_chosen], round(score_2, 2), pos = 3, col="blue")

#3. Impute by [-100,15], unlikely, but as far as true value (centered)

data_imp_3 <- original_data
data_imp_3[id_chosen,] <- c(chosen_X1, 15)
points(data_imp_3$X2[id_chosen], data_imp_3$X1[id_chosen], col = "purple", pch = 19)
score_3 <- miceDRF::energy_dist(original_data, data_imp_3)
text(data_imp_3$X2[id_chosen], data_imp_3$X1[id_chosen], round(score_3, 2), pos = 3, col="purple")

#4. Impute by [100,5], as likely as true value (centered)

data_imp_4 <- original_data
data_imp_4[id_chosen,] <- c(chosen_X1, -15)
points(data_imp_4$X2[id_chosen], data_imp_4$X1[id_chosen], col = "orange", pch = 19)
score_4 <- miceDRF::energy_dist(original_data, data_imp_4)
text(data_imp_4$X2[id_chosen], data_imp_4$X1[id_chosen], round(score_4, 2), pos = 3, col="orange")


