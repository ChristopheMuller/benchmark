
library(miceDRF)

# mice drf for numerical

missdf <- mar(yeast, 0.3)
colnames(missdf) <- paste0("X", as.character(1:ncol(missdf)))

impute_mice_drf(missdf = missdf, printFlag = TRUE, maxit = 2)



# mice drf for factor

a <- sample(1:10, 1484, TRUE)
a <- factor(a, levels = unique(a))

a[5] <- NA

yeast2 <- cbind(missdf, a)

sapply(yeast2, is.factor) # the last column is a factor

impute_mice_drf(yeast2)

# mice cart for factor

mice::mice(yeast2, method = "cart")




