#####
# Evaluate the datasets
#####


data <- scm20d

dim(data)
str(data)
head(data[,1:10])
sum(is.na(data))

# distribution of values of feature x
feature <- 10
hist(data[,feature], breaks = 20, main = colnames(data)[feature], xlab = colnames(data)[feature])
(how_many_distinct_values <- length(unique(data[,feature])))
table(data[,feature])


for (i in 1:ncol(data)) {
  print(paste("Feature", i))
  (how_many_distinct_values <- length(unique(data[,i])))
  print(how_many_distinct_values)
  if (how_many_distinct_values < 50){
    print("   ")
  }
}

corr_matrix <- cor(data)
colnames(corr_matrix) <- (1:ncol(data))
rownames(corr_matrix) <- (1:ncol(data))
library(corrplot)
corrplot(corr_matrix, method = "color")

