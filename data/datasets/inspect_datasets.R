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




#########
## Cat
#########

library("OpenML")
## temporarily set API key to read only key
setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f")


# Electricity: 44156
# eye-movement: 44157, cat_features <- c(3, 4, 17, 22, 23, 24)
# diamond: 44059, cat_features <- c(2, 3, 4)

data = getOMLDataSet(data.id = 44059)$data
dim(data)
str(data)
head(data[,])
sum(is.na(data))

cat_features <- c(2, 3, 4)

for (i in cat_features) {
  print(paste("Feature", i))
  print(table(data[,i]))
}

feature <- 10
hist(data[,feature], breaks = 20, main = colnames(data)[feature], xlab = colnames(data)[feature])
(how_many_distinct_values <- length(unique(data[,feature])))
table(data[,feature])


corr_matrix <- cor(data[,-cat_features])
colnames(corr_matrix) <- (1:ncol(data[,-cat_features]))
rownames(corr_matrix) <- (1:ncol(data[,-cat_features]))
library(corrplot)
corrplot(corr_matrix, method = "color")

# saveRDS(data, "data/datasets/complete_backup/unprocessed/diamond.RDS")


