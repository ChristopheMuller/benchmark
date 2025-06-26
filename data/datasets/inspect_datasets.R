library(dplyr)
library(tibble)
library(corrplot)


#####
# Evaluate the datasets
#####


# data <- scpf
data <- OpenML::getOMLDataSet(46610)
data <- data$data

# incomp
## birth : c(1,3,4,5,6,9,15,19,20,22) are numerical
## breast:
# rem.col <- c(1,2,3,4)
# are.cat <- c(5,6,8,9,10,12,13,15)
# are.num <- c(7,11)
# num.to.be <- c(14) To be continued ..

dim(data)
str(data)
head(data[])
tail(data)
sum(is.na(data))

# distribution of values of feature x
feature <- 37
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

for (i in are.num) {
  # hist
  hist(data[,i], breaks = 20, main = colnames(data)[i], xlab = colnames(data)[i])
}

data_filled <- data[,num.features]
data_filled[is.na(data_filled)] <- 0
corr_matrix <- cor(data_filled)
colnames(corr_matrix) <- num.features
rownames(corr_matrix) <- num.features
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


