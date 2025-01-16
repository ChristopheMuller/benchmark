##########################
### Code snippets for preprocessing the datasets
##########################


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




#####
# Preprocess the datasets
#####

rename_columns <- function(data, prefix="X"){
  colnames(data) <- paste0(prefix, 1:ncol(data))
  return(data)
}

### Airoil_self_noise
# Only num

airfoil_self_noise <- readRDS("~/INRIA/R_scripts/benchmark/data/datasets/complete_backup/unprocessed/airfoil_self_noise.RDS")
airfoil_self_noise <- rename_columns(airfoil_self_noise)

# saveRDS(airfoil_self_noise, "data/datasets/complete_backup/only_num/airfoil_self_noise.RDS")

### Allergens
# Only num

allergens <- read.csv("data/datasets/complete_backup/unprocessed/ACC_2023_Chip1.csv")
allergens <- allergens[, which(colnames(allergens) == "Act.d.1"):ncol(allergens)]

dim(allergens)
#> 2351 112
sum(is.na(allergens))
#> 7
allergens[is.na(allergens)] <- 9  # replaced all 9 by NA by accident

# saveRDS(allergens, "data/datasets/complete_backup/only_num/allergens.RDS")


### Concrete
# Only num

concrete <- readRDS("~/INRIA/R_scripts/benchmark/data/datasets/complete_backup/unprocessed/concrete.RDS")
concrete <- rename_columns(concrete)

# saveRDS(concrete, "data/datasets/complete_backup/only_num/concrete.RDS")

### Enb
# Only num
enb <- readRDS("~/INRIA/R_scripts/benchmark/data/datasets/complete_backup/unprocessed/enb.RDS")
enb <- rename_columns(enb)

# saveRDS(enb, "data/datasets/complete_backup/only_num/enb.RDS")

### German
# Mixed

german <- readRDS("~/INRIA/R_scripts/benchmark/data/datasets/complete_backup/unprocessed/german.RDS")
german <- rename_columns(german)

# 1. Keep num only
num_feat <- c(2,5)
german_only_num <- german[, num_feat]

# saveRDS(german_only_num, "data/datasets/complete_backup/only_num/german.RDS")

# 2. One-hot encoding
german_one_hot <- german
german_one_hot[, -c(2, 5)] <- lapply(german_one_hot[, -c(2, 5)], as.factor)
german_one_hot <- model.matrix(~ . - 1, data = german_one_hot)

# saveRDS(german_one_hot, "data/datasets/complete_backup/one_hot/german.RDS")


### Hayes_roth
# Only cat

hayes_roth <- readRDS("~/INRIA/R_scripts/benchmark/data/datasets/complete_backup/unprocessed/hayes_roth.RDS")
hayes_roth <- as.data.frame(hayes_roth)
hayes_roth <- hayes_roth[, -1] # remove ID column
hayes_roth <- rename_columns(hayes_roth)

# 1. One-hot encoding

hayes_roth_one_hot <- hayes_roth
hayes_roth_one_hot <- lapply(hayes_roth_one_hot, as.factor)
hayes_roth_one_hot <- as.data.frame(hayes_roth_one_hot)

hayes_roth_one_hot <- model.matrix(~ . - 1, data = hayes_roth_one_hot)
hayes_roth_one_hot <- as.data.frame(hayes_roth_one_hot)

# saveRDS(hayes_roth_one_hot, "data/datasets/complete_backup/one_hot/hayes_roth.RDS")


### Oes10
# Only num

oes <- readRDS("~/INRIA/R_scripts/benchmark/data/datasets/complete_backup/unprocessed/oes10.RDS")
oes <- rename_columns(oes)

# saveRDS(oes, "data/datasets/complete_backup/only_num/oes10.RDS")


### Scm1d
# Only num

scm1d <- readRDS("~/INRIA/R_scripts/benchmark/data/datasets/complete_backup/unprocessed/scm1d.RDS")
scm1d <- rename_columns(scm1d)

# saveRDS(scm1d, "data/datasets/complete_backup/only_num/scm1d.RDS")


### Scm20d
# Only num

scm20d <- readRDS("~/INRIA/R_scripts/benchmark/data/datasets/complete_backup/unprocessed/scm20d.RDS")
scm20d <- rename_columns(scm20d)

# saveRDS(scm20d, "data/datasets/complete_backup/only_num/scm20d.RDS")


### Sf2
# Ordinal and cat

# TO DO #


### Slump
# Only num

slump <- readRDS("~/INRIA/R_scripts/benchmark/data/datasets/complete_backup/unprocessed/slump.RDS")
slump <- rename_columns(slump)

# saveRDS(slump, "data/datasets/complete_backup/only_num/slump.RDS")


### Yeast
# Mixed

yeast <- readRDS("~/INRIA/R_scripts/benchmark/data/datasets/complete_backup/unprocessed/yeast.RDS")
yeast <- yeast[, -1] # remove ID column
yeast <- rename_columns(yeast)

# 1. Keep num only
num_feat <- 1:8
yeast_only_num <- yeast[, num_feat]

# saveRDS(yeast_only_num, "data/datasets/complete_backup/only_num/yeast.RDS")

# 2. One-hot encoding
yeast_one_hot <- yeast
yeast_one_hot$X9[yeast_one_hot$X9 == "ERL"] <- "ERLPOX"
yeast_one_hot$X9[yeast_one_hot$X9 == "POX"] <- "ERLPOX"
yeast_one_hot$X9 <- as.factor(yeast_one_hot$X9)

yeast_one_hot <- model.matrix(~ . - 1, data = yeast_one_hot)
yeast_one_hot <- as.data.frame(yeast_one_hot)

# saveRDS(yeast_one_hot, "data/datasets/complete_backup/one_hot/yeast.RDS")

