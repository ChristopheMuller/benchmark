##########################
### Code snippets for preprocessing the datasets
##########################


rename_columns <- function(data, prefix="X"){
  colnames(data) <- paste0(prefix, 1:ncol(data))
  return(data)
}

###################
## NUMERICAL DATA
###################

### Airoil_self_noise
# Only num

airfoil_self_noise <- readRDS("./data/datasets/complete_backup/unprocessed/airfoil_self_noise.RDS")
airfoil_self_noise <- rename_columns(airfoil_self_noise)

# saveRDS(airfoil_self_noise, "data/datasets/complete_backup/only_num/airfoil_self_noise.RDS")

### Allergens
# Only num

allergens <- read.csv("data/datasets/complete_backup/unprocessed/ACC_2023_Chip1.csv")
allergens <- allergens[, which(colnames(allergens) == "Act.d.1"):ncol(allergens)]
allergens <- rename_columns(allergens)

dim(allergens)
#> 2351 112
sum(is.na(allergens))
#> 7
allergens[is.na(allergens)] <- 9  # replaced all 9 by NA by accident

# saveRDS(allergens, "data/datasets/complete_backup/only_num/allergens.RDS")


### Concrete
# Only num

concrete <- readRDS("./data/datasets/complete_backup/unprocessed/concrete.RDS")
concrete <- rename_columns(concrete)

# saveRDS(concrete, "data/datasets/complete_backup/only_num/concrete.RDS")

### Enb
# Only num
enb <- readRDS("./data/datasets/complete_backup/unprocessed/enb.RDS")
enb <- rename_columns(enb)

# saveRDS(enb, "data/datasets/complete_backup/only_num/enb.RDS")


### Oes10
# Only num

oes <- readRDS("./data/datasets/complete_backup/unprocessed/oes10.RDS")
oes <- rename_columns(oes)

# saveRDS(oes, "data/datasets/complete_backup/only_num/oes10.RDS")


### Scm1d
# Only num

scm1d <- readRDS("./data/datasets/complete_backup/unprocessed/scm1d.RDS")
scm1d <- rename_columns(scm1d)

# saveRDS(scm1d, "data/datasets/complete_backup/only_num/scm1d.RDS")


### Scm20d
# Only num

scm20d <- readRDS("./data/datasets/complete_backup/unprocessed/scm20d.RDS")
scm20d <- rename_columns(scm20d)

# saveRDS(scm20d, "data/datasets/complete_backup/only_num/scm20d.RDS")


### Slump
# Only num

slump <- readRDS("./data/datasets/complete_backup/unprocessed/slump.RDS")
slump <- rename_columns(slump)

# saveRDS(slump, "data/datasets/complete_backup/only_num/slump.RDS")


### Yeast
# Mixed

yeast <- readRDS("./data/datasets/complete_backup/unprocessed/yeast.RDS")
yeast <- yeast[, -1] # remove ID column
yeast <- rename_columns(yeast)

# => Keep num only
num_feat <- 1:8
yeast_only_num <- yeast[, num_feat]

# saveRDS(yeast_only_num, "data/datasets/complete_backup/only_num/yeast.RDS")






###################
## CATEGORICAL DATA
###################

### German
# Mixed

german <- readRDS("./data/datasets/complete_backup/unprocessed/german.RDS")
german <- rename_columns(german)

# 1. Keep num only
num_feat <- c(2,5)
german_only_num <- german[, num_feat]

# saveRDS(german_only_num, "data/datasets/complete_backup/only_num/german.RDS")

# 2. Cat as factor
german_cat <- german
german_cat[, -c(2, 5)] <- as.data.frame(lapply(german_cat[, -c(2, 5)], function(x) factor(as.integer(factor(x)))))

# saveRDS(german_cat, "data/datasets/complete_backup/categorical_as_factor/german.RDS")


### Yeast
# Mixed

yeast <- readRDS("./data/datasets/complete_backup/unprocessed/yeast.RDS")
yeast <- yeast[, -1] # remove ID column
yeast <- rename_columns(yeast)

# => Cat as factor
yeast_cat <- yeast
yeast_cat$X9[yeast_cat$X9 == "ERL"] <- "ERLPOX"
yeast_cat$X9[yeast_cat$X9 == "POX"] <- "ERLPOX"
yeast_cat$X9 <- factor(as.integer(factor(yeast_cat$X9)))

# saveRDS(yeast_cat, "data/datasets/complete_backup/categorical_as_factor/yeast.RDS")


### Hayes_roth
# Only cat

hayes_roth <- readRDS("./data/datasets/complete_backup/unprocessed/hayes_roth.RDS")
hayes_roth <- as.data.frame(hayes_roth)
hayes_roth <- hayes_roth[, -1] # remove ID column
hayes_roth <- rename_columns(hayes_roth)

hayes_roth <- as.data.frame(lapply(hayes_roth, as.factor))

# saveRDS(hayes_roth, "data/datasets/complete_backup/categorical_as_factor/hayes_roth.RDS")



