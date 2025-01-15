### allergens:

ACC_2023_Chip1 <- read_csv("data/datasets/allergens_gross/ACC_2023_Chip1.csv")
# keep as of col of "Act d 1"
ACC_2023_Chip1 <- ACC_2023_Chip1[, which(colnames(ACC_2023_Chip1) == "Act d 1"):ncol(ACC_2023_Chip1)]
View(ACC_2023_Chip1)
dim(ACC_2023_Chip1)
# 2351 112
sum(is.na(ACC_2023_Chip1))
# fill na by 9
ACC_2023_Chip1[is.na(ACC_2023_Chip1)] <- 9
sum(ACC_2023_Chip1 > 0)

# save as RDS file
saveRDS(ACC_2023_Chip1, "data/datasets/complete_backup/allergens.RDS")
