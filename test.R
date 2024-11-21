###
# TEST OF ALL R METHODS
###


### GENERATE DATA WITH NAs

n <- 100
d <- 10
prc_missing <- 0.15

set.seed(123)

X.complete <- matrix(rnorm(n*d), n, d)

X.missing <- X.complete
X.missing[sample(1:(n*d), n*d*prc_missing)] <- NA
X.missing <- as.data.frame(X.missing)

idx_not_all_missing <- apply(X.missing, 1, function(x) any(!is.na(x)))
X.missing <- X.missing[idx_not_all_missing,]
X.complete <- X.complete[idx_not_all_missing,]


n_methods <- 25
imputed_datasets <- list()
imputed_datasets$complete <- X.complete


### Load all imputomics methods

devtools::install_github("BioGenies/imputomics")
library(imputomics)

df <- imputomics::impute_mean(missdf = X.missing)
imputed_datasets$mean <- df

df <- imputomics::impute_median(missdf = X.missing)
imputed_datasets$median <- df

df <- imputomics::impute_min(missdf = X.missing)
imputed_datasets$min <- df

df <- imputomics::impute_halfmin(missdf = X.missing)
imputed_datasets$halfmin <- df

df <- imputomics::impute_random(missdf = X.missing)
imputed_datasets$random <- df

df <- imputomics::impute_zero(missdf = X.missing)
imputed_datasets$zero <- df

df <- imputomics::impute_mice_cart(missdf = X.missing)
imputed_datasets$mice_cart <- df

df <- imputomics::impute_mice_pmm(missdf = X.missing)
imputed_datasets$mice_pmm <- df

df <- imputomics::impute_mice_mixed(missdf = X.missing)
imputed_datasets$mice_mixed <- df

df <- imputomics::impute_mice_rf(missdf = X.missing)
imputed_datasets$mice_rf <- df

df <- imputomics::impute_missforest(missdf = X.missing)
imputed_datasets$missforest <- df

df <- imputomics::impute_metabimpute_rf(missdf = X.missing)
imputed_datasets$metabimpute_rf <- df

df <- imputomics::impute_missmda_em(missdf = X.missing)
imputed_datasets$missmda_em <- df

df <- imputomics::impute_amelia(missdf = X.missing)
imputed_datasets$amelia <- df

df <- imputomics::impute_areg(missdf = X.missing)
imputed_datasets$areg <- df

df <- imputomics::impute_tknn(missdf = X.missing)
imputed_datasets$tknn <- df

df <- imputomics::impute_corknn(missdf = X.missing)
imputed_datasets$corknn <- df

df <- imputomics::impute_knn(missdf = X.missing)
imputed_datasets$knn <- df

df <- imputomics::impute_bpca(missdf = X.missing)
imputed_datasets$bpca <- df

df <- imputomics::impute_metabimpute_bpca(missdf = X.missing)
imputed_datasets$metabimpute_bpca <- df

df <- imputomics::impute_cm(missdf = X.missing)
imputed_datasets$cm <- df

df <- imputomics::impute_softimpute(missdf = X.missing)
imputed_datasets$softimpute <- df

df <- imputomics::impute_bayesmetab(missdf = X.missing)
imputed_datasets$bayesmetab <- df

# temp <- X.missing + min(X.missing) + 1
# df <- imputomics::impute_mnmf(missdf = temp)
# imputed_datasets$mnmf <- df - min(X.missing) - 1

df <- imputomics::impute_nipals(missdf = X.missing)
imputed_datasets$nipals <- df

df <- imputomics::impute_pemm(missdf = X.missing)
imputed_datasets$pemm <- df

df <- imputomics::impute_ppca(missdf = X.missing)
imputed_datasets$ppca <- df

df <- imputomics::impute_regimpute(missdf = X.missing)
imputed_datasets$regimpute <- df

df <- imputomics::impute_svd(missdf = X.missing)
imputed_datasets$svd <- df

df <- imputomics::impute_bcv_svd(missdf = X.missing)
imputed_datasets$bcv_svd <- df


#### Load all DIMAR methods

devtools::install_github("kreutz-lab/DIMAR") ### => Did not work, fixed it on local fork below
# devtools::install_local("~/INRIA/R_scripts/benchmark/DIMAR-main")
# remove.packages("DIMAR")
library(DIMAR)

mtx <- as.matrix(X.missing)

df <- DIMAR::dimar(mtx = mtx, methods=NULL)  # First run: does not work, second time works (need one run to load packages?)
imputed_datasets$dimar <- df[["Imputation"]]

mtx <- as.matrix(X.missing)
df <- DIMAR::dimar(mtx = mtx, methods="fast")
imputed_datasets$dimar_fast <- df[["Imputation"]]

mtx <- as.matrix(X.missing)
df <- DIMAR::dimar(mtx = mtx, methods=c("impSeqRob")) # Run it twice as well?
imputed_datasets$dimar_impSeqRob <- df[["Imputation"]]

mtx <- as.matrix(X.missing)
df <- DIMAR::dimar(mtx = mtx, methods=c("impSeq"))
imputed_datasets$dimar_impSeq <- df[["Imputation"]]

mtx <- as.matrix(X.missing)
df <- DIMAR::dimar(mtx = mtx, methods=c("MinProb"))
imputed_datasets$dimar_MinProb <- df[["Imputation"]]

mtx <- as.matrix(X.missing)
df <- DIMAR::dimar(mtx = mtx, methods=c("norm"))
imputed_datasets$dimar_norm <- df[["Imputation"]]

mtx <- as.matrix(X.missing)
df <- DIMAR::dimar(mtx = mtx, methods=c("SVTImpute"))
imputed_datasets$dimar_SVTImpute <- df[["Imputation"]]

mtx <- as.matrix(X.missing)
df <- DIMAR::dimar(mtx = mtx, methods=c("SVDImpute")) # Run it twice?
imputed_datasets$dimar_SVDImpute <- df[["Imputation"]]

mtx <- as.matrix(X.missing)
df <- DIMAR::dimar(mtx = mtx, methods=c("irmi"))
imputed_datasets$dimar_irmi <- df[["Imputation"]]

mtx <- as.matrix(X.missing)
df <- DIMAR::dimar(mtx = mtx, methods=c("regression"))
imputed_datasets$dimar_regression <- df[["Imputation"]]

mtx <- as.matrix(X.missing)
df <- DIMAR::dimar(mtx = mtx, methods=c("nlpca"))
imputed_datasets$dimar_nlpca <- df[["Imputation"]]


#### Load other R methods

source("miceDRF.R")
library(mice)

temp <- mice(X.missing, m = 1, method = "DRF", printflag=FALSE)
imputed_datasets$mice_drf <- mice::complete(temp,1)

temp <- mice(X.missing, m = 1, method = "norm.predict", printflag=FALSE)
imputed_datasets$mice_norm_predict <- mice::complete(temp,1)

temp <- mice(X.missing, m = 1, method = "norm.nob", printflag=FALSE)
imputed_datasets$mice_norm_nob <- mice::complete(temp,1)



library("FHDI")
temp <- FHDI::FHDI_Driver(X.missing, s_op_imputation = "FEFI")
temp <- temp[["simp.data"]]
imputed_datasets$fhdi_fefi <- temp

temp <- FHDI::FHDI_Driver(X.missing, s_op_imputation = "FHDI")
temp <- temp[["simp.data"]]
imputed_datasets$fhdi_fhdi <- temp


library("ImputeRobust")
library("mice")
temp <- mice(X.missing, m=1, method="gamlss") # VERY SLOW
temp <- mice::complete(temp)
imputed_datasets$mice_gamlss <- temp

library("VIM")
temp <- VIM::impPCA(X.missing, method="classical", m=1)
imputed_datasets$vim_pca_classical <- temp

temp <- VIM::impPCA(X.missing, method="mcd", m=1)
imputed_datasets$vim_pca_robust <- temp

library("pcaMethods")
temp <- pcaMethods::llsImpute(X.missing, k=5, completeObs = TRUE)
temp <- pcaMethods::completeObs(temp)
imputed_datasets$pca_lls <- temp


source("rmiMAE.R")
temp <- rmiMAE(as.matrix(X.missing))
imputed_datasets$rmiMAE <- temp$x


################
# Load from Python
################

library(reticulate)
use_python("C:\\Users\\Chris\\anaconda3\\envs\\hyperimpute\\python.exe", required = TRUE)
source_python("hyperimpute_script.py")


















################
# Compute RMSE
################

(list_methods <- names(imputed_datasets))

rmse <- function(X.complete, X.imputed){
  sqrt(mean((X.complete - X.imputed)^2, na.rm = TRUE))
}

rmse_values <- c()

for (i in 1:length(list_methods)){
  rmse_values <- c(rmse_values, rmse(X.complete, as.matrix(imputed_datasets[[i]])))
}

## Plot RMSE values

barplot(rmse_values, names.arg = list_methods, las = 2, col = "lightblue", main = "RMSE values", ylab = "RMSE", xlab = "Methods")



###########
# Compute Energy
###########

library(energy)

energy <- function(X_observed, X_imputed) {
  eqdist.e(rbind(X_observed, X_imputed), c(nrow(X_observed), nrow(X_imputed)))
}

energy_values <- c()

for (i in 1:length(list_methods)){
  energy_values <- c(energy_values, energy(X.complete, as.matrix(imputed_datasets[[i]])))
}

## Plot Energy values

barplot(energy_values, names.arg = list_methods, las = 2, col = "lightblue", main = "Energy values", ylab = "Energy", xlab = "Methods")
n <- length(energy_values)
barplot(log(energy_values[2:n]), names.arg = list_methods[2:n], las = 2, col = "lightblue", main = "Energy values", ylab = "Log Energy", xlab = "Methods")





