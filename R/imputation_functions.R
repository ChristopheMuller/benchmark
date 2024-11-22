
source("R/rmiMAE.R")

impute_dimar <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = NULL)
  )))
  return(temp[["Imputation"]])
}

impute_dimar_fast <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = "fast")
  )))
  return(temp[["Imputation"]])
}

impute_impSeqRob <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = "impSeqRob")
  )))
  return(temp[["Imputation"]])
}

impute_impSeq <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = "impSeq")
  )))
  return(temp[["Imputation"]])
}

impute_minProb <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = "MinProb")
  )))
  return(temp[["Imputation"]])
}

impute_norm <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = "norm")
  )))
  return(temp[["Imputation"]])
}

impute_SVTImpute <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = "SVTImpute")
  )))
  return(temp[["Imputation"]])
}

impute_SVDImpute <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = "SVDImpute")
  )))
  return(temp[["Imputation"]])
}

impute_irmi <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = "irmi")
  )))
  return(temp[["Imputation"]])
}

impute_regression <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = "regression")
  )))
  return(temp[["Imputation"]])
}

impute_nlpca <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- DIMAR::dimar(mtx = as.matrix(missdf), methods = "nlpca")
  )))
  return(temp[["Imputation"]])
}

impute_FEFI <- function(missdf, ...) {
  capture.output(
    temp <- FHDI::FHDI_Driver(missdf, s_op_imputation = "FEFI")
  )
  temp <- temp[["simp.data"]]
  return(temp)
}

impute_FHDI <- function(missdf, ...) {
  capture.output(
    temp <-FHDI::FHDI_Driver(missdf, s_op_imputation = "FHDI")
  )
  temp <- temp[["simp.data"]]
  return(temp)
}

impute_mice_gamlss <- function(missdf, ...) {
  capture.output(suppressMessages(suppressWarnings(
    temp <- mice::mice(missdf, method = "gamlss", m = 1)
  )))
  temp <- mice::complete(temp)
  return(temp)
}

impute_vim_pca <- function(missdf, ...) {
  capture.output(suppressMessages(
    temp <- VIM::impPCA(missdf, method="classical", m=1)
  ))
  return(temp)
}

impute_vim_pca_robust <- function(missdf, ...) {
  capture.output(suppressMessages(
    temp <- VIM::impPCA(missdf, method="mcd", m=1)
  ))
  return(temp)
}

impute_llsImpute <- function(missdf, ...) {
  k <- min(ncol(missdf)-1, 10)
  temp <- pcaMethods::llsImpute(missdf, k=k, completeObs=TRUE)
  temp <- pcaMethods::completeObs(temp)
  return(temp)
}


impute_mice_norm <- function(missdf, ...) {
  temp <- mice::mice(missdf, m=1, method="norm", printFlag=FALSE)
  temp <- mice::complete(temp)
  return(temp)
}

impute_mice_norm_predict <- function(missdf, ...) {
  temp <- mice::mice(missdf, m=1, method="norm.predict", printFlag=FALSE)
  temp <- mice::complete(temp)
  return(temp)
}

impute_mice_norm_nob <- function(missdf, ...) {
  temp <- mice::mice(missdf, m=1, method="norm.nob", printFlag=FALSE)
  temp <- mice::complete(temp)
  return(temp)
}

impute_rmiMAE <- function(missdf, ...) {
  temp <- rmiMAE(as.matrix(missdf))$x
  return(temp)
}

