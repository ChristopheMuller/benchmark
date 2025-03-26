

impute_missmda_famd_em <- function (missdf, ...) {
  imputed <- missMDA::imputeFAMD(X = missdf, method = "EM",
                                 ncp = min(2, ncol(missdf) - 1))
  data.frame(imputed[["completeObs"]])
}

impute_missmda_famd_reg <- function (missdf, ...) {
  imputed <- missMDA::imputeFAMD(X = missdf, method = "Regularized",
                                 ncp = min(2, ncol(missdf) - 1))
  data.frame(imputed[["completeObs"]])
}

impute_missmda_pca_em <- function (missdf, ...) {
  imputed <- missMDA::imputePCA(X = missdf, method = "EM",
                                ncp = min(2, ncol(missdf) - 1))
  data.frame(imputed[["completeObs"]])
}

impute_missmda_pca_reg <- function (missdf, ...) {
  imputed <- missMDA::imputePCA(X = missdf, method = "Regularized",
                                ncp = min(2, ncol(missdf) - 1))
  data.frame(imputed[["completeObs"]])
}

impute_missmda_MIFAMD_em <- function (missdf, ...) {
  imputed <- missMDA::MIFAMD(X = missdf, method = "EM", nboot = 1,
                             ncp = min(2, ncol(missdf) - 1))
  data.frame(imputed[[1]][[1]])
}

impute_missmda_MIFAMD_reg <- function (missdf, ...) {
  imputed <- missMDA::MIFAMD(X = missdf, method = "Regularized", nboot = 1,
                             ncp = min(2, ncol(missdf) - 1))
  data.frame(imputed[[1]][[1]])
}

impute_missmda_MIPCA_em <- function (missdf, ...) {
  imputed <- missMDA::MIPCA(X = missdf, method = "EM", nboot = 1, 
                            ncp = min(2, ncol(missdf) - 1))
  data.frame(imputed[[1]][[1]])
}

impute_missmda_MIPCA_reg <- function (missdf, ...) {
  imputed <- missMDA::MIPCA(X = missdf, method = "Regularized", nboot = 1,
                            ncp = min(2, ncol(missdf) - 1))
  data.frame(imputed[[1]][[1]])
}






