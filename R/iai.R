

impute_opt_tree <- function(missdf, ...) {
  lnr <- iai::imputation_learner(method = "opt_tree")
  iai::fit(lnr, missdf)
  iai::transform(lnr, missdf)
}

impute_opt_knn <- function(missdf, ...) {
  lnr <- iai::imputation_learner(method = "opt_knn")
  iai::fit(lnr, missdf)
  iai::transform(lnr, missdf)
}

impute_opt_svm <- function(missdf, ...) {
  lnr <- iai::imputation_learner(method = "opt_svm")
  iai::fit(lnr, missdf)
  iai::transform(lnr, missdf)
}

impute_single_knn <- function(missdf, ...) {
  lnr <- iai::imputation_learner(method = "knn")
  iai::fit(lnr, missdf)
  iai::transform(lnr, missdf)
}


