

impute_missforest <- function (missdf, ...) {
  imputed <- missForest::missForest(xmis = missdf, ...)
  imputed[["ximp"]]
}


impute_metabimpute_rf <- function (missdf) {
  capture.output(imputed <- MetabImpute::Impute(data = missdf, method = "RF", local = FALSE, reps = NULL))
  
  imputed
}
