

impute_robCompositions_knn <- function(missdf, ...) {
  imp <- robCompositions::impKNNa(missdf, metric = "Euclidean")
  imp$xImp
}