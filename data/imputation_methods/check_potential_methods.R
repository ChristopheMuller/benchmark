
df <- readRDS("C:/gits/benchmark/data/datasets/complete/dataset1.RDS")




robCompositions::impCoda(missdf)


imp <- robCompositions::impKNNa(missdf, metric = "Aichison")
imp$xImp


MINMA::MINMA(t(missdf))


lnr <- iai::imputation_learner(method = "opt_tree")

imputed <- iai::transform(lnr, missdf)


imputed == missdf
