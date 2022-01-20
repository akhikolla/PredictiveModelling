library(batchtools)
library(WeightedROC)
library(glmnet)
library(earth)
library(animint2)

library(tidyverse)
library(caret)
library("datarium")
library(ggplot2)

spp.csv.vec <- normalizePath(Sys.glob("data/*"))
all.y.list <- list()
all.X.list <- list()
species.name.vec <- c(
  "318"="Sugar Maple",
  "123"="Table Mountain Pine")
names(species.name.vec) <- paste0(
  "spp_env_", names(species.name.vec), ".csv")

for(spp.csv in spp.csv.vec){
  print(spp.csv)
  spp <- fread(spp.csv)
  print(spp)
  print(basename(spp.csv))
  species <- species.name.vec[[basename(spp.csv)]]
  all.y.list[[species]] <- spp$PRES
  all.X.list[[species]] <- as.matrix(spp[, 6:36])
}

X.sc <- scale(all.X.list[[1]])
X.center <- attr(X.sc, "scaled:center")
X.scale <- attr(X.sc, "scaled:scale")
X.my <- t((t(all.X.list[[1]])-X.center)/X.scale)
all.equal(as.numeric(X.sc), as.numeric(X.my))

registry <- function(reg.dir, subset.fun){
  list(reg.dir=reg.dir, subset.fun=subset.fun)
}
reg.list <- list(
  registry("registry-expired", function(dt){
    dt[algorithm != "earth"]
  }),
  registry("registry-earth-prop-zeros", identity)
)

glmnet.dt.list <- list()


for(test.set.i in 1:nrow(xgboost1)){
  test.set.info <- xgboost1[test.set.i]
  print(test.set.info)
  on.vec <- c("test.fold", "species")
  test.set.meta <- test.set.info[, ..on.vec]
  test.set.algos <- done[test.set.meta, on=on.vec]
  is.train <- algo.result.list[[test.set.info$list.index]]$is.train
  test.y.vec <- all.y.list[[species]][!is.train]
  test.X.mat <- all.X.list[[species]][!is.train,]
  for(algo.i in 1:nrow(test.set.algos)){
    result.i <- test.set.algos$list.index[[algo.i]]
    algo.result <- algo.result.list[[result.i]]
    algo.name <- test.set.algos$algorithm[[algo.i]]
    algo.meta <- data.table(
      test.set.meta,
      algorithm=algo.name,
      weight.name=test.set.algos$weight.name[[algo.i]])
    if(algo.name=="earth"){
      earth.dt.list[[paste(test.set.i, algo.i)]] <-
        with(algo.result, data.table(
          algo.meta,
          prop.zero,
          feature=names(prop.zero)))
    }
    if(algo.name=="glmnet"){
      weight.vec <- coef(algo.result$fit)[-1,]
      glmnet.dt.list[[paste(test.set.i, algo.i)]] <- data.table(
        algo.meta,
        feature=names(weight.vec),
        weight=weight.vec,
        norm.weight=weight.vec*X.scale)
    }
    pred.prob.vec <- if(algo.name%in%c("glmnet")){
      predict(algo.result$fit, test.X.mat, type="response")
    }else{
      algo.result$pred.prob.vec
    }
