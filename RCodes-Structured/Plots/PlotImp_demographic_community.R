library(data.table)
library(glmnet)
library(ggplot2)

library(tidyverse)
library(caret)
library(datarium)
library(glmnet)

data(QuickStartExample)
x <- QuickStartExample$x
y <- QuickStartExample$y

folds.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/factored_folds_input_data.csv")
head(folds.input.data)

folds.input.data<- read_csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/factored_folds_input_data.csv")
head(folds.input.data)

folds.input.data <- folds.input.data[,-c(92,130)]

unique.values.percol.coaching <- list()
for(i in 1:ncol(folds.input.data)){
  print(names(folds.input.data)[i])
  col.values <- folds.input.data[i]
  unique.col.values <- unique(col.values)
  print(unique.col.values)
  unique.values.percol.coaching[names(folds.input.data)[i]] <- unique.col.values
}

demographic.or.community.factors <- nces.cwis.dt[,c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,28,33)] 
fold.vec <- sample(rep(1:10,l=nrow(demographic.or.community.factors)))

folds.demographic.or.community.factors <- data.table::data.table(demographic.or.community.factors,fold=factor(fold.vec))

folds.demographic.or.community.factors <- as.data.frame(folds.demographic.or.community.factors)
for(k in 1:ncol(folds.demographic.or.community.factors)){
  print(class(folds.demographic.or.community.factors[,k]))
  folds.demographic.or.community.factors[,k] <- as.numeric(folds.demographic.or.community.factors[,k])
 print(class(folds.demographic.or.community.factors[,k]))
}


weight.dt.list <- list()
for(i in 1:10){
  set.seed(i)
  print(i)
  test.fold = i
  # fold.i<- folds.demographic.or.community.factors[folds.demographic.or.community.factors$fold == i,]
  # trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  # testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- folds.demographic.or.community.factors[folds.demographic.or.community.factors$fold != test.fold,] #69637 obs. of  129 variables:
  datatest <- folds.demographic.or.community.factors[folds.demographic.or.community.factors$fold == test.fold,]
  # datatrain <- fold.i[trainind,]
  # datatest <- fold.i[testind,]
  y_obs =  datatest[,24]
  x_train <- datatrain[,-c(24,25)]
  x_test <- as.matrix(datatest[,-c(24,25)])
  x = as.matrix(x_train)
  y = datatrain[,24]
  set.seed(1011)
  cvob1 = cv.glmnet(x, y)
  weight.dt.list[[i]] <- data.table(
    i,
    row.name=rownames(coef(cvob1))[-1],
    weight=coef(cvob1)[-1])
  print("------")
}
weight.dt <- do.call(rbind, weight.dt.list)

ggplot()+
  geom_point(aes(
    weight, row.name),
    data=weight.dt)

weight.dt[, n.nonzero := sum(weight != 0), by=row.name]


ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weight, row.name),
    data=weight.dt)



ordforres <- ordfor(depvar="FactorOrdinalETL", data=datatrain, nsets=1000, ntreeperdiv=100,
                    ntreefinal=5000, perffunction = "probability")
ordforres

sort(ordforres$varimp, decreasing=TRUE)