library(data.table)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(caret)
library("ordinalNet")
library("ordinalForest")
library(tidyverse)
library(caret)
library("datarium")
library(ggplot2)


demographic.or.community.factors <- nces.cwis.dt[,c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,25,26,28,33)] 
fold.vec <- sample(rep(1:10,l=nrow(demographic.or.community.factors)))

folds.demographic.or.community.factors <- data.table::data.table(demographic.or.community.factors,fold=factor(fold.vec))

folds.demographic.or.community.factors <- as.data.frame(folds.demographic.or.community.factors)
for(k in 1:ncol(folds.demographic.or.community.factors)){
  print(class(folds.demographic.or.community.factors[,k]))
  folds.demographic.or.community.factors[,k] <- as.numeric(folds.demographic.or.community.factors[,k])
  print(class(folds.demographic.or.community.factors[,k]))
}

# folds.demographic.or.community.factors <- folds.demographic.or.community.factors[,-c(26)]

write.csv(folds.demographic.or.community.factors,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/dcf_folds_glmnetdata.csv", row.names = FALSE)

folds.demographic.or.community.factors$OrdinalETL <- ""
for(i in 1:nrow(folds.demographic.or.community.factors)) {
  row <- folds.demographic.or.community.factors[i,]
  etlScore <- row$ETL.AVERAGE
  print(etlScore)
  print(i)
  if (etlScore <= 1) {
    folds.demographic.or.community.factors[i,]$OrdinalETL <- "Very negative"
    print("Very negative")
  } else if (etlScore > 1 && etlScore <= 2) {
    folds.demographic.or.community.factors[i,]$OrdinalETL <- "Negative"
    print("Negative")
  } else if ( etlScore > 2 && etlScore <= 3) {
    folds.demographic.or.community.factors[i,]$OrdinalETL <- "Neutral"
    print("Neutral")
  } else if(etlScore > 3 && etlScore <= 4){
    folds.demographic.or.community.factors[i,]$OrdinalETL <- "Very positive"
    print("Very positive")
  }else{
    folds.demographic.or.community.factors[i,]$OrdinalETL <- "Positive"
    print("Positive")
  }
}
levels <- c("Very negative","Negative","Neutral","Positive","Very positive")
folds.demographic.or.community.factors$OrdinalETL <- factor(folds.demographic.or.community.factors$OrdinalETL,levels)
write.csv(folds.demographic.or.community.factors,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/dcf_folds_orddata.csv", row.names = FALSE)

head(folds.demographic.or.community.factors)


#ordinal vs glmnet vs baselines
final.accuracy.list <- list()
accuracy.dt <- list()
predictions.list <- list()
split = 10
for(i in 1:split){
  set.seed(i)
  print(i)
  test.fold = i
  datatrain <-folds.demographic.or.community.factors[folds.demographic.or.community.factors$fold != test.fold,] #69637 obs. of  129 variables:
  datatest <- folds.demographic.or.community.factors[folds.demographic.or.community.factors$fold == test.fold,]
  ord_y_train <- datatrain[,26]
  y_train <- datatrain[,24]
  
  ord_y_test <- datatest[,26]
  y_test <- datatest[,24]
  
  train <- as.matrix(datatrain[,-c(24,25,26)])
  test <-  as.matrix(datatest[,-c(24,25,26)])
  
  ordnet <- ordinalNet(head(train),head(ord_y_train), family="cumulative", link="logit",
                       parallelTerms=TRUE, nonparallelTerms=FALSE)
  
  cv.fit.gaussian <- cv.glmnet(train,y_train)
  
  one.pred <- function(x)rep(x, nrow(test))
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  freq <-as.data.frame(table(y_train))
  median.values <- sort(y_train)
  median.ind.val <- median(median.values)
  mean.ind.val <- mean(y_train)
  
  predictions.list <-  list(
    glmnet.gaussian=as.numeric(predict(cv.fit.gaussian,newx=test,s=cv.fit.gaussian$lambda.1se,type="response")),
    baseline.l0=one.pred(as.numeric.factor(freq[which.max(freq$Freq),]$y_train)),
    baseline.l1=one.pred(median.ind.val),
    baseline.l2=one.pred(mean.ind.val),
    ordnet.pred = predict(ordnet,newx=test, type="class"))
  
  accuracy.dt.list <- list()
  for(algo in names(predictions.list)){
    print(algo)
    pred.vec = predictions.list[[algo]]
    accuracy.dt.list[[algo]] <- data.table(
      algo.name = algo,
      error.percent= sqrt(mean((pred.vec - y_test)^2))#l2 error
    )
  }
  
  accuracy.dt[[i]] <- data.table(fold=i,accuracies = do.call(rbind, accuracy.dt.list))
}
final.accuracy.list <- do.call(rbind, accuracy.dt) 
final.accuracy.list
error.values = final.accuracy.list$accuracies.error.percent
model=final.accuracy.list$accuracies.algo.name
ggplot()+
  geom_point(aes(
    x = error.values,y=model)) + ggtitle("ordinalNetVsglmnetVsbaslines") 


#ordinalNet vs ordinalForest



final.accuracy.list <- list()
accuracy.dt <- list()
predictions.list <- list()
for(i in 1:10){
  set.seed(i)
  print(i)
  # folds.input.data.i <-  folds.input.data[folds.input.data$fold == i,]
  # folds.input.data.i$OrdinalETL <- factor(folds.input.data.i$OrdinalETL,levels)
  # folds.input.data.i <- folds.input.data.i[,-27]
  
  set.seed(i)
  print(i)
  test.fold = i
  datatrain <-folds.demographic.or.community.factors[folds.demographic.or.community.factors$fold != test.fold,] #69637 obs. of  129 variables:
  datatest <- folds.demographic.or.community.factors[folds.demographic.or.community.factors$fold == test.fold,]
  ord_y_train <- datatrain[,26]
  y_train <- datatrain[,24]
  
  ord_y_test <- datatest[,26]
  y_test <- datatest[,24]
  
  train <- as.matrix(datatrain[,-c(24,25,26)])
  test <-  as.matrix(datatest[,-c(24,25,26)])
  
  ordfor_train <-head(datatrain[,-c(24,25)])
  ordfor_test <- datatest[,-c(24,25,26)]

  
  ordforres <- ordfor(depvar="OrdinalETL", data=ordfor_train, nsets=1000, ntreeperdiv=100,
                      ntreefinal=5000, perffunction = "probability")
  
  ordnet <- ordinalNet(head(train,head)(ord_y_train), family="cumulative", link="logit",
                       parallelTerms=TRUE, nonparallelTerms=FALSE)
  # ordnetParallel <- ordinalNet(train,ord_y, family="cumulative", link="logit",
  #                      parallelTerms=TRUE, nonparallelTerms=FALSE)
  # ordnetSemiParallel <- ordinalNet(train,ord_y, family="cumulative", link="logit",
  #                      parallelTerms=TRUE, nonparallelTerms=FALSE)
  # ordnetNonParallel
  
  predictions.list <-  list( 
    ordinalNet = predict(ordnet,newx=test, type="class"),
    ordinalForest=as.integer(predict(ordforres, newdata=ordfor_test,type="class")$ypred)
  )
  
  accuracy.dt.list <- list()
  for(algo in names(predictions.list)){
    print(algo)
    pred.vec = predictions.list[[algo]]
    accuracy.dt.list[[algo]] <- data.table(
      algo.name = algo,
      error.percent= sqrt(mean((pred.vec - y_test)^2))
    )
  }
  
  accuracy.dt[[i]] <- data.table(fold=i,accuracies = do.call(rbind, accuracy.dt.list))
}
final.accuracy.list <- do.call(rbind, accuracy.dt) 
final.accuracy.list
error.values = final.accuracy.list$accuracies.error.percent
model=final.accuracy.list$accuracies.algo.name
ggplot()+
  geom_point(aes(
    x = error.values,y=model)) + ggtitle("ordinalNetVsordinalForest") 





