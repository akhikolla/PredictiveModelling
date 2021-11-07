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
library("Metrics")

ETL.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/factored_combined_input_data.csv")
head(ETL.input.data)

ETL.input.data <- ETL.input.data[,-c(1,2,102)]

head(ETL.input.data)
head(ETL.input.data$ETL.AVERAGE,100)
unique(ETL.input.data$ETL.AVERAGE)

levels <- c(0,1,2,3,4,5)

ETL.input.data$ETL.AVERAGE.factor <- factor(as.integer(ETL.input.data$ETL.AVERAGE),levels)

ETL.input.data <- as.data.frame(ETL.input.data)

for(j in 1:99){
  ETL.input.data[,j][is.na(ETL.input.data[,j])] <- 0
}

for(k in 1:99){
  ETL.input.data[,k] <- as.numeric(ETL.input.data[,k])
}


ETL.input.data$ETL.AVERAGE <- as.integer(ETL.input.data$ETL.AVERAGE)
head(ETL.input.data$ETL.AVERAGE)

ETL.input.data <- as.data.table(ETL.input.data)

final.accuracy.list <- list()
accuracy.dt <- list()
predictions.list <- list()
split = 10
for(i in 1:split){
  set.seed(i)
  print(i)
  ## train - test split
  test.fold = i
  datatrain <-ETL.input.data[fold != test.fold]
  datatest <- ETL.input.data[fold == test.fold]
  
  ##targets for models
  
  ord_y_train <- datatrain$ETL.AVERAGE.factor
  ord_y_test <- datatest$ETL.AVERAGE.factor
  
  y_train <- as.matrix(datatrain[,17])
  y_test <- datatest[,17]
  
 ## train,test categorized data
  ##glmnet baseline train
  train <- as.matrix(datatrain[,-c(17,99,100)])
  test <-  as.matrix(datatest[,-c(17,99,100)])
  
  #ordinalforest train and test
  ordfor_train <- datatrain[,-c(17,99)]
  ordfor_test <- datatest[,-c(17,99,100)]
  
  fit <-ordinalNet(train, ord_y_train, family="cumulative", link="logit",
                   parallelTerms=TRUE, nonparallelTerms=FALSE)
  bestLambdaIndex <- which.max(mean(fit$loglik))
 # tunenet <- ordinalNetCV(train,ord_y_train,
 #    lambdaVals = NULL,
 #    folds = NULL,
 #    nFolds = 10,
 #    nFoldsCV = 10,
 #    tuneMethod = "cvLoglik")
 # summary(tunenet)
 # 
 # bestLambdaIndex.tune <- which.max(rowMeans(tunenet$loglik))
 # plot(tunenet)
 # 
 # cv <- ordinalNetCV(train,ord_y_train, family = "cumulative", link = "logit",
 #                     lambdaMinRatio = 1e-04, printProgress = FALSE)
 # 
 #  ordnet <- ordinalNetTune(train,ord_y_train)
 #  summary(ordnet)
 #  plot(ordnet)
 #  bestLambdaIndex <- which.max(rowMeans(ordnet$loglik))
  cv.fit.gaussian <- cv.glmnet(train,y_train)
  
  
  ordforres <- ordfor(depvar="ETL.AVERAGE.factor", data=ordfor_train, nsets=1000, ntreeperdiv=100,
                      ntreefinal=5000, perffunction = "probability")
  
  one.pred <- function(x)rep(x, nrow(test))
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  freq <-as.data.frame(table(y_train))
  median.values <- sort(y_train)
  median.ind.val <- median(median.values)
  mean.ind.val <- mean(y_train)
  
  predictions.list <-  list(
    glmnet.gaussian=(predict(cv.fit.gaussian,newx=test,s=cv.fit.gaussian$lambda.1se,type="response")),
    baseline.l0=(one.pred(as.numeric.factor(freq[which.max(freq$Freq),]$y_train))),
    baseline.l1=(one.pred(median.ind.val)),
    baseline.l2=(one.pred(mean.ind.val)),
    ordnet.pred = predict(ordnet$fit,newx=(test), type="class",whichLambda=bestLambdaIndex),
    ordinalForest=as.integer(predict(ordforres, newdata=(ordfor_test),type="class")$ypred))
  
  accuracy.dt.list <- list()
  for(algo in names(predictions.list)){
    print(algo)
    pred.vec = predictions.list[[algo]]
    print(class(pred.vec))
    accuracy.dt.list[[algo]] <- data.table(
      algo.name = algo,
      meanabs.error.percent= mean(abs((pred.vec) - (y_test$ETL.AVERAGE))),
      rmse.error.percent = sqrt(mean(((pred.vec) - (y_test$ETL.AVERAGE))^2))#l2 error
    )
  }
  
  accuracy.dt[[i]] <- data.table(fold=i,accuracies = do.call(rbind, accuracy.dt.list))
}
final.accuracy.list <- do.call(rbind, accuracy.dt) 
final.accuracy.list
error.values = final.accuracy.list$accuracies.meanabs.error.percent#final.accuracy.list$accuracies.error.percent
model=final.accuracy.list$accuracies.algo.name
ggplot()+
  geom_point(aes(
    x = error.values,y=model)) + ggtitle("L1ordinalNetVsglmnetVsbaslinesVsordinalForest") 


final.accuracy.list <- do.call(rbind, accuracy.dt) 
final.accuracy.list
error.values = final.accuracy.list$accuracies.rmse.error.percent#final.accuracy.list$accuracies.error.percent
model=final.accuracy.list$accuracies.algo.name
ggplot()+
  geom_point(aes(
    x = error.values,y=model)) + ggtitle("L2ordinalNetVsglmnetVsbaslinesVsordinalForest") 






