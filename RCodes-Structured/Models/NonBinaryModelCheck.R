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
library("MLmetrics")

LogLossBinary = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps) - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}
y_test <- as.numeric(y_test)
ordnet.pred 
LogLossBinary(y_test,ordnet.pred)

combined.binary.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/coaching_cwis_aggregrate_districts.csv")
head(combined.binary.input.data)
fold.vec <- sample(rep(1:10,l=nrow(combined.binary.input.data)))
combined.binary.input.data <- data.table::data.table(combined.binary.input.data,fold=factor(fold.vec))

combined.binary.input.data$BinaryETL <- ""
for(i in 1:nrow(combined.binary.input.data)) {
  row <- combined.binary.input.data[i,]
  etlScore <- row$ETL.AVERAGE
  print(etlScore)
  if (etlScore >= 0 && etlScore <= 2.8) {
    combined.binary.input.data[i,]$BinaryETL <- 1
  } else{
    combined.binary.input.data[i,]$BinaryETL <- 2
  }
}
write.csv(combined.binary.input.data,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/onetwo_factored_combined_binary.input_data.csv", row.names = FALSE)


binary.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/onetwo_factored_combined_binary.input_data.csv")
head(binary.input.data)
binary.input.data <- binary.input.data[,-c(1,2,19)]

head(binary.input.data)
head(binary.input.data$BinaryETL,100)
unique(binary.input.data$BinaryETL)

binary.input.data$BinaryETL <- as.factor(binary.input.data$BinaryETL)


binary.input.data <- as.data.frame(binary.input.data)

for(j in 1:98){
  binary.input.data[,j][is.na(binary.input.data[,j])] <- 0
}

for(k in 1:98){
  binary.input.data[,k] <- as.numeric(binary.input.data[,k])
}


binary.input.data <- as.data.table(binary.input.data)
head(binary.input.data)


final.accuracy.list <- list()
accuracy.dt <- list()
predictions.list <- list()
split = 10
for(i in 1:split){
  set.seed(i)
  print(i)
  
  ## train - test split
  test.fold = i
  datatrain <-binary.input.data[fold != test.fold]
  datatest <- binary.input.data[fold == test.fold]
  
  ##targets for models
  ord_y_train <- datatrain$BinaryETL
  ord_y_test <- datatest$BinaryETL
  
  y_train <- as.matrix(datatrain[,99])
  y_test <- as.matrix(datatest[,99])
  
  ## train,test categorized data
  ##glmnet baseline train
  train <- as.matrix(datatrain[,-c(98,99)])
  test <-  as.matrix(datatest[,-c(98,99)])
  
  #ordinalforest train and test
  ordfor_train <- datatrain[,-c(98)]
  ordfor_test <- datatest[,-c(98,99)]
  
  ordnet <- ordinalNetTune(train,ord_y_train)
  bestLambdaIndex <- which.max(rowMeans(ordnet$loglik))
  
  cv.fits.binomial <- cv.glmnet(train, y_train, family = "binomial")
  
  assess.glmnet(cv.fit.binomial.preds,newx = test, newy=as.numeric(y_test), s=0.1)
  print(assess.glmnet)
  # predict(fit2, type = "nonzero")
  
  ordforres <- ordfor(depvar="BinaryETL", data=ordfor_train, nsets=1000, ntreeperdiv=100,
                      ntreefinal=5000, perffunction = "probability")
  
  one.pred <- function(x)rep(x, nrow(test))
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  freq <-as.data.frame(table(y_train))
  median.values <- sort(y_train)
  median.ind.val <- median(median.values)
  mean.ind.val <- mean(y_train)
  
  predictions.list <-  list(
    glmnet.binomial=as.integer(predict(cv.fits.binomial,newx=test,s=cv.fits.binomial$lambda.1se,type="class")),
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
      misclassfication.error=
        log.loss.error=
        #meanabs.error.percent= mean(abs((pred.vec) - (y_test$ETL.AVERAGE))),
        #rmse.error.percent = sqrt(mean(((pred.vec) - (y_test$ETL.AVERAGE))^2))#l2 error
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
    x = error.values,y=model)) + ggtitle("L2ordinalNetVsglmnetVsbaslinesVsordinalForest") 

#010
final.accuracy.list <- do.call(rbind, accuracy.dt) 
final.accuracy.list
error.values = final.accuracy.list$accuracies.rmse.error.percent#final.accuracy.list$accuracies.error.percent
model=final.accuracy.list$accuracies.algo.name
ggplot()+
  geom_point(aes(
    x = error.values,y=model)) + ggtitle("L1ordinalNetVsglmnetVsbaslinesVsordinalForest") 


#949 rows 
multinomial_matrix <- matrix(0, nrow = 186, ncol = 5)






