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

# 
# combined.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/coaching_cwis_aggregrate_districts.csv")
# head(combined.input.data)
# 
# # input.data <- input.data[,-c(1,2,15,16,36,40,42,99)]
# # # colnames(input.data)[27] <- "ETLAverage"
# fold.vec <- sample(rep(1:10,l=nrow(combined.input.data)))
# fold.vec <- sample(rep(1:10,l=nrow(combined.input.data)))
# combined.input.data <- data.table::data.table(combined.input.data,fold=factor(fold.vec))
# 
# combined.input.data$OrdinalETL <- ""
# for(i in 1:nrow(combined.input.data)) {
#   row <- combined.input.data[i,]
#   etlScore <- row$ETL.AVERAGE
#   print(etlScore)
#   print(i)
#   if (etlScore <= 1) {
#     combined.input.data[i,]$OrdinalETL <- "Very negative"
#     print("Very negative")
#   } else if (etlScore > 1 && etlScore <= 2) {
#     combined.input.data[i,]$OrdinalETL <- "Negative"
#     print("Negative")
#   } else if ( etlScore > 2 && etlScore <= 3) {
#     combined.input.data[i,]$OrdinalETL <- "Neutral"
#     print("Neutral")
#   } else if(etlScore > 3 && etlScore <= 4){
#     combined.input.data[i,]$OrdinalETL <- "Very positive"
#     print("Very positive")
#   }else{
#     combined.input.data[i,]$OrdinalETL <- "Positive"
#     print("Positive")
#   }
# 
# }
# levels <- c("Very negative","Negative","Neutral","Positive","Very positive")
# write.csv(combined.input.data,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/factored_combined_input_data.csv", row.names = FALSE)


folds.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/factored_combined_input_data.csv")
head(folds.input.data)
# 
# folds.input.data <- folds.input.data[,-c(92)]

head(folds.input.data)

folds.input.data <- as.data.frame(folds.input.data)

for(j in 3:101){
  folds.input.data[,j][is.na(folds.input.data[,j])] <- 0
}
for(k in 3:101){
  folds.input.data[,k] <- as.numeric(folds.input.data[,k])
}

folds.input.data <- as.data.table(folds.input.data)
# folds.input.data<- lapply(folds.input.data,as.numeric)
levels <- c("Very negative","Negative","Neutral","Positive","Very positive")
folds.input.data$OrdinalETL <- factor(folds.input.data$OrdinalETL,levels)

folds.input.data <- folds.input.data[,-c(1,2)]

head(folds.input.data)


final.accuracy.list <- list()
accuracy.dt <- list()
predictions.list <- list()
split = 10
for(i in 1:split){
  set.seed(i)
  print(i)
  test.fold = i
  datatrain <-folds.input.data[fold != test.fold] #69637 obs. of  129 variables:
  datatest <- folds.input.data[fold == test.fold]
  ord_y_train <- datatrain$OrdinalETL
  
  y_train <- as.matrix(datatrain[,17])
  
  ord_y_test <- datatest[,100]
  y_test <- datatest[,17]
  
  train <- as.matrix(datatrain[,-c(17,99,100)])
  test <-  as.matrix(datatest[,-c(17,99,100)])
  
  ordfor_train <- datatrain[,-c(17,99)]
  ordfor_test <- datatest[,-c(17,99,100)]
  
  ordnet <- ordinalNetTune(train,ord_y_train)
  bestLambdaIndex <- which.max(rowMeans(ordnet$loglik))
  cv.fit.gaussian <- cv.glmnet(train,y_train)
  
  ordforres <- ordfor(depvar="OrdinalETL", data=ordfor_train, nsets=1000, ntreeperdiv=100,
                      ntreefinal=5000, perffunction = "probability")
  
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
    x = error.values,y=model)) + ggtitle("L2ordinalNetVsglmnetVsbaslinesVsordinalForest") 


final.accuracy.list <- do.call(rbind, accuracy.dt) 
final.accuracy.list
error.values = final.accuracy.list$accuracies.rmse.error.percent#final.accuracy.list$accuracies.error.percent
model=final.accuracy.list$accuracies.algo.name
ggplot()+
  geom_point(aes(
    x = error.values,y=model)) + ggtitle("L1ordinalNetVsglmnetVsbaslinesVsordinalForest") 




