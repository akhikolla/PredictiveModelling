library(data.table)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(caret)
library("ordinalNet")

library(tidyverse)
library(caret)
library("datarium")
library(ggplot2)

# input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/coaching_nces_aggregrate_districts.csv")
# head(input.data)
# input.data <- input.data[,-c(1,2,15,16,36,40,42,99)]
# # colnames(input.data)[27] <- "ETLAverage"
# fold.vec <- sample(rep(1:10,l=nrow(input.data)))
# fold.vec <- sample(rep(1:10,l=nrow(input.data)))
# folds.input.data <- data.table::data.table(input.data,fold=factor(fold.vec))
# 
# folds.input.data$OrdinalETL <- ""
# for(i in 1:nrow(folds.input.data)) {
#   row <- folds.input.data[i,]
#   etlScore <- row$ETL.AVERAGE
#   print(etlScore)
#   print(i)
#   if (etlScore <= 1) {
#     folds.input.data[i,]$OrdinalETL <- "Very negative"
#     print("Very negative")
#   } else if (etlScore > 1 && etlScore <= 2) {
#     folds.input.data[i,]$OrdinalETL <- "Negative"
#     print("Negative")
#   } else if ( etlScore > 2 && etlScore <= 3) {
#     folds.input.data[i,]$OrdinalETL <- "Neutral"
#     print("Neutral")
#   } else if(etlScore > 3 && etlScore <= 4){
#     folds.input.data[i,]$OrdinalETL <- "Very positive"
#     print("Very positive")
#   }else{
#     folds.input.data[i,]$OrdinalETL <- "Positive"
#     print("Positive")
#   }
#   
# }
# levels <- c("Very negative","Negative","Neutral","Positive","Very positive")
# write.csv(folds.input.data,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/factored_folds_input_data.csv", row.names = FALSE)

folds.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/factored_folds_input_data.csv")
head(folds.input.data)

folds.input.data <- folds.input.data[,-c(92)]

head(folds.input.data)

#folds.input.data <- as.data.frame(folds.input.data)
for(k in 1:128){
  folds.input.data[,k] <- as.numeric(folds.input.data[,k])
}
folds.input.data <- as.data.table(folds.input.data)
# folds.input.data<- lapply(folds.input.data,as.numeric)
levels <- c("Very negative","Negative","Neutral","Positive","Very positive")
folds.input.data$OrdinalETL <- factor(folds.input.data$OrdinalETL,levels)


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
  ord_y_train <- datatrain[,128]

  y_train <- datatrain[,39]
  
  ord_y_test <- datatest[,128]
  y_test <- datatest[,39]
  
  train <- as.matrix(datatrain[,-c(39,128,129)])
  test <-  as.matrix(datatest[,-c(39,128,129)])
  
  ordnet <- ordinalNet(head(train),ord_y_train, family="cumulative", link="logit",
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



