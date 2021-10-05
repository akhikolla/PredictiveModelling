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

input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/data/ReducedCoaching+CWISUpdated+NCES- binary.csv")
head(input.data)

colnames(input.data)[27] <- "ETLAverage"
fold.vec <- sample(rep(1:40,l=nrow(input.data)))
fold.vec <- sample(rep(1:40,l=nrow(input.data)))
folds.input.data <- data.table::data.table(input.data,fold=factor(fold.vec))

folds.input.data$OrdinalETL <- ""
for(i in 1:nrow(folds.input.data)) {
  row <- folds.input.data[i,]
  etlScore <- row$ETLAverage
  # print(etlScore)
  if (etlScore <= 1) {
    folds.input.data[i,]$OrdinalETL <- "Very negative"
    # print("Very negative")
  } else if (etlScore > 1 && etlScore <= 2) {
    folds.input.data[i,]$OrdinalETL <- "Negative"
    # print("Negative")
  } else if ( etlScore > 2 && etlScore <= 3) {
    folds.input.data[i,]$OrdinalETL <- "Neutral"
    # print("Neutral")
  } else if(etlScore > 3 && etlScore <= 4){
    folds.input.data[i,]$OrdinalETL <- "Very positive"
    # print("Very positive")
  }else{
    folds.input.data[i,]$OrdinalETL <- "Positive"
    # print("Positive")
  }
  
}
folds.input.data.updatedTypes<- data.frame(Collaborativeteams = as.numeric(folds.input.data$`Collaborative teams`),
                                           Commonformativeassessment = as.integer(folds.input.data$`Common formative assessment`),
                                           Databaseddecisionmaking = as.numeric(folds.input.data$`Data-based decision making`),
                                           CWIS = as.numeric(folds.input.data$CWIS),
                                           common_practices_can_statements =as.numeric(folds.input.data$common_practices_can_statements),
                                           common_practices_student_work = as.numeric(folds.input.data$common_practices_student_work),
                                           common_practices_self_assessment= as.numeric(folds.input.data$common_practices_self_assessment),
                                           common_practices_receive_feedback = as.numeric(folds.input.data$common_practices_receive_feedback),
                                           common_practices_student_feedback = as.numeric(folds.input.data$common_practices_student_feedback),
                                           common_practices_state_criteria = as.numeric(folds.input.data$common_practices_state_criteria),
                                           ETLAverage = folds.input.data$ETLAverage,
                                           OrdinalAverage = folds.input.data$OrdinalETL,
                                           fold = as.numeric(folds.input.data$fold),
                                           stringsAsFactors = FALSE)


input.cols <- c("Collaborativeteams","Commonformativeassessment","Databaseddecisionmaking",
                "CWIS","common_practices_can_statements",
                "common_practices_student_work", "common_practices_self_assessment","common_practices_receive_feedback",
                "common_practices_student_feedback","common_practices_state_criteria")


levels <- c("Very negative","Negative","Neutral","Positive","Very positive")


final.accuracy.list <- list()
accuracy.dt <- list()
predictions.list <- list()
for(i in 1:40){
  set.seed(i)
  print(i)
  # folds.input.data.i <-  folds.input.data[folds.input.data$fold == i,]
  # folds.input.data.i$OrdinalETL <- factor(folds.input.data.i$OrdinalETL,levels)
  # folds.input.data.i <- folds.input.data.i[,-27]
  
  fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  fold.i$OrdinalAverage <- factor(fold.i$OrdinalAverage,levels)
  
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  
  ord_y <- datatrain[,12]
  y_train <- datatrain[,11]
  
  ord_y_test <- datatest[,12]
  y_test <- datatest[,11]
  
  train <- as.matrix(datatrain[,input.cols])
  test <-  as.matrix(datatest[,input.cols])
  
  ordnet <- ordinalNet(train,ord_y, family="cumulative", link="logit",
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
    x = error.values,y=model)) + ggtitle("Coefficient of determination") 



