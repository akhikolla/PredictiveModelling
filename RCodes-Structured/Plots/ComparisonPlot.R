library(data.table)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(caret)

input.data<- data.table::fread("/Users/akhilachowdarykolla/Downloads/ReducedCoaching+CWISUpdated+NCES- binary.csv")
head(input.data)
#print(names(input.data))
#total rows = 25547
# training - 20439
# testing - 5108
colnames(input.data)[27] <- "ETLAverage"
fold.vec <- sample(rep(1:10,l=nrow(input.data)))
fold.vec <- sample(rep(1:10,l=nrow(input.data)))
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
                                           fold = as.numeric(folds.input.data$fold),
                                           stringsAsFactors = FALSE)

ETLinput.data.updatedTypes<- data.frame(Collaborativeteams = as.numeric(folds.input.data$`Collaborative teams`),
                                        Commonformativeassessment = as.integer(folds.input.data$`Common formative assessment`),
                                        Databaseddecisionmaking = as.numeric(folds.input.data$`Data-based decision making`),
                                        CWIS = as.numeric(folds.input.data$CWIS),
                                        common_practices_can_statements =as.numeric(folds.input.data$common_practices_can_statements),
                                        common_practices_student_work = as.numeric(folds.input.data$common_practices_student_work),
                                        common_practices_self_assessment= as.numeric(folds.input.data$common_practices_self_assessment),
                                        common_practices_receive_feedback = as.numeric(folds.input.data$common_practices_receive_feedback),
                                        common_practices_student_feedback = as.numeric(folds.input.data$common_practices_student_feedback),
                                        common_practices_state_criteria = as.numeric(folds.input.data$common_practices_state_criteria),
                                        ETLAverage = folds.input.data$OrdinalETL,
                                        fold = as.numeric(folds.input.data$fold),
                                        stringsAsFactors = FALSE)


input.cols <- c("Collaborativeteams","Commonformativeassessment","Databaseddecisionmaking",
                "CWIS","common_practices_can_statements",
                "common_practices_student_work", "common_practices_self_assessment","common_practices_receive_feedback",
                "common_practices_student_feedback","common_practices_state_criteria","fold")

# trainind <- sort(sample(1:nrow(folds.input.data.updatedTypes), size=floor(nrow(folds.input.data.updatedTypes)*(3/4))))
# testind <- setdiff(1:nrow(folds.input.data.updatedTypes), trainind)
# datatrain <- folds.input.data.updatedTypes[trainind,]
# datatest <- folds.input.data.updatedTypes[testind,]
# 
# training_dataset <- datatrain[,input.cols ]
# y_train <- datatrain[,11 ] 
# 
# testing_dataset <- as.matrix(datatest[,input.cols ])
# y_test <- datatest[,11 ] 



ggplot() +
  geom_point(mapping = aes(x = value, y = algorithm, color = algorithm)) 

levels <- c("Very negative","Negative","Neutral","Positive","Very positive")

final.accuracy.list <- list()
accuracy.dt <- list()

for(i in 1:10){
  set.seed(i)
  print(i)
  folds.input.data.i <-  folds.input.data[folds.input.data$fold == i,]
  folds.input.data.i$OrdinalETL <- factor(folds.input.data.i$OrdinalETL,levels)
  folds.input.data.i <- folds.input.data.i[,-27]
  
  fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  y_obs =  datatest[,11 ] 
  
  ord.forest.trainind <- sort(sample(1:nrow(folds.input.data.i), size=floor(nrow(folds.input.data.i)*(3/4))))
  ord.forest.testind <- setdiff(1:nrow(folds.input.data.i), trainind)
  ord.forest.datatrain <- folds.input.data.i[ord.forest.trainind,]
  ord.forest.datatest <- folds.input.data.i[ord.forest.testind,]
  
  x = as.matrix(datatrain[,input.cols])
  # print(x)
  y = datatrain[,11 ]
  # print(y)
  one.pred <- function(x)rep(x, nrow(datatest))
  freq <-as.data.frame(table(y))
  median.values <- sort(y)
  median.ind.val <- median(median.values)
  mean.ind.val <- mean(y)
  cv.fit.gaussian <- cv.glmnet(x,y)
  ordforres <- ordfor(depvar="OrdinalETL", data=ord.forest.datatrain, nsets=1000, ntreeperdiv=100,
                      ntreefinal=5000, perffunction = "probability")
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
predictions.list <-  list(
glmnet.gaussian=predict(cv.fit.gaussian,newx=as.matrix(datatest[,input.cols]),s=cv.fit.gaussian$lambda.1se,type="response"),
  ordinalForest=predict(ordforres, newdata=ord.forest.datatest),
  baseline.l0=one.pred(as.numeric.factor(freq[which.max(freq$Freq),]$y)),
  baseline.l1=one.pred(median.ind.val),
  baseline.l2=one.pred(mean.ind.val))

accuracy.dt.list <- list()
for(algorithm in names(predictions.list)){
  print(algorithm)
  pred.vec = predictions.list[[algorithm]]
  if(algorithm == "ordinalForest"){
    table(data.frame(true_values=ord.forest.datatest$OrdinalETL, predictions=pred.vec$ypred))
    cm = as.matrix(table(data.frame(true_values=ord.forest.datatest$OrdinalETL, predictions=pred.vec$ypred)))
    n = sum(cm) # number of instances
    nc = nrow(cm) # number of classes
    diag = diag(cm) # number of correctly classified instances per class 
    rowsums = apply(cm, 1, sum) # number of instances per class
    colsums = apply(cm, 2, sum) # number of predictions per class
    p = rowsums / n # distribution of instances over the actual classes
    q = colsums / n # distribution of instances over the predicted classes
    s = matrix(0, nrow = 2, ncol = 2)
    accuracy.dt.list[[algorithm]] <- data.table(
      algo.name = algorithm,
      accuracy= (sum(diag) / n)*100
    )
  }else if(algorithm == "glmnet.gaussian"){
    accuracy.dt.list[[algorithm]] <- data.table(
      algo.name = algorithm,
      accuracy= 1-(sum((d)^2)/sum((y_obs-mean(y_obs))^2))
    )
  }
}
  accuracy.dt[[i]] <- data.table(fold=i,accuracies = do.call(rbind, accuracy.dt.list))
}
final.accuracy.list <- do.call(rbind, accuracy.dt) 
final.accuracy.list

error.values = final.accuracy.list$accuracies.accuracy
model=final.accuracy.list$accuracies.algo.name

ggplot()+
  geom_point(aes(
    x = error.values,y=model)) + ggtitle("Coefficient of determination") 


# 
# algorithm =  names(predictions.list)
# accuracy[i] = as.numeric(accuracys)
# print(accuracy[i])
}
# temp_plot = ggplot() +
#   geom_point(mapping = aes(x = , y = algorithm, color = algorithm)) 
# }

#par(new=TRUE)
# ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10)
# }
  