library(tidyverse)
library(caret)
library(datarium)
library(glmnet)

#/Users/akhilachowdarykolla/Desktop/ReducedCoaching+CWISUpdated+NCES.csv
input.data <- data.table::fread("/Users/akhilachowdarykolla/Downloads/ReducedCoaching+CWISUpdated+NCES- binary.csv")
head(input.data)
print(names(input.data))
#total rows = 25547
# training - 20439
# testing - 5108
colnames(input.data)[27] <- "ETLAverage"


input.data$OrdinalETL <- ""
for(i in 1:nrow(input.data)) {
  row <- input.data[i,]
  etlScore <- row$ETLAverage
  print(etlScore)
  if (etlScore <= 1) {
    input.data[i,]$OrdinalETL <- "Very negative"
    print("Very negative")
  } else if (etlScore > 1 && etlScore <= 2) {
    input.data[i,]$OrdinalETL <- "Negative"
    print("Negative")
  } else if ( etlScore > 2 && etlScore <= 3) {
    input.data[i,]$OrdinalETL <- "Neutral"
    print("Neutral")
  } else if(etlScore > 3 && etlScore <= 4){
    input.data[i,]$OrdinalETL <- "Very positive"
    print("Very positive")
  }else{
    input.data[i,]$OrdinalETL <- "Positive"
    print("Positive")
  }
  
}
input.data.updatedTypes<- data.frame(Collaborativeteams = as.numeric(input.data$`Collaborative teams`),
                                     Commonformativeassessment = as.integer(input.data$`Common formative assessment`),
                                     Databaseddecisionmaking = as.numeric(input.data$`Data-based decision making`),
                                     CWIS = as.numeric(input.data$CWIS),
                                     common_practices_can_statements =as.numeric(input.data$common_practices_can_statements),
                                     common_practices_student_work = as.numeric(input.data$common_practices_student_work),
                                     common_practices_self_assessment= as.numeric(input.data$common_practices_self_assessment),
                                     common_practices_receive_feedback = as.numeric(input.data$common_practices_receive_feedback),
                                     common_practices_student_feedback = as.numeric(input.data$common_practices_student_feedback),
                                     common_practices_state_criteria = as.numeric(input.data$common_practices_state_criteria),
                                     ETLAverage = input.data$OrdinalETL,
                                     stringsAsFactors = FALSE)

input.cols <- c("Collaborativeteams","Commonformativeassessment","Databaseddecisionmaking",
                "CWIS","common_practices_can_statements",
                "common_practices_student_work", "common_practices_self_assessment","common_practices_receive_feedback",
                "common_practices_student_feedback","common_practices_state_criteria")

trainind <- sort(sample(1:nrow(input.data.updatedTypes), size=floor(nrow(input.data.updatedTypes)*(3/4))))
testind <- setdiff(1:nrow(input.data.updatedTypes), trainind)
datatrain <- input.data.updatedTypes[trainind,]
datatest <- input.data.updatedTypes[testind,]

training_dataset <- as.matrix(datatrain[,input.cols ])
y_train <- datatrain[,11 ] 

testing_dataset <- as.matrix(datatest[,input.cols ])
y_test <- datatest[,11 ] 
  
  

# cv.fit = cv.glmnet(x, y,alpha=1,type.measure = "mse")
cv.fit = cv.glmnet(training_dataset,y_train,family="multinomial", alpha=0.9,type.measure = "class")
# cv.fit = cv.glmnet(x, y,alpha=1,type.measure = "mae")
# cv.fit = cv.glmnet(x, y,family="multinomial",alpha=1,type.measure = "class")
res <- coef(cv.fit, s = cv.fit$lambda.min)




predictions.list <-  list(
  baseline.l0=most frequent train label
  baseline.l1=median train labels
  baseline.l2=mean train labels,
  glmnet.multinomial=predict(cv.fit,newx=test_dataset,s=cv.fit$lambda.1se,type="response"),
  glmnet.gaussian=round(predict(cv.gaussian, ...)),
  ordinalForestWrongOrder=predict(fit.forest.wrong.factor),
  ordinalForestRightOrder=predict(fit...),
  ordinalNet=predict(ord.fit, ...))

for(algorithm in names(predictions.list)){
  pred.vec = predictions.list[[algorithm]]
  if(any(!pred.vec %in% 1:5)){
    stop("some predictions are not integers between 1 and 5 please fix")
  }
  DO EVALUATION METRICS: L0, L1, L2
}
cm = as.matrix(table(Actual = y_train, Predicted = train_predictions))
table(data.frame(true_values=(y_train), predictions=train_predictions))
accuracy.percent=mean((lasso_predictions-y)^2)*100
weight.vec <- coef(cv.fit)[-1,]
lasso_predictions <- predict(cv.fit,newx = x,s=cv.fit$lambda.1se,type="response")




  
  
