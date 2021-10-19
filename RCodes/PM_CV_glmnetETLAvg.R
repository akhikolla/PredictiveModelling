library(tidyverse)
library(caret)
library(datarium)
library(glmnet)

# cv.fit = cv.glmnet(x, y,alpha=1,type.measure = "mse")
#cv.fit = cv.glmnet(training_dataset,y_train,family="multinomial", alpha=0.9,type.measure = "class")
# cv.fit = cv.glmnet(x, y,alpha=1,type.measure = "mae")
# cv.fit = cv.glmnet(x, y,family="multinomial",alpha=1,type.measure = "class")
res <- coef(cv.fit, s = cv.fit$lambda.min)

train_predictions <-  predict(cv.fit,newx=training_dataset,s=cv.fit$lambda.1se,type="response")
cm = as.matrix(table(Actual = y_train, Predicted = train_predictions))
table(data.frame(true_values=(y_train), predictions=train_predictions))
accuracy.percent=mean((lasso_predictions-y)^2)*100
weight.vec <- coef(cv.fit)[-1,]
lasso_predictions <- predict(cv.fit,newx = x,s=cv.fit$lambda.1se,type="response")


input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/data/ReducedCoaching+CWISUpdated+NCES- binary.csv")
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
                                     ETLAverage = input.data$ETLAverage,
                                     stringsAsFactors = FALSE)

ETLinput.data.updatedTypes<- data.frame(Collaborativeteams = as.numeric(input.data$`Collaborative teams`),
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

hist(input.data.updatedTypes$ETLAverage,col="coral")
prop.table(table(input.data.updatedTypes$ETLAverage))

trainind <- sort(sample(1:nrow(input.data.updatedTypes), size=floor(nrow(input.data.updatedTypes)*(3/4))))
testind <- setdiff(1:nrow(input.data.updatedTypes), trainind)
datatrain <- input.data.updatedTypes[trainind,]
datatest <- input.data.updatedTypes[testind,]

ETLdatatrain <- ETLinput.data.updatedTypes[trainind,]
ETLdatatest <- ETLinput.data.updatedTypes[testind,]

training_dataset <- as.matrix(datatrain[,input.cols ])
y_train <- datatrain[,11 ] 
ETLy_train = factor(ETLdatatrain[,11],levels)
testing_dataset <- as.matrix(datatest[,input.cols ])
y_test <- datatest[,11 ] 
ETLy_test = ETLdatatest[,11]

cvob1 = cv.glmnet(training_dataset, y_train)
summary(cvob1) 
plot(cvob1)
coef(cvob1)
predict(cvob1, newx = training_dataset[1:5, ], s = "lambda.min")

set.seed(1011)  
cvob1a = cv.glmnet(training_dataset, y_train, type.measure = "mae")
plot(cvob1a)
coef(cvob1a)


cv.fit = cv.glmnet(head(training_dataset,50), head(ETLy_train,50),family = "multinomial",type.measure = "mae")
plot(cv.fit)
preds <- predict(cv.fit, newx = training_dataset, type = "class", s = 0.01)
preds
as.numeric(preds)
title("Multinomial Family", line = 2.5)
frame()

confusion.glmnet(cv.fit, newx = datatest, newy = ETLy_test, s = 0.01)
newX <- model.matrix(data=datatest)

fit3c = cv.glmnet(training_dataset, ETLy_train, family = "multinomial", type.measure="class", keep=TRUE)
idmin = match(fit3c$lambda.min, fit3c$lambda)
confusion.glmnet(fit3c$fit.preval, newy = ETLy_train, family="multinomial")[[idmin]]


tfit3c = cv.glmnet(testing_dataset, ETLy_test, family = "multinomial", type.measure="class", keep=TRUE)
tidmin = match(tfit3c$lambda.min, tfit3c$lambda)
confusion.glmnet(tfit3c$fit.preval, newy = ETLy_test, family="multinomial")[[tidmin]]

newx = x[-train, ],





