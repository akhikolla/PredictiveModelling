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
                                     ETLAverage = input.data$ETLAverage,
                                     stringsAsFactors = FALSE)

input.cols <- c("Collaborativeteams","Commonformativeassessment","Databaseddecisionmaking",
                "CWIS","common_practices_can_statements",
                "common_practices_student_work", "common_practices_self_assessment","common_practices_receive_feedback",
                "common_practices_student_feedback","common_practices_state_criteria")

random_sample <- createDataPartition(input.data$ETLAverage, p = 0.8, list = FALSE)
is.train <- rep(FALSE, length(random_sample))
for(val in random_sample){
  is.train[val] = TRUE
}
validations = rep(FALSE, length(random_sample))
my_range <- 1:length(is.train)
for(tval in my_range){
  print(tval)
  if(identical(is.train[tval],FALSE) ){
    validations <- c(validations,tval)
  }
}

vars <- unlist(validations)
validation_data <- input.data[vars]
validation_dataset <- input.columns[vars,]

data_2_model <- input.data.updatedTypes[random_sample,]
input.columns <- input.data.updatedTypes[,input.cols]
cor(input.columns) 

trainind <- sort(sample(1:nrow(input.data.updatedTypes), size=floor(nrow(input.data.updatedTypes)*(3/4))))
testind <- setdiff(1:nrow(input.data.updatedTypes), trainind)
datatrain <- input.data.updatedTypes[trainind,]
datatest <- input.data.updatedTypes[testind,]


training_dataset <- datatrain[,input.cols ]
#testing_dataset <- input.data[, ]

x <- as.matrix(training_dataset)
head(x)
y <-input.data.updatedTypes[random_sample,11]
head(y)
validation_dataset <- input.columns[vars,]
tx <- as.matrix(validation_dataset)

# cv.fit = cv.glmnet(x, y,alpha=1,type.measure = "mse")
cv.fit = cv.glmnet(x, y,alpha=1,type.measure = "deviance")
# cv.fit = cv.glmnet(x, y,alpha=1,type.measure = "mae")
# cv.fit = cv.glmnet(x, y,family="multinomial",alpha=1,type.measure = "class")
weight.vec <- coef(cv.fit)[-1,]
lasso_predictions <- predict(cv.fit,newx = x,s=cv.fit$lambda.1se,type="response")
accuracy.percent=mean((lasso_predictions-y)^2)*100

lasso_predict <- list()
for(i in 1:nrow(lasso_prob)){
  if(max(lasso_prob[i]) >0.5)
lasso_predict[i] <- "Positive"
}
lasso_predict <- rep("neg",nrow(tx))
table(pred=lasso_predict,true=testData$New_Product_Type)
table(pred=lasso_prob,true=validation_data$OrdinalETL)

v <- as.matrix(validation_dataset)
head(v)
plot(cv.fit)
predict(cv.fit, newx = x[1:5, ])
coef(cv.fit, s = "lambda.min")
predict(cv.fit, newx = x[1:5, ], s = c(0.001, 0.002))



cv.fitr = cv.glmnet(x, y, relax = TRUE)
predict(cv.fit, newx = x[1:5, ])
coef(cv.fit)
plot(coef(cv.fit))
plot(coef(cv.fit, s = "lambda.min", gamma = "gamma.min"))
predict(cv.fit, newx = x[1:5, ], s = c(0.001, 0.002), gamma = "gamma.min")

#cvfit = cv.glmnet(x, y=as.factor(trainData$New_Product_Type), alpha=1, family="binomial",type.measure = "mse")
lambda_1se <- cv.fit$lambda.1se
coef(cv.fit,s=lambda_1se)
x_test <- model.matrix(New_Product_Type~.,data = testData)
lasso_prob <- predict(cvfit,newx = x_test,s=lambda_1se,type="response")
lasso_predict <- rep("neg",nrow(testData))
table(pred=lasso_predict,true=testData$New_Product_Type)

mean((lasso_prob - validation_data$ETLAverage)^2)

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


x <- as.matrix(datatrain)
head(x)
y <- datatrain$FactorOrdinalETL
head(y)


set.seed(1010)
cvob1 = cv.glmnet(x, y)
plot(cvob1)#,xvar="lambda",label=TRUE)
coef(cvob1)

plot(coef(cvob1))

predictions <- predict(cvob1, newx = v  , s = "lambda.min")
plot(predictions)
title("Cross-Validation curve", line = 2.5)

plot()

valueFormula <- ETLAverage ~ Collaborativeteams + Commonformativeassessment+Databaseddecisionmaking+CWIS+common_practices_can_statements + common_practices_student_work+common_practices_self_assessment+common_practices_receive_feedback+common_practices_student_feedback+common_practices_state_criteria - 1
manX <- useful::build.x(valueFormula, data=data_2_model,
                        # do not drop the baselines of factors
                        contrasts=FALSE,
                        # use a sparse matrix
                        sparse=TRUE)

manY <- useful::build.y(valueFormula, data=data_2_model)

mod1 <- glmnet(x=manX, y=manY, family='gaussian')
coefplot(mod1, lambda=330500, sort='magnitude')
plot(mod1, xvar='lambda', label=TRUE)
coefpath(mod1)




mod2 <- cv.glmnet(x=manX, y=manY, family='gaussian', alpha=0.8, nfolds=100)

coefplot(mod2, lambda='lambda.1se', sort='magnitude')

coefplot(mod2, lambda='lambda.min', sort='magnitude')

coefpath(mod2)
