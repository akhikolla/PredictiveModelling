library(tidyverse)
library(caret)
library(datarium)
library(glmnet)
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


random_sample <- createDataPartition(input.data$OrdinalETL, p = 0.8, list = FALSE)
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

training_dataset <- input.columns[random_sample, ]
#testing_dataset <- input.data[~random_sample, ]

x <- as.matrix(training_dataset)
head(x)
y <-as.factor(input.data.updatedTypes[random_sample,11])
head(y)
validation_dataset <- input.columns[vars,]
tx <- as.matrix(validation_dataset)

cv.fit = cv.glmnet(x, y, alpha=1, family="multinomial",type.measure = "class")
coef(cv.fit)
lasso_prob <- predict(cv.fit,newx = tx,s=cv.fit$lambda.1se,type="response")
weight.vec <- coef(cv.fit)[-1,]
lasso_prob <- predict(cv.fit,tx,type="response")
mean((lasso_prob-validation_data$`ETL AVERAGE`))*100


errorPercent <- function( predictions, answersTilde)
{
  ## too slow
  
  #count <- 0
  #for( i in 1: length(predictions) )
  #{
  #  if (round( sigmoid(predictions[i])) == answers[i])
  #    count = count+1
  #}
  #return ( 100*(1-count/length(predictions)) )
  
  predictions <- ifelse(predictions>0, 1, -1)
  100*colMeans(as.vector(answersTilde) != predictions)
}
table(pred=lasso_predict,true=testData$New_Product_Type)
table(pred=lasso_prob,true=validation_data$OrdinalETL)

errorPercent(lasso_prob,validation_data$ETLAverage)


