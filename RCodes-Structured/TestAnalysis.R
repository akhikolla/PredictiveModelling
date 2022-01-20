library(tidyverse)
library(caret)
library(datarium)
library(glmnet)

#/Users/akhilachowdarykolla/Desktop/ReducedCoaching+CWISUpdated+NCES.csv
input.data <- data.table::fread("/Users/akhilachowdarykolla/Downloads/ReducedCoaching+CWISUpdated+NCES- binary.csv")
head(input.data)
colnames(input.data)[27] <- "ETLAverage"

trainind <- sort(sample(1:nrow(input.data), size=floor(nrow(input.data)*(2/3))))
testind <- setdiff(1:nrow(input.data), trainind)
datatrain <- input.data[trainind,]
datatest <- input.data[testind,]
datatest <- datatest[,-1]
x <- as.matrix(datatrain[,-27])
head(x)
y <- datatrain$ETLAverage
head(y)

set.seed(1010)
cvob1 = cv.glmnet(x, y)
plot(cvob1)#,xvar="lambda",label=TRUE)
coef(cvob1)
plot(coef(cvob1))
plot(coef(cvob1, s = "lambda.min"))
plot(coef(cvob1, s = "lambda.1se"))

#testset <- datatest[,-27] 
v <- (datatest[,-26])
newX <- model.matrix(v~., data=datatest)
predictions <- predict(cvob1, newx =v[1:5,] , s = "lambda.min")
plot(predictions)
title("Cross-Validation curve", line = 2.5)

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
