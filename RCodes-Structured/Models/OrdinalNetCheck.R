library("ordinalNet")

library(tidyverse)
library(caret)
library("datarium")
library(ggplot2)
#/Users/akhilachowdarykolla/Desktop/ReducedCoaching+CWISUpdated+NCES.csv
input.data <- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/data/ReducedCoaching+CWISUpdated+NCES- binary.csv")
head(input.data)
print(names(input.data))
#total rows = 25547
# training - 20439
# testing - 5108

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
                                     ETLAverage = input.data$`ETL AVERAGE`,
                                     LocaleCode = as.numeric(input.data$`Locale Code*`),
                                     students= as.numeric(input.data$`Students*`),
                                     teachers = as.numeric(input.data$`Teachers*`),
                                     studentTeacherRatio = as.numeric(input.data$`Student Teacher Ratio*`),
                                     ReducedLunch= as.numeric(input.data$`Reduced Lunch*`),
                                     freelunch = as.numeric(input.data$`Free Lunch*`),
                                     CollectiveTeacherEfficacy=as.numeric(input.data$`Collective teacher efficacy`),
                                     schoolBasedCoaching = as.numeric(input.data$`School-based implementation coaching`),
                                     instructionalLeadership = as.numeric(input.data$`Instructional Leadership`),
                                     teachingLearningPractices = as.numeric(input.data$`Effective teaching/learning practices`),
                                     stringsAsFactors = FALSE)

input.cols <- c("Collaborativeteams","Commonformativeassessment","Databaseddecisionmaking",
                "CWIS","common_practices_can_statements",
                "common_practices_student_work", "common_practices_self_assessment","common_practices_receive_feedback",
                "common_practices_student_feedback","common_practices_state_criteria","LocaleCode","students","teachers",
                "studentTeacherRatio","ReducedLunch","freelunch","CollectiveTeacherEfficacy","schoolBasedCoaching",
                "instructionalLeadership","teachingLearningPractices")
trainind <- sort(sample(1:nrow(input.data.updatedTypes), size=floor(nrow(input.data.updatedTypes)*(3/4))))
testind <- setdiff(1:nrow(input.data.updatedTypes), trainind)
datatrain <- input.data.updatedTypes[trainind,]
datatest <- input.data.updatedTypes[testind,]
x_train = as.matrix(datatrain[,input.cols])
y_train = as.factor(datatrain[,11])
x_test = as.matrix(datatest[,input.cols])
y_obs =  datatest[,11 ] 
colnames(x_train)<-NULL
rownames(x_train)<-NULL
head(x_train)


x5 <- head(x_train,10)
y5 <- head(y_train,10)
colnames(x_test)<-NULL
rownames(x_test)<-NULL
xt <- head(x_test,10)
yt <- head(y_obs,5)

fit1 <- ordinalNet(x5, y5, family="cumulative", link="logit",
                   parallelTerms=TRUE, nonparallelTerms=FALSE)
predict(fit1,newx=xt, type="response")
predict(fit1,newx=xt, type="class")

fit2 <- ordinalNet(x5, y5, family="cumulative", link="logit",
                   parallelTerms=FALSE, nonparallelTerms=TRUE)
predict(fit2,newx=xt, type="response")
predict(fit2,newx=xt, type="class")
fit3 <- ordinalNet(x5, y5, family="cumulative", link="logit",
                   parallelTerms=TRUE, nonparallelTerms=TRUE)
predict(fit3,newx=xt, type="response")
predict(fit3,newx=xt, type="class")






# Fit parallel cumulative logit model
fit1 <- ordinalNet(x5, y5, family="cumulative", link="logit",
                   parallelTerms=TRUE, nonparallelTerms=FALSE)


# Fit parallel cumulative logit model
fit1 <- ordinalNet(x_train, y_train, family="cumulative", link="logit",
                   parallelTerms=TRUE, nonparallelTerms=FALSE)
summary(fit1)
coef(fit1)
coef(fit1, matrix=TRUE)
predict(fit1, type="response")
predict(fit1, type="class")



n <- 50
intercepts <- c(-1, 1)
beta <- c(1, 1, 0, 0, 0)
ncat <- length(intercepts) + 1  # number of response categories
p <- length(beta)  # number of covariates
x <- matrix(rnorm(n*p), ncol=p)  # n x p covariate matrix
eta <- c(x %*% beta) + matrix(intercepts, nrow=n, ncol=ncat-1, byrow=TRUE)
invlogit <- function(x) 1 / (1+exp(-x))
cumprob <- t(apply(eta, 1, invlogit))
prob <- cbind(cumprob, 1) - cbind(0, cumprob)
yint <- apply(prob, 1, function(p) sample(1:ncat, size=1, prob=p))
y <- as.factor(yint)


