library(data.table)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(caret)
library(datarium)
library(glmnet)

glment.binary.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/PredictiveModelling/data/binary_factored_combined_binary.input_data.csv")
head(glment.binary.input.data)
glment.binary.input.data <- glment.binary.input.data[,-c(1,2,19)]


glment.weight.dt.list <- list()
for(i in 1:10){
  set.seed(i)
  test.fold = i
  datatrain <-glment.binary.input.data[fold != test.fold]
  datatest <- glment.binary.input.data[fold == test.fold]
  y_train <- as.numeric(as.matrix(datatrain[,99]))
  train <- as.matrix(datatrain[,-c(98,99)])
  test <-  as.matrix(datatest[,-c(98,99)])
  cvob1 <- cv.glmnet(train, y_train, family = "binomial")
  glment.weight.dt.list[[i]] <- data.table(
    i,
    row.name=rownames(coef(cvob1))[-1],
    weight=coef(cvob1)[-1])
  print("------")
  
}
weight.dt <- do.call(rbind, glment.weight.dt.list)

ggplot()+
  geom_point(aes(
    weight, row.name),
    data=weight.dt)

weight.dt[, n.nonzero := sum(weight != 0), by=row.name]
unique.imp <- weight.dt[weight.dt$n.nonzero > 0]

glmnetImportance <- ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weight, row.name),
    data=unique.imp)


png(filename="/Users/akhilachowdarykolla/Documents/PredictiveModelling/figures/22NovglmnetImportance.png",500,700)
print(glmnetImportance)
dev.off()



#/Users/akhilachowdarykolla/Desktop/ReducedCoaching+CWISUpdated+NCES.csv
# input.data<- data.table::fread("/Users/akhilachowdarykolla/Downloads/ReducedCoaching+CWISUpdated+NCES- binary.csv")
# head(input.data)
# print(names(input.data))
# #total rows = 25547
# # training - 20439
# # testing - 5108
# colnames(input.data)[27] <- "ETLAverage"
# fold.vec <- sample(rep(1:10,l=nrow(input.data)))
# fold.vec <- sample(rep(1:10,l=nrow(input.data)))
# folds.input.data <- data.table::data.table(input.data,fold=factor(fold.vec))
# 
# folds.input.data$OrdinalETL <- ""
# for(i in 1:nrow(folds.input.data)) {
#   row <- folds.input.data[i,]
#   etlScore <- row$ETLAverage
#   # print(etlScore)
#   if (etlScore <= 1) {
#     folds.input.data[i,]$OrdinalETL <- "Very negative"
#     # print("Very negative")
#   } else if (etlScore > 1 && etlScore <= 2) {
#     folds.input.data[i,]$OrdinalETL <- "Negative"
#     # print("Negative")
#   } else if ( etlScore > 2 && etlScore <= 3) {
#     folds.input.data[i,]$OrdinalETL <- "Neutral"
#     # print("Neutral")
#   } else if(etlScore > 3 && etlScore <= 4){
#     folds.input.data[i,]$OrdinalETL <- "Very positive"
#     # print("Very positive")
#   }else{
#     folds.input.data[i,]$OrdinalETL <- "Positive"
#     # print("Positive")
#   }
#   
# }
# folds.input.data.updatedTypes<- data.frame(Collaborativeteams = as.numeric(folds.input.data$`Collaborative teams`),
#                                      Commonformativeassessment = as.integer(folds.input.data$`Common formative assessment`),
#                                      Databaseddecisionmaking = as.numeric(folds.input.data$`Data-based decision making`),
#                                      CWIS = as.numeric(folds.input.data$CWIS),
#                                      common_practices_can_statements =as.numeric(folds.input.data$common_practices_can_statements),
#                                      common_practices_student_work = as.numeric(folds.input.data$common_practices_student_work),
#                                      common_practices_self_assessment= as.numeric(folds.input.data$common_practices_self_assessment),
#                                      common_practices_receive_feedback = as.numeric(folds.input.data$common_practices_receive_feedback),
#                                      common_practices_student_feedback = as.numeric(folds.input.data$common_practices_student_feedback),
#                                      common_practices_state_criteria = as.numeric(folds.input.data$common_practices_state_criteria),
#                                      ETLAverage = folds.input.data$ETLAverage,
#                                      fold= as.numeric(folds.input.data$fold),
#                                      stringsAsFactors = FALSE)
# 
# input.cols <- c("Collaborativeteams","Commonformativeassessment","Databaseddecisionmaking",
#                 "CWIS","common_practices_can_statements",
#                 "common_practices_student_work", "common_practices_self_assessment","common_practices_receive_feedback",
#                 "common_practices_student_feedback","common_practices_state_criteria")
# 
# # trainind <- sort(sample(1:nrow(folds.input.data.updatedTypes), size=floor(nrow(folds.input.data.updatedTypes)*(3/4))))
# # testind <- setdiff(1:nrow(folds.input.data.updatedTypes), trainind)
# # datatrain <- folds.input.data.updatedTypes[trainind,]
# # datatest <- folds.input.data.updatedTypes[testind,]
# # 
# # training_dataset <- datatrain[,input.cols ]
# # y_train <- datatrain[,11 ] 
# # 
# # testing_dataset <- as.matrix(datatest[,input.cols ])
# # y_test <- datatest[,11 ] 
# 
# 

weight.dt.list <- list()
for(i in 1:10){
  set.seed(i)
  print(i)
  fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  y_obs =  datatest[,11 ] 
  x = as.matrix(datatrain[,input.cols])
  y = datatrain[,11 ]
  set.seed(1011)
  cvob1 = cv.glmnet(x, y)
  weight.dt.list[[i]] <- data.table(
    i,
    row.name=rownames(coef(cvob1))[-1],
    weight=coef(cvob1)[-1])
  print("------")
}
weight.dt <- do.call(rbind, weight.dt.list)

ggplot()+
  geom_point(aes(
    weight, row.name),
    data=weight.dt)

weight.dt[, n.nonzero := sum(weight != 0), by=row.name]


ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weight, row.name),
    data=weight.dt)



ordforres <- ordfor(depvar="FactorOrdinalETL", data=datatrain, nsets=1000, ntreeperdiv=100,
                    ntreefinal=5000, perffunction = "probability")
ordforres

sort(ordforres$varimp, decreasing=TRUE)