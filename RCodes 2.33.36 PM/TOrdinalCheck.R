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


levels <- c("Very negative","Negative","Neutral","Positive","Very positive")

final.accuracy.list <- list()
accuracy.dt <- list()
predictions.list <- list()
for(i in 1:40){
  set.seed(i)
  print(i)
  folds.input.data.i <-  folds.input.data[folds.input.data$fold == i,]
  folds.input.data.i$OrdinalETL <- factor(folds.input.data.i$OrdinalETL,levels)
  folds.input.data.i <- folds.input.data.i[,-27]
  
  fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  ETLfold.i<- ETLinput.data.updatedTypes[ETLinput.data.updatedTypes$fold == i,]
  
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  
  ord.forest.trainind <- sort(sample(1:nrow(ETLfold.i), size=floor(nrow(ETLfold.i)*(3/4))))
  ord.forest.testind <- setdiff(1:nrow(ETLfold.i), ord.forest.trainind)
  ord.forest.datatrain <- ETLfold.i[ord.forest.trainind,]
  ord.forest.datatest <- ETLfold.i[ord.forest.testind,]
  
  ord_x_tr <- as.matrix(ord.forest.datatrain[,input.cols])
  cvtypey <- ord.forest.datatrain[,11]
  ord_y_tr<- factor(ord.forest.datatrain[,11],levels)#factor(folds.input.data.i$OrdinalETL,levels)
  ord_x_te <- as.matrix(ord.forest.datatest[,input.cols])
  ord_y_te<-  ord.forest.datatest[,11]
  
  x_net = as.matrix(datatrain[,input.cols])
  y_net = as.integer(ord_y_tr)#as.factor(datatrain[,11])
  ordnet <- ordinalNet(head(x_net),head(y_net) , family="cumulative", link="logit",
                       parallelTerms=TRUE, nonparallelTerms=FALSE)
  y_got <- predict(ordnet,newx=head(ord_x_te), type="class")
  
  x_train = as.matrix(datatrain[,input.cols])
  y_train = as.factor(datatrain[,11])
  x_test = as.matrix(datatest[,input.cols])
  y_obs =  datatest[,11 ]
  
  cv.glmnet <- cv.glmnet(x_net,head(datatrain[,11],50))
  # glmnet.gaussian=as.numeric(predict(cv.glmnet,newx=head(x_test,50),s=cv.glmnet$lambda.1se,type="class"))
  # 
  # predict(cv.glmnet,newx=head(ord_x_te), type="response")
  # predict(cv.glmnet,newx=head(ord_x_te), type="class")
  # f <- factor(head(ord_y_te), levels) 
  # as.integer(f)
  # obs <- as.integer(head(ord_y_te))
  
  predictions.list <-  list(
    glmnet.gaussian=as.numeric(predict(cv.fit.gaussian,newx=as.matrix(datatest[,input.cols]),s=cv.fit.gaussian$lambda.1se,type="response")),
    baseline.l0=one.pred(as.numeric.factor(freq[which.max(freq$Freq),]$y)),
    baseline.l1=one.pred(median.ind.val),
    baseline.l2=one.pred(mean.ind.val),
    ordnet.pred = predict(ordnet,newx=head(ord_x_te), type="class"))
  
  accuracy.dt.list <- list()
  for(algo in names(predictions.list)){
    print(algo)
    pred.vec = predictions.list[[algo]]
    print(accuracy(pred.vec, y_obs)*100)
    d = y_obs - pred.vec
    accuracy.dt.list[[algo]] <- data.table(
      algo.name = algo,
      error.percent= sqrt(mean((pred.vec - y_obs)^2))#1-(sum((d)^2)/sum((y_obs-mean(y_obs))^2))
      #(mae(y_obs,pred.vec))#mae(y_obs,pred.vec)*100 #sqrt(mean((y_obs - pred.vec)^2))*100
    )#mean((pred.vec-y_obs)^2) * 100 )#mean((y_obs-pred.vec)^2) * 100)
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






summary(fit1)

weight.dt.list[[i]] <- data.table(
  i,
  row.name=rownames(coef(fit1))[-1],
  weight=coef(fit1)[-1])
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

# coef(fit1)
# coef(fit1, matrix=TRUE)
predict(fit1,newx=x_test, type="response")
predict(fit1,newx=x_test, type="class")



library("ordinalNet")
library("ordinalgmifs")
data("hccframe")
y <- hccframe$group
x <- as.matrix(subset(hccframe, select = -group))




fit1 <- ordinalNet(x, y, family = "cumulative", link = "logit",
                   parallelTerms = TRUE, nonparallelTerms = FALSE)
predict(fit1, type="response")
res <- predict(fit1, type="class")
levels <- c("Normal","Cirrhosis non-HCC","Tumor")
f <- factor(res, levels)
levels2int <- structure(seq_along(levels), names=levels)
levels2int[paste(res)]



fit1 <- ordinalNet(head(x_train), head(y_train), family = "cumulative", link = "logit",
                   parallelTerms = TRUE, nonparallelTerms = FALSE)
predict(fit1,newx=x_test, type="response")


x = as.matrix(datatrain[,input.cols])
y = datatrain[,11 ]

x_train = as.matrix(datatrain[,input.cols])
y_train = as.factor(datatrain[,11])
x_test = as.matrix(datatest[,input.cols])
y_obs =  datatest[,11 ]

x_net = as.matrix(datatrain[,input.cols])
y_net = as.factor(datatrain[,11])

colnames(x_net)<-NULL
rownames(x_net)<-NULL
cv.fit.gaussian <- cv.glmnet(x_train,y)


