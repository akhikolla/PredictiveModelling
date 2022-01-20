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


ordnet.weight.dt.list <- list()
glment.weight.dt.list <- list()
for(i in 1:10){
  set.seed(i)
  test.fold = i
  datatrain <-glment.binary.input.data[fold != test.fold]
  ord_y_train <- as.factor(datatrain$BinaryETL)
  y_train <- as.numeric(as.matrix(datatrain[,99]))
 
  train <- as.matrix(datatrain[,-c(98,99)])
  
  cvob1 <- cv.glmnet(train, y_train, family = "binomial")
  ordnet <- ordinalNetTune(train,ord_y_train,parallelTerms=TRUE)
  bestLambdaIndex <- which.max(rowMeans(ordnet$loglik))
  summary(ordnet)
  
  glment.weight.dt.list[[i]] <- data.table(
    i,
    row.name=rownames(coef(cvob1))[-1],
    weight=coef(cvob1)[-1])
  print("------")
  
  ordnet.weight.dt.list[[i]] <- data.table(
    i,
    row.name=colnames(ordnet$fit$coefs)[-1],
    weight=as.vector(ordnet$fit$coefs[bestLambdaIndex,])[-1])
  print("------")
  
}

weight.dt <- do.call(rbind, glment.weight.dt.list)
ordnet.weight.dt <- do.call(rbind, ordnet.weight.dt.list)

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


png(filename="/Users/akhilachowdarykolla/Documents/PredictiveModelling/figures/updated23NovglmnetImportance.png",500,700)
print(glmnetImportance)
dev.off()

ggplot()+
  geom_point(aes(
    weight, row.name),
    data=ordnet.weight.dt)

ordnet.weight.dt[, n.nonzero := sum(weight != 0), by=row.name]

unique.ordnet.weight.dt <- ordnet.weight.dt[ordnet.weight.dt$n.nonzero > 0]
unique.ordnet.weight.dt$weight <- -1 * (unique.ordnet.weight.dt$weight)

ordinalImportance <- ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weight, row.name),
    data=unique.ordnet.weight.dt)


png(filename="/Users/akhilachowdarykolla/Documents/PredictiveModelling/figures/varImp23Nov.png",500,700)
print(ordinalImportance)
dev.off()


