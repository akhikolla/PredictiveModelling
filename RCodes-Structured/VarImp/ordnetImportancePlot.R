ordinal.binary.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/PredictiveModelling/data/binary_factored_combined_binary.input_data.csv")
#head(ordinal.binary.input.data)
ordinal.binary.input.data <- ordinal.binary.input.data[,-c(1,2,19)]

head(ordinal.binary.input.data)
head(ordinal.binary.input.data$BinaryETL,100)
unique(ordinal.binary.input.data$BinaryETL)

ordinal.binary.input.data$BinaryETL <- as.factor(ordinal.binary.input.data$BinaryETL)


ordinal.binary.input.data <- as.data.frame(ordinal.binary.input.data)

for(j in 1:98){
  ordinal.binary.input.data[,j][is.na(ordinal.binary.input.data[,j])] <- 0
}

for(k in 1:98){
  ordinal.binary.input.data[,k] <- as.numeric(ordinal.binary.input.data[,k])
}


ordinal.binary.input.data <- as.data.table(ordinal.binary.input.data)
head(ordinal.binary.input.data)


ordnet.weight.dt.list <- list()
for(i in 1:10){
  set.seed(i)
  test.fold = i
  datatrain <-ordinal.binary.input.data[fold != test.fold]
  datatest <- ordinal.binary.input.data[fold == test.fold]
  ##targets for models
  ord_y_train <- datatrain$BinaryETL
  ord_y_test <- datatest$BinaryETL
  ## train,test categorized data
  ##glmnet baseline train
  train <- as.matrix(datatrain[,-c(98,99)])
  test <-  as.matrix(datatest[,-c(98,99)])
  
  fit=randomForest(BinaryETL~., data=rf.binary.input.data)
  
  ordnet <- ordinalNetTune(train,ord_y_train,parallelTerms=TRUE)
  bestLambdaIndex <- which.max(rowMeans(ordnet$loglik))
  summary(ordnet)
  
  ordnet.weight.dt.list[[i]] <- data.table(
    i,
    row.name=colnames(ordnet$fit$coefs)[-1],
    weight=as.vector(ordnet$fit$coefs[bestLambdaIndex,])[-1])
  print("------")
}
ordnet.weight.dt <- do.call(rbind, ordnet.weight.dt.list)

ggplot()+
  geom_point(aes(
    weight, row.name),
    data=ordnet.weight.dt)

ordnet.weight.dt[, n.nonzero := sum(weight != 0), by=row.name]

unique.ordnet.weight.dt <- ordnet.weight.dt[ordnet.weight.dt$n.nonzero > 0]


ordinalImportance <- ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weight, row.name),
    data=unique.ordnet.weight.dt)


png(filename="/Users/akhilachowdarykolla/Documents/PredictiveModelling/figures/ordinalImportance.png",500,700)
print(ordinalImportance)
dev.off()

