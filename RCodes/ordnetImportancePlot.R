binary.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/binary_factored_combined_binary.input_data.csv")
head(binary.input.data)
binary.input.data <- binary.input.data[,-c(1,2,19)]



weight.dt.list <- list()
for(i in 1:10){
  set.seed(i)
  test.fold = i
  datatrain <-binary.input.data[fold != test.fold]
  datatest <- binary.input.data[fold == test.fold]
  ord_y_train <- datatrain$BinaryETL
  ord_y_test <- datatest$BinaryETL
  train <- as.matrix(datatrain[,-c(98,99)])
  test <-  as.matrix(datatest[,-c(98,99)])
  
  ordnet <- ordinalNetTune(train,ord_y_train)
  bestLambdaIndex <- which.max(rowMeans(ordnet$loglik))
  summary(ordnet)
  
  # x = as.matrix(datatrain[,input.cols])
  # y = datatrain[,11 ]
  # set.seed(1011)
  # cvob1 = cv.glmnet(x, y)
  weight.dt.list[[i]] <- data.table(
    i,
    row.name=colnames(ordnet$fit$coefs),
    weight=as.vector(ordnet$fit$coefs[bestLambdaIndex,]))
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
