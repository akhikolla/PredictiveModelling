rf.binary.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/PredictiveModelling/data/binary_factored_combined_binary.input_data.csv")
head(rf.binary.input.data)
rf.binary.input.data <- rf.binary.input.data[,-c(1,2,19)]
rf.binary.input.data$BinaryETL = as.factor(rf.binary.input.data$BinaryETL)


require(randomForest)
fit=randomForest(BinaryETL~., data=rf.binary.input.data)
(VI_F=importance(fit))

library(caret)
varImp(fit)

png(filename="/Users/akhilachowdarykolla/Documents/PredictiveModelling/figures/23NovRFImportance.png",500,700)
print(varImpPlot(fit,type=2))
dev.off()


library(randomForest)
set.seed(71)
rf <-randomForest(BinaryETL~.,data=rf.binary.input.data, ntree=500) 
print(rf)

floor(sqrt(ncol(rf.binary.input.data) - 1))

mtry <- tuneRF(rf.binary.input.data[-1],rf.binary.input.data$BinaryETL, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

mtry <- tuneRF(rf.binary.input.data[-1],rf.binary.input.data$BinaryETL, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

