library("party")
library("partykit")

decision.binary.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/factored_combined_input_data.csv")
head(decision.binary.input.data)

decision.binary.input.data <- decision.binary.input.data[,-c(1,2,102)]
# decision.binary.input.data$ETL.AVERAGE <- ceiling(decision.binary.input.data$ETL.AVERAGE)

decision.binary.input.data$ETLBin <- ""
# for(i in 1:nrow(decision.binary.input.data)) {
#   row <- decision.binary.input.data[i,]
#   etlScore <- row$ETL.AVERAGE
#   print(etlScore)
#   if (etlScore >= 0 && etlScore <= 2.8) {
#     decision.binary.input.data[i,]$ETLBin <- "no"
#   } else{
#     decision.binary.input.data[i,]$ETLBin <- "yes"
#   }
# }

decision.binary.input.data$ETLBin <- as.factor(ifelse(decision.binary.input.data$ETL.AVERAGE>=2.9,"yes","no"))

write.csv(decision.binary.input.data,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/decision_boolean_combined_binary.input_data.csv", row.names = FALSE)


decision.binary.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/decision_boolean_combined_binary.input_data.csv")
head(decision.binary.input.data)
decision.binary.input.data <- decision.binary.input.data[,-c(1,2,17)]

decision.binary.input.data$ETLBin <- as.factor(decision.binary.input.data$ETLBin)
accuracy.dt<- list()

pdf("~/Desktop/DecisionTrees.pdf",width = 8, height= 10)
par(mfrow=c(5,2))
par(mar = c(3,2,1.5,1))
par(omi=rep(.3,4))
# plots <- list()
for(i in 1:split){
  set.seed(i)
  print(i)
  test.fold = i
  datatrain <-decision.binary.input.data[fold != test.fold]
  datatest <- decision.binary.input.data[fold == test.fold]
  
  datatrain <- (datatrain[,-c(96)])
  datatest <-  (datatest[,-c(96)])
  
  party_tree = ctree(ETLBin~., datatrain)
  try_pred = (predict(party_tree, datatest, type = "response"))
 #png(filename=paste0("~/Desktop/",test.fold,"_decision_tree.png"),1000,1000)
  # p1 = plot(party_tree)
  # plots[[i]] <-
  plot(party_tree)
  #dev.off()
  # 
  misclassfication.error=  mean(try_pred !=datatest$ETLBin) * 100
  accuracy.dt[[i]] <- data.table(fold=i,accuracies =misclassfication.error )
  # table(try_pred ,datatest$ETLBin)
}
 dev.off()
#multiplot(plotlist = plots, cols = 2)

# plots <- list()  # new empty list
# for (i in 1:6) {
#   p1 = qplot(1:10, rnorm(10), main = i)
#   plots[[i]] <- p1  # add each plot into plot list
# }
# multiplot(plotlist = plots, cols = 3)

