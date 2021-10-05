library("ordinalForest")
set.seed(123)

#/Users/akhilachowdarykolla/Desktop/ReducedCoaching+CWISUpdated+NCES.csv
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

levels <- c("Very negative","Negative","Neutral","Positive","Very positive")
input.data$FactorOrdinalETL <- factor(input.data$OrdinalETL,levels)
head(input.data)
str(input.data)

# as.integer(input.data$FactorOrdinalETL) to get the value as integer
# structure(seq_Along(levels),names= levels)

trainind <- sort(sample(1:nrow(input.data), size=floor(nrow(input.data)*(2/3))))
testind <- setdiff(1:nrow(input.data), trainind)
datatrain <- input.data[trainind,]
datatrain <- datatrain[,-c(38,27)]
datatest <- input.data[testind,]

ordforres <- ordfor(depvar="FactorOrdinalETL", data=datatrain, nsets=1000, ntreeperdiv=100,
                    ntreefinal=5000, perffunction = "probability")
ordforres

sort(ordforres$varimp, decreasing=TRUE)

boxplot(datatrain$common_practices_state_criteria ~ datatrain$FactorOrdinalETL, horizontal=TRUE)
fisher.test(table(datatrain$common_practices_state_criteria, datatrain$FactorOrdinalETL),simulate.p.value=TRUE)

freq <-as.data.frame(table(datatrain$FactorOrdinalETL))
preds <- predict(ordforres, newdata=datatest)
preds
one.pred <- function(x)rep(x, nrow(datatest))
baseline_prediction <- one.pred("Very positive")
mean(baseline_prediction==preds$ypred)*100

median.values <- sort(datatrain$FactorOrdinalETL)
median.ind.val <- median.values[floor(length(median.values)/2)]
L1_prediction <- one.pred(median.ind.val)
mean(L1_prediction==preds$ypred)*100

mean.values <- datatrain$FactorOrdinalETL
mean.ind.val <- mean.values[floor(length(mean.values)/5)+1]
L2_prediction <- one.pred(mean.ind.val)
mean(L2_prediction==preds$ypred)*100

algorithm = c("ordinalForest","baseline_0","L1","L2")
value = c(0.9427526*100,mean(baseline_prediction==preds$ypred)*100,mean(L1_prediction==preds$ypred)*100,mean(L2_prediction==preds$ypred)*100)

ggplot() +
  geom_point(mapping = aes(x = value, y = algorithm, color = algorithm)) 



perff_equal(ytest=datatest$FactorOrdinalETL, ytestpred=preds$ypred)
perff_proportional(ytest=datatest$FactorOrdinalETL, ytestpred=preds$ypred)
perff_oneclass(ytest=datatest$FactorOrdinalETL, ytestpred=preds$ypred)
perff_custom(ytest=datatest$FactorOrdinalETL, ytestpred=preds$ypred)

cm = as.matrix(table(data.frame(true_values=datatest$FactorOrdinalETL, predictions=preds$ypred)))

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
s = matrix(0, nrow = 2, ncol = 2)
accuracy = sum(diag) / n

head(preds$classprobs)
head(preds$ypred)

confusionMatrix(data = datatest$FactorOrdinalETL, reference = preds$ypred)


  
 

library(cvms)
library(tibble)   # tibble()

set.seed(1)

conf_mat <- confusion_matrix(targets = datatest$FactorOrdinalETL,
                             predictions = preds$ypred)


#plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]])


plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]],
                      palette = "Greens")
countlist = list()
for(i in 1:8516) {
  print("In stage")
  print(preds$ypred[i])
  print("leveles")
  print(datatest$FactorOrdinalETL[i])
  print("--------")
}
data(hearth)
# Inspect the data:
table(hearth$Class)
dim(hearth)
head(hearth)

# Split into training dataset and test dataset:
set.seed(123)
trainind <- sort(sample(1:nrow(hearth), size=floor(nrow(hearth)*(2/3))))
testind <- setdiff(1:nrow(hearth), trainind)
datatrain <- hearth[trainind,]
datatest <- hearth[testind,]
# Construct OF prediction rule using the training dataset (default
# perffunction = "probability" corresponding to the
# (negative) ranked probability score as performance function):
ordforres <- ordfor(depvar="FactorOrdinalETL", data=datatrain, nsets=1000, ntreeperdiv=100,
                    ntreefinal=5000, perffunction = "equal")
ordforres

# ordinal net
library("ordinalNet")
x <- as.matrix(datatrain)
y <- datatrain$FactorOrdinalETL
fit1 <- ordinalNet(x, y, family="cumulative", link="logit",
                     parallelTerms=TRUE, nonparallelTerms=FALSE)
summary(fit1)
coef(fit1)
coef(fit1, matrix=TRUE)

if(!require(devtools)) 
  install.packages("devtools")
devtools::install_github('jbryer/likert')




  
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
                                       stringsAsFactors = FALSE)
  
  input.cols <- c("Collaborativeteams","Commonformativeassessment","Databaseddecisionmaking",
                  "CWIS","common_practices_can_statements",
                  "common_practices_student_work", "common_practices_self_assessment","common_practices_receive_feedback",
                  "common_practices_student_feedback","common_practices_state_criteria")
  
  