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


decision.binary.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/PredictiveModelling/data/decision_boolean_combined_binary.input_data.csv")
head(decision.binary.input.data)
decision.binary.input.data <- decision.binary.input.data[,-c(1,2,17)]

decision.binary.input.data$ETLBin <- as.factor(decision.binary.input.data$ETLBin)
accuracy.dt<- list()

fmBH <- mob(ETLBin ~ lstat + rm | zn + indus + chas + nox + age +
              dis + rad + tax + crim + b + ptratio,
            control = mob_control(minsplit = 40), data = BostonHousing,
            model = linearModel)


Data.based.decision.making
party_tree = ctree(ETLBin~ Data.based.decision.making + Effective.teaching.learning.practices + Instructional.Leadership +  School.based.implementation.coaching, datatrain)
try_pred = (predict(party_tree, datatest, type = "response"))

gtree <- ctree(ETLBin ~ ., data = datatrain)
prob <- predict(gtree, type = "prob")[,1] +
  runif(nrow(datatrain), min = -0.01, max = 0.01)
splitvar <- character_split(split_node(node_party(gtree)),data = data_party(gtree))$name

plot(datatrain[[splitvar]], prob,
       pch = as.numeric(datatrain$ETLBin), ylab = "Conditional Class Prob.",
       xlab = splitvar)

abline(v = split_node(node_party(gtree))$breaks, lty = 2)
legend(0.15, 0.7, pch = 1:2, legend = levels(datatrain$ETLBin), bty = "n")


plot(datatrain$common_practices_student_use_cfa, datatrain$CFA.AVERAGE, pch=21, 
     bg=c("red","green3","blue")[unclass(datatrain$ETLBin)], 
     main="Edgar Anderson's Iris Data")
lines(c(0,6),c(1.75,1.75)) # manually put split criterions
lines(c(4.95,4.95),c(0,3))  # manually put split criterions



install.packages("tree")
library(tree)
tree1 <- tree(ETLBin ~ CFA.AVERAGE + common_practices_student_use_cfa, data = datatrain)
summary(tree1)
png(filename=paste0("~/Desktop/selectedattrs_decision_tree_figure.png"),1000,1000)
plot(tree1)
text(tree1)
dev.off()

plot(datatrain$CFA.AVERAGE,datatrain$common_practices_student_use_cfa,pch=19,col=as.numeric(datatrain$ETLBin))
partition.tree(tree1,label="ETLBin",add=TRUE)
legend(1.75,4.5,legend=unique(datatrain$ETLBin),col=unique(as.numeric(datatrain$ETLBin)),pch=19)

plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree1,label="Species",add=TRUE)
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

library(parsnip)
library(parttree)
library(titanic) ## Just for a different data set
set.seed(123) ## For consistent jitter
titanic_train$Survived = as.factor(titanic_train$Survived)
## Build our tree using parsnip (but with rpart as the model engine)
ti_tree =
  decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification") %>%
  fit(ETLBin ~ collab_teams_data_collaboration   + common_practices_student_use_cfa , data = datatrain)
## Plot the data and model partitions
datatrain %>%
  ggplot(aes(y=collab_teams_data_collaboration  , x=common_practices_student_use_cfa)) +
  geom_jitter(aes(col=ETLBin), alpha=0.7) +
  geom_parttree(data = ti_tree, aes(fill=ETLBin), alpha = 0.1) +
  theme_minimal()





pdf("~/Desktop/DecisionTrees.pdf",width = 8, height= 10)

png("~/Desktop/DecisionTrees.png",width = 1000, height= 1000)
par(mfrow=c(3,2))
par(mar = c(3,2,1.5,1))
par(omi=rep(.3,4))
# plots <- list()
for(i in 1:10){
  set.seed(i)
  print(i)
  test.fold = i
  datatrain <-decision.binary.input.data[fold != test.fold]
  datatest <- decision.binary.input.data[fold == test.fold]
  
  datatrain <- (datatrain[,-c(96)])
  datatest <-  (datatest[,-c(96)])
  
  party_tree = ctree(ETLBin~., datatrain)
  try_pred = (predict(party_tree, datatest, type = "response"))
  
  prob <- predict(party_tree, type = "prob")[,1] + runif(nrow(datatrain), min = -0.01, max = 0.01)
  splitvar <- character_split(split_node(node_party(party_tree)), data = data_party(party_tree))$name
  plot(datatrain[[splitvar]], prob,
       pch = as.numeric(datatrain$Class), ylab = "Conditional Class Prob.",
       xlab = splitvar)
  
  abline(v = split_node(node_party(gtree))$breaks, lty = 2)
  legend(0.15, 0.7, pch = 1:2, legend = levels(GlaucomaM$Class), bty = "n")
  
 png(filename=paste0("~/Desktop/14-9_",test.fold,"_decision_tree.png"),1000,1000)
 
 fit <- mob(ETLBin~Data.based.decision.making + Effective.teaching.learning.practices | Instructional.Leadership + School.based.implementation.coaching 
            + Collective.teacher.efficacy +  Practice.profiles + Self.assessment.practice.profile...SAPP. + Learning.module.materials..i.e..power.points..handouts. ,
            control = mob_control(minsplit = 40), data = datatrain,model = linearModel)
  # p1 = plot(party_tree)
  # plots[[i]] <-
 
 node_scatterplot(party_tree, which = NULL, col = "black",
                  linecol = "red", cex = 0.5, pch = NULL, jitter = FALSE,
                  xscale = NULL, yscale = NULL, ylines = 1.5, id = TRUE,
                  labels = FALSE)
  plot(party_tree)
  dev.off()
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

