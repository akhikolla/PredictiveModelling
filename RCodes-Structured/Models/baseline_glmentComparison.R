final.accuracy.list <- list()
accuracy.dt <- list()
for(i in 1:10){
  set.seed(i)
  print(i)
  fold.i<- folds.input.data.updatedTypes[folds.input.data.updatedTypes$fold == i,]
  trainind <- sort(sample(1:nrow(fold.i), size=floor(nrow(fold.i)*(3/4))))
  testind <- setdiff(1:nrow(fold.i), trainind)
  datatrain <- fold.i[trainind,]
  datatest <- fold.i[testind,]
  y_obs = datatest[,11 ]#round(datatest[,11 ], digits=1) 
  
  x = as.matrix(datatrain[,input.cols])
  y = datatrain[,11 ]

  one.pred <- function(x)rep(x, nrow(datatest))
  freq <-as.data.frame(table(y))
  median.values <- sort(y)
  median.ind.val <- median(median.values)
  mean.ind.val <- mean(y)
  cv.fit.gaussian <- cv.glmnet(x,y)# family = "multinomial"
  
  predictions.list <-  list(
    glmnet.gaussian=as.numeric(predict(cv.fit.gaussian,newx=as.matrix(datatest[,input.cols]),s=cv.fit.gaussian$lambda.1se,type="response")),
    baseline.l0=one.pred(as.numeric.factor(freq[which.max(freq$Freq),]$y)),
    baseline.l1=one.pred(median.ind.val),
    baseline.l2=one.pred(mean.ind.val))
  
  accuracy.dt.list <- list()
  for(algo in names(predictions.list)){
    print(algo)
    pred.vec = predictions.list[[algo]]
    print(accuracy(pred.vec, y_obs)*100)
    d = y_obs - pred.vec
    accuracy.dt.list[[algo]] <- data.table(
      algo.name = algo,
      accuracy= 1-(sum((d)^2)/sum((y_obs-mean(y_obs))^2))
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

# temp_plot = ggplot() +
#   geom_point(mapping = aes(x = , y = algorithm, color = algorithm)) 
# }

#par(new=TRUE)
# ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10)
# }






