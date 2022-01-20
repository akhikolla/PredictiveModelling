source("packages.R")

baseline.csv.vec <- Sys.glob(
  "~/Documents/Coding/development/PredictiveModelling/neuroblastoma-data/data/systematic/cv/*/testFolds/*/randomTrainOrderings/*/baseline.csv")

result.list <- list()
for(csv.i in seq_along(baseline.csv.vec)){
  baseline.csv <- baseline.csv.vec[[csv.i]]
  seed.dir <- dirname(baseline.csv)
  seed <- as.integer(basename(seed.dir))
  split.dir <- dirname(dirname(seed.dir))
  test.fold <- as.integer(basename(split.dir))
  cv.type.dir <- dirname(dirname(split.dir))
  dt <- fread(baseline.csv)
  result.list[[csv.i]] <- data.table(
    seed, test.fold, cv.type=basename(cv.type.dir), dt)
}
result <- do.call(rbind, result.list)

result <- segment.period.month.tracker
#result$month <- as.integer(result$month)

most.train <- result[, {
  .SD[train.size==max(train.size), list(
    min.accuracy=min(accuracy.percent),
    max.accuracy=max(accuracy.percent)
  )]
}, by=list(cv.type, test.fold, model)]
(bad <- most.train[min.accuracy != max.accuracy])
if(nrow(bad)){
  print(result[bad, on=list(cv.type, test.fold, model)][train.size==max(train.size)])
  stop("some accuracy numbers not equal")
}

if(FALSE){
  unlink(Sys.glob(
    "data/systematic/cv/profileSize/testFolds/2/randomTrainOrderings/*/baseline.csv"))
}

ymin <- 1
png.width <- 100
xmax <- 170
result <- tail(result,50)
max.dt <- tail(result[, list(max.train=max(FrequencyOfCoachings)), by=list(State.District.ID, period)])

max.dt <- (result[, list(max.train=max(month)), by=list(State.District.ID, period)])

max.color <- "grey"
# break.vec <- c(
#   min(result$train.size),
#   10^(1:3))

break.vec <- c(1,2,3,4,5,6,7,8,9,10,11,12)
#dl.method <- list(cex=0.7, "last.polygons")

ggplot(segment.period.month.tracker, aes(x=factor(month,levels=1:12), y=FrequencyOfCoachings))+
  geom_point(stat='identity')+
  scale_x_discrete('month', breaks=factor(1:12), drop=FALSE)

ggks <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(State.District.ID ~ period, labeller=label_both,scales = "free")+
  geom_segment(aes(
    max.train, Inf,
    xend=max.train, yend=ymin),
    color=max.color,
    data=max.dt)+ 
  geom_text(aes(
    max.train, ymin, label=paste0(" ", max.train)),
    color=max.color,
    hjust=0,
    vjust=0,
    data=max.dt)+
  geom_point(aes(
     FrequencyOfCoachings,month),
    data=result)+
  ylab("Percent correctly predicted intervals")+
  scale_x_log10(
    "Labeled sequences in train set",
    limits=c(NA, xmax), breaks=break.vec)+
  coord_cartesian(ylim=c(ymin, 170))


(dl <- directlabels::direct.label(ggks, dl.method))
png("~/Desktop/strfigure-baseline-lines.png", 50, 50, units="in", res=100)
print(ggks)
dev.off()


segment.period.month.tracker %>%
  ggplot(aes(x = month,y=FrequencyOfCoachings)) + geom_point()



library(ggplot2)
library(dplyr)
library(ggeasy)
library(stringr)
library(patchwork)

View(ggplot2::mpg)

district.count.tracker<- summary.dt[,(noofcoachings=.N), by= c("State.District.ID")]
colnames(district.count.tracker)[2] <- "count"
district.count.tracker <- district.count.tracker[order(rank(-count)),]

View(district.count.tracker)

df <- summary.dt%>%
  dplyr::group_by(State.District.ID)%>%
  dplyr::mutate(State.District.ID = str_to_title(State.District.ID))%>%
  tally()



reorder(df$State.District.ID, df$n)
pl1 <- ggplot(data = df,aes(x= reorder(State.District.ID, n), y = n,fill = State.District.ID))
pl1 <- pl1 + geom_bar(stat = "identity")
pl1 <- pl1 + geom_text(aes(label = n), size = 2, vjust = -0.3)
pl1 <- pl1 + theme_classic() + scale_fill_viridis_d()
pl1 <- pl1 + theme(legend.position = "none")
pl1 <- pl1 + ggeasy::easy_rotate_labels(which = "x", angle = 90)
pl1 <- pl1 + labs(x = "Car Manufacturers", y= "Number of Models")
pl1 <- pl1 + labs(title = "Models from each manufacturer")
pl1 <- pl1 + labs(subtitle = "Shown in ascending order of  number of models")
pl1

png(filename="~/Desktop/figure_histogram.png",5000,10000)
print(pl1)
dev.off()



data(BinomialExample)
x <- BinomialExample$x; y <- BinomialExample$y
fit2 = glmnet(x[train,], y[train], family = "binomial")
assess.glmnet(fit2,newx = x[-train,], newy=y[-train], s=0.1)
plot(roc.glmnet(fit2, newx = x[-train,], newy=y[-train])[[10]])
fit2c = cv.glmnet(x, y, family = "binomial", keep=TRUE)
idmin = match(fit2c$lambda.min, fit2c$lambda)
plot(roc.glmnet(fit2c$fit.preval, newy = y)[[idmin]])

train = sample(seq(length(y)),70,replace=FALSE)



  
