library(tidyverse)
library(caret)
library("datarium")
library(lars)

trainind <- sort(sample(1:nrow(folds.demographic.or.community.factors), size=floor(nrow(folds.demographic.or.community.factors)*(3/4))))
testind <- setdiff(1:nrow(folds.demographic.or.community.factors), trainind)
datatrain <- folds.demographic.or.community.factors[trainind,]
datatest <- folds.demographic.or.community.factors[testind,]
y_obs =  datatest[,24]
x_train <- datatrain[,-c(24,25)]
x_test <- as.matrix(datatest[,-c(24,25)])
x = as.matrix(x_train)
y = datatrain[,24]


fit <- lars(x,y,type="lasso")
fit$lambda


pred.nox <- predict(fit, type="coef")
beta <- scale(pred.nox$coefficients, FALSE, 1/fit$normx)
arclength <- rowSums(abs(beta))
path.list <- list()
for(variable in colnames(beta)){
  standardized.coef <- beta[, variable]
  path.list[[variable]] <- data.table::data.table(
    step=seq_along(standardized.coef),
    lambda=0.01859,#c(fit$lambda, 0),
    variable,
    standardized.coef,
    fraction=pred.nox$fraction,
    arclength)
}
path <- do.call(rbind, path.list)
variable.colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
  "#A65628", "#F781BF", "#999999","#FFF00F",
  "#DFFF00","#CD5C5C","#FF5733","#6495ED","#40E0D0","#DE3163","#FF7F50","#FFBF00","#E9967A","#F08080",
  "#000000","#FFFFFF","#FF0000","#0000FF","#FFFF00","#00FFFF","#FFA07A","#800080","#CCCCFF")

library("ggplot2")
gg.lambda <- ggplot()+
  theme_bw()+
  theme(panel.spacing =grid::unit(0, "lines"))+
  scale_color_manual(values=variable.colors)+
  geom_line(aes(
    lambda, standardized.coef, color=variable, group=variable),
    data=path)+
  ggtitle("LASSO path for ETL learning calculated using the LARS")

gg.lambda

x.scaled <- with(fit, scale(x, meanx, normx))
lfit <- lm.fit(x.scaled, y)
library(data.table)
lpoints <- data.table::data.table(
  variable=colnames(x),
  standardized.coef=lfit$coefficients,
  arclength=sum(abs(lfit$coefficients)))

gg.lambda+
  geom_point(aes(
    0, standardized.coef, color=variable),
    data=lpoints)



fraction <- sort(unique(c(
  seq(0, 1, l=21))))
pred.fraction <- predict(
  fit, input.columns,
  type="coef", mode="fraction", s=fraction)
coef.grid.list <- list()
coef.grid.mat <- scale(pred.fraction$coefficients, FALSE, 1/fit$normx)
for(fraction.i in seq_along(fraction)){
  standardized.coef <- coef.grid.mat[fraction.i,]
  coef.grid.list[[fraction.i]] <- data.table::data.table(
    fraction=fraction[[fraction.i]],
    variable=colnames(x),
    standardized.coef,
    arclength=sum(abs(standardized.coef)))
}
coef.grid <- do.call(rbind, coef.grid.list)
ggplot()+
  ggtitle("LASSO path for ETL learning calculated using the LARS")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_color_manual(values=variable.colors)+
  geom_line(aes(
    arclength, standardized.coef, color=variable, group=variable),
    data=path)+
  geom_point(aes(
    arclength, standardized.coef, color=variable),
    data=lpoints)+
  geom_point(aes(
    arclength, standardized.coef, color=variable),
    shape=10,
    fill=NA,
    size=2,
    data=coef.grid)



## Test Train Error Plot

pred.list <- predict(
  fit, x_test,
  mode="fraction", s=fraction)
residual.mat <- pred.list$fit - y_obs
squares.mat <- residual.mat * residual.mat
mean.error.list <- list()



# train.n <- 20439
# train.set <- rep(TRUE, train.n)
# train.set
# 
# test.n <- 5108
# validation.set <- rep(FALSE, test.n)
# validation.set


# set = "train"
# mse <- colMeans(squares.mat[train.set, ])
# mean.error.list[[paste(set)]] <- data.table::data.table(
#     set, mse, fraction,
#     arclength=rowSums(abs(coef.grid.mat)))
# 
# sets = "validation"
# mse <- colMeans(squares.mat[validation.set, ])
# print(mse)
# mean.error.list[[paste(sets)]] <- data.table::data.table(
#   sets, mse, fraction,
#   arclength=rowSums(abs(coef.grid.mat)))

for(set in c("train", "validation")){
  val <- if(set=="train")TRUE else FALSE
  is.set <- trainind == val
  # print(is.set)
  mse <- colMeans(squares.mat[is.set, ],na.rm = TRUE)
  print(mse)
  mean.error.list[[paste(set)]] <- data.table::data.table(
    set, mse, fraction,
    arclength=rowSums(abs(coef.grid.mat)))
}

for(set in c("train", "validation")){
  val <- if(set=="train")TRUE else FALSE
  is.set <- trainind == val
  # print(is.set)
  mse <- colMeans(squares.mat[is.set, ],na.rm = TRUE)
  print(mse)
  mean.error.list[[paste(set)]] <- data.table::data.table(
    set, mse, fraction,
    arclength=rowSums(abs(coef.grid.mat)))
}


mean.error <- do.call(rbind, mean.error.list)
rect.width <- diff(mean.error$arclength[1:2])/2
addY <- function(dt, y){
  data.table::data.table(dt, y.var=factor(y, c("error", "weights")))
}
# tallrect.dt <- coef.grid[variable==variable[1],]
gg.path <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(y.var ~ ., scales="free")+
  ylab("")+
  scale_color_manual(values=variable.colors)+
  geom_line(aes(
    arclength, standardized.coef, color=variable, group=variable),
    data=addY(path, "weights"))+
  geom_line(aes(
    arclength, mse, linetype=set, group=set),
    data=addY(mean.error, "error"))
# geom_tallrect(aes(
#   xmin=arclength-rect.width,
#   xmax=arclength+rect.width),
#   clickSelects="arclength",
#   alpha=0.5,
#   data=tallrect.dt)
print(gg.path)
directlabels::direct.label(gg.path+xlim(0, 400))

