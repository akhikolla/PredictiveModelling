library(data.table)
library(glmnet)
library(ggplot2)
weight.dt.list <- list()
for(seed in 1:10){
  set.seed(seed)
  n = 1000
  p = 100
  nzc = trunc(p/10)
  x = matrix(rnorm(n * p), n, p)
  beta = rnorm(nzc)
  fx = x[, seq(nzc)] %*% beta
  eps = rnorm(n) * 5
  y = drop(fx + eps)
  px = exp(fx)
  px = px/(1 + px)
  ly = rbinom(n = length(px), prob = px, size = 1)
  set.seed(1011)
  cvob1 = cv.glmnet(x, y)
  weight.dt.list[[seed]] <- data.table(
    seed,
    var.name=rownames(coef(cvob1))[-1],
    weight=coef(cvob1)[-1])
}
weight.dt <- do.call(rbind, weight.dt.list)

ggplot()+
  geom_point(aes(
    weight, var.name),
    data=weight.dt)

weight.dt[, n.nonzero := sum(weight != 0), by=var.name]

ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weight, var.name),
    data=weight.dt)

##family=multinomial.
n = 500
p = 30
nzc = trunc(p/10)
x = matrix(rnorm(n * p), n, p)
beta3 = matrix(rnorm(30), 10, 3)
beta3 = rbind(beta3, matrix(0, p - 10, 3))
f3 = x %*% beta3
p3 = exp(f3)
p3 = p3/apply(p3, 1, sum)
g3 = glmnet:::rmult(p3)
set.seed(10101)
cvfit = cv.glmnet(x, g3, family = "multinomial")
pred.result <- predict(cvfit, x)
str(pred.result)
head(predict(cvfit, x, type="class"))
head(predict(cvfit, x, type="link"))
head(predict(cvfit, x, type="response"))#probabilty

levels <- c("Very Negative", "Negative", "Neutral", "Positive", "Very Positive")
some.data <- c("Very Negative", "Positive")
factor(some.data) #wrong factor levels ordering.
f <- factor(some.data, levels) #use levels to indicate correct ordering.
as.integer(f) # convert factor to integer
(levels2int <- structure(seq_along(levels), names=levels))
levels2int[paste(f)]
