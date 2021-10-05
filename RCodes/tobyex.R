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
    var.names=rownames(coef(cvob1))[-1],
    weights=coef(cvob1)[-1])
}
weightz.dt <- do.call(rbind, weight.dt.list)

ggplot()+
  geom_point(aes(
    weights, var.names),
    data=weight.dt)

weightz.dt[, n.nonzero := sum(weights != 0), by=var.names]

ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weights, var.names),
    data=weight.dt)