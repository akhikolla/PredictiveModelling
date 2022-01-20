pro.dt <- segment.period.month.tracker
one.sequence <- pro.dt[period == "Aug17-Feb18"]
one.sequence
one.sequence$month <- as.integer(one.sequence$month)
one.sequence.order <- one.sequence[order(rank(month)),]

ggplot() +
  geom_point(aes(x = month, y = FrequencyOfCoachings), data = one.sequence.order) + scales

#+coord_cartesian(xlim =c(0, 13), ylim = c(0, 150))

#+ expand_limits(x=c(1,12), y =c(0,170))
  
  #coord_cartesian(xlim =c(1, 12), ylim = c(0, 150))

#234 data points are present on this sequence

#Answer 2

ggplot(one.sequence.order, aes(x=factor(month,levels=1:12), y=FrequencyOfCoachings))+
  geom_point(stat='identity')+
  scale_x_discrete('month', breaks=factor(1:12), drop=FALSE)

Kmax <- 12
models <- binsegRcpp::binseg_normal(one.sequence.order$FrequencyOfCoachings, Kmax)

ggplot() +
  geom_point(aes(segments, loss),
             data = models)

ggplot(models, aes(x=factor(segments,levels=1:12), y=loss))+
  geom_point(stat='identity')+
  scale_x_discrete('month', breaks=factor(1:12), drop=FALSE)

data.list <- list()
one.seq.list <- list()

models <-
  binsegRcpp::binseg_normal(one.sequence.order$FrequencyOfCoachings, 12)

segs.dt <- coef(models)
for (col.name in c("start", "end")) {
  col.value <- segs.dt[[col.name]]
  set(segs.dt,
      j = paste0(col.name, ".pos"),
      value = one.sequence.order$month[col.value])
}

segs.dt[, end.before := c(NA, end.pos[-.N]), by = .(segments)]
change.dt <- data.table(one.sequence.order, segs.dt[1 < start])
change.dt[, changepoint := (start.pos + end.before) / 2]



gg <- ggplot(one.sequence.order, aes(x=factor(month,levels=1:12), y=FrequencyOfCoachings))+
  geom_point(stat='identity')+
  scale_x_discrete('month', breaks=factor(1:12), drop=FALSE)

gg.models <- gg +
  facet_grid(segments ~ .) +
  geom_segment(
    aes(
      x = start.pos,
      y = mean,
      xend = end.pos,
      yend = mean
    ),
    color = "green",
    data = segs.dt
  ) +
  geom_vline(aes(xintercept = changepoint),
             color = "green",
             data = change.dt)




