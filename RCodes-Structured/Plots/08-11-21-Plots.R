periods <- c("Aug17-Feb18"  ,"Mar18-July18" , "Aug18-Feb19", "Mar19-July19" , 
             "Aug19-Feb20" , "Mar20-July20" , "Aug20-Feb21","Mar21-July21" 
             , "Aug21-Feb22")

months <-c("Jan","Feb","Mar",
               "Apr","May","Jun",
               "Jul","Aug","Sep",
               "Oct","Nov","Dec")

period.mon.data <- matrix(0,nrow = 59, ncol = 12)   
rownames(heat.mon.data) <-ordered.district.count.tracker$State.District.ID

colnames(heat.mon.data) <- c("Jan","Feb","Mar",
                             "Apr","May","Jun",
                             "Jul","Aug","Sep",
                             "Oct","Nov","Dec")

for(i in 1:nrow(summary.dt)){
  date <-summary.dt$Date.of.Event.Visit[i]
  Month <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 1))
  summary.dt$month[i] <- months[Month]
}

segment.period.month <- summary.dt[,c(1,3,4)]
segment.period.month$month <- ""
for(yt in 1:nrow(segment.period.month)){
  date <-summary.dt$Date.of.Event.Visit[yt]
  Month <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 1))
  print(Month)
  segment.period.month$month[yt] <- Month 
}
segment.period.month <- as.data.table(segment.period.month[,c(2,3,4)])
segment.period.month.tracker<- segment.period.month[,(FrequencyOfCoachings=.N), by= c("State.District.ID","period","month")]
segment.period.month.tracker$month <- as.integer(segment.period.month.tracker$month)
segment.period.month.tracker <- segment.period.month.tracker[order(rank(month)),]


colnames(segment.period.month.tracker)[4] <- "FrequencyOfCoachings"


district.period.ord.tracker<- summary.dt[,(noofcoachings=.N), by= c("State.District.ID","period")]




gg.models <- gg +
  facet_grid(State.District.ID ~ .) +
  geom_segment(
    aes(
      x = periods,
      y = months,
      xend = end.pos,
      yend = mean
    ),
    color = "green",
    data = segs.dt
  ) +
  geom_vline(aes(xintercept = changepoint),
             color = "green",
             data = change.dt)


gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(cv.type + test.fold ~ Model, labeller=label_both)+
  geom_line(aes(
    train.size, accuracy.percent, color=selection.disp, group=paste(selection.disp,seed)),
    data=res.both)+
  ylab("Percent correctly predicted intervals")+
  scale_x_log10(
    "Labeled sequences in train set",
    breaks=c(
      range(res.both$train.size),
      20))+
  coord_cartesian(xlim=c(1, 1000), ylim=c(80, 100))
(dl <- directlabels::direct.label(gg, dl.method))
png("figure-random-linear-selection.png", 15, 6, units="in", res=100)
print(dl)
dev.off()






