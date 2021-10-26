
gg <- ggplot()+
  ggtitle("check if train AUM decreases")+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  geom_line(aes(
    Date.of.Event.Visit, Duration.of.Event,
    group=paste(State.District.ID,gsub("-.*","",Date.of.Event.Visit) )),
    data=required.timeframe.cols)+
  facet_grid(State.District.ID + gsub("-.*","",Date.of.Event.Visit) ~ ., scales="free", labeller=label_both)
png(
  "figure-linear-model-test-aum-train-decreases.png",
  width=4, height=35, res=100, units="in")
print(gg)
dev.off()


par(mar = c(2,2,2,2))
par(mfrow=c(4,3))

for(i in 1:nrow(required.timeframe.cols)){
  plot(required.timeframe.cols$Date.of.Event.Visit, required.timeframe.cols$Duration.of.Event)
}









