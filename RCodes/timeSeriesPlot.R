library("lubridate") 
library("ggplot2")
library("data.table")
library("lubridate")
library("dplyr")

required.timeframe.cols <- as.data.table(required.timeframe.cols)

required.timeframe.cols$Duration.of.Event[required.timeframe.cols$Duration.of.Event == "00:60:00"] <- 1
required.timeframe.cols$Duration.of.Event <- gsub(":",".",gsub(":[^:]+$","",required.timeframe.cols$Duration.of.Event)) 
required.timeframe.cols$Duration.of.Event <- gsub("00","0",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- as.numeric(required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$period <- ""
for(i in 1:nrow(required.timeframe.cols)){
  date <-required.timeframe.cols$Date.of.Event.Visit[i]
  print(date)
  Month <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 1))
  Year <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 3))
  if(Month < 3){
    val <- paste0("Aug", Year-1 , "-" , "Feb", Year)
  }else if(Month > 3 & Month < 8){
    val <- paste0("Mar", Year , "-" , "July" , Year)
    print(val)
  }else{
    val <- paste0("Aug", Year , "-" , "Feb" , Year+1)
  }
  required.timeframe.cols$period[i] <- val 
}

required.timeframe.cols <- required.timeframe.cols[order(rank(period, Duration.of.Event))]

dt <- head(required.timeframe.cols,1000)
gg.district.compare <- ggplot(data=dt, aes(x=factor(period),y=Duration.of.Event))+ 
  geom_point() + 
  facet_wrap(~State.District.ID,nrow=95,ncol=2 ,scales="free_x") + geom_hline(yintercept=4.0,linetype="dashed", color = "blue") +
  geom_hline(yintercept=8.0,linetype="dashed", color = "red")

gg.district.compare + theme(axis.text.x = element_text(face="bold", color="#993333", 
                                                       size=10, angle=45),
                            axis.text.y = element_text(face="bold", color="#993333", 
                                                       size=10, angle=45))


gg.district.compare + labs(x = "Academic Year") + labs(y="Duration of the Coaching")


png(filename="~/Desktop/figure-district-timeseries-period.png",2000,5000)
print(gg.district.compare)
dev.off()



required.timeframe.cols.date <- required.timeframe.cols[order(rank(as.numeric(sapply(strsplit(as.character(Date.of.Event.Visit),'/'), "[", 3))), Duration.of.Event)]

dts <- head(required.timeframe.cols.date,100)
gg.district.compares <- ggplot(data=dts, aes(x=Date.of.Event.Visit,y=Duration.of.Event))+ 
  geom_point() + 
  facet_wrap(~State.District.ID,nrow=6,ncol=2 ,scales="free_x") + geom_hline(yintercept=4.0,linetype="dashed", color = "blue") +
  geom_hline(yintercept=8.0,linetype="dashed", color = "red")

gg.district.compares+ theme(axis.text.x = element_text(face="bold", color="#993333", 
                                                       size=10, angle=45),
                            axis.text.y = element_text(face="bold", color="#993333", 
                                                       size=10, angle=45))

ggplot(data=dts, aes(x=Date.of.Event.Visit,y=Duration.of.Event))+ 
  geom_point() +
  facet_grid(~State.District.ID, scales='free_x', space='free_x', labeller=label_wrap_gen(width = 10, multi_line = TRUE)) +
  theme(strip.text.x=element_text(angle=90, hjust=0.5, vjust=0.5))


gg.district.compares + labs(x = "Academic Year") + labs(y="Duration of the Coaching")


png(filename="~/Desktop/figure-district-timeseries-date.png",2000,10000)
print(gg.district.compares)
dev.off()

forcheck <- required.timeframe.cols
forcheck$Duration.of.Event[forcheck$Duration.of.Event == "00:60:00"] <- 1
forcheck$Duration.of.Event <- gsub(":",".",gsub(":[^:]+$","",forcheck$Duration.of.Event)) 
forcheck$Duration.of.Event <- gsub("00","0",forcheck$Duration.of.Event)
forcheck$Duration.of.Event <- as.numeric(forcheck$Duration.of.Event)


forcheck$period <- ""
for(i in 1:nrow(forcheck)){
  date <-forcheck$Date.of.Event.Visit[i]
  print(date)
  Month <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 1))
  Year <- as.numeric(sapply(strsplit(as.character(date),'/'), "[", 3))
  if(Month < 3){
    val <- paste0("Aug", Year-1 , "-" , "Feb", Year)
  }else if(Month > 3 & Month < 8){
    val <- paste0("Mar", Year , "-" , "July" , Year)
    print(val)
  }else{
    val <- paste0("Aug", Year , "-" , "Feb" , Year+1)
  }
  forcheck$period[i] <- val 
}
kt <- as.data.table(kt)
kt <- kt[order(rank(Date.of.Event.Visit, Duration.of.Event))]
kt <- head(forcheck,5924)

mt <- head(kt,10)
gg.district.compared <- ggplot(data=kt, aes(x=Date.of.Event.Visit,y=Duration.of.Event))+ 
  geom_point() + 
  facet_wrap(~State.District.ID,scales="free_x")+ geom_hline(yintercept=4.0,linetype="dashed", color = "blue") +
  geom_hline(yintercept=8.0,linetype="dashed", color = "red") +
  labs(x = "Academic Year") + labs(y="Duration of the Coaching") + theme_classic()


png(filename="~/Documents/figure-classic_sorted_border.png",2000,10000)
print(gg.district.compared)
dev.off()


academic_year <- factor(kt$period,levels)
levels <- c( "Mar1-July1", "Aug207-Feb208" , "Aug17-Feb18"  ,"Mar18-July18" ,
             "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
             "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22")


mt <- head(kt,10) 
gg.district.compared <- ggplot(data=kt, aes(x=academic_year,y=Duration.of.Event,color=factor(period)))+ 
  geom_point() + 
  facet_wrap(~State.District.ID,scales="free_x")+ geom_hline(yintercept=4.0,linetype="dashed", color = "blue") +
  geom_hline(yintercept=8.0,linetype="dashed", color = "red") +
  labs(x = "Academic Year") + labs(y="Duration of the Coaching") +  theme_linedraw()


png(filename="~/Desktop/figure_district_timeseries-date_change.png",2000,10000)
print(gg.district.compared)
dev.off()

#create factor with seond agru

# heat map -> easy understanding of trend for every district over years
# for one district1 <- full, half <- y -axis, x-axis<- years
## year1
## year2
#time over period
# summary.dt <- required.dt[,(noofcoachings=.N), by(year,duration)] y - duration , x - years fill = no of coaching
# geom-tile




library(RColorBrewer)

heatmap(kt, scale="column", col = cm.colors(256))
heatmap(data, scale="column", col = terrain.colors(256))

coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(data, scale="column", col = coul)

# heatmap(data)
# summary.dt <- kt.dt[,(noofcoachings=.N), by= c("Date.of.Event.Visit","Duration.of.Event")]
# #y - duration , x - years fill = no of coaching


cv <- ggplot(kt, aes(period, State.District.ID  , fill = State.District.ID)) +
  geom_raster(hjust = 0, vjust = 0)


kt.dt

write.csv(kt.dt,"/Users/akhilachowdarykolla/Desktop/periodtable.csv", row.names = FALSE)


ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = value), color = "white", size = 4) +
  coord_fixed()



