library("lubridate") 
library("ggplot2")
library("data.table")

library(lubridate)
library(dplyr)

coachingdatecheck <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/Coaching logs Fall 2017- Spring 2021/Condensed columns-Table 1.csv")#,header = TRUE,check.names=TRUE,format = "%m/%d/%Y"  )
required.timeframe.cols <- coachingdatecheck[,c(4,5,8)]
head(required.timeframe.cols)
tail(required.timeframe.cols)

required.timeframe.cols$Duration.of.Event[required.timeframe.cols$Duration.of.Event == ""] <- 0

required.timeframe.cols$Duration.of.Event[required.timeframe.cols$Duration.of.Event == "NA"] <- 0
required.timeframe.cols$Duration.of.Event  <- gsub("\\.000","",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event  <- gsub("60:00","1",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event  <-gsub(":",".",gsub(":00","",required.timeframe.cols$Duration.of.Event))

## Minutes Converstion
required.timeframe.cols$Duration.of.Event <- gsub("min","minutes",required.timeframe.cols$Duration.of.Event,fixed = T)
required.timeframe.cols$Duration.of.Event <- gsub("Minutes","minutes",required.timeframe.cols$Duration.of.Event,fixed = T)

required.timeframe.cols$Duration.of.Event <- gsub("mnutes","minutes",required.timeframe.cols$Duration.of.Event,fixed = T)
required.timeframe.cols$Duration.of.Event <- gsub(" minutes ","minutes",required.timeframe.cols$Duration.of.Event,fixed = T)

required.timeframe.cols$Duration.of.Event <- gsub("minutesutes","minutes",required.timeframe.cols$Duration.of.Event,fixed = T)



##HOURS REPLACEMENTS
required.timeframe.cols$Duration.of.Event <- gsub(",",".",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub(" ","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("houra$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hour$"," hours",required.timeframe.cols$Duration.of.Event)
#required.timeframe.cols$Duration.of.Event <- gsub("hour"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hoirs$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("Hours"," hours",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("hrs.$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hrs$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hr$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hr"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("h$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("hh$"," hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("Hrs$"," hours",required.timeframe.cols$Duration.of.Event)


##UNNECCESSARY DATA REMOVAL

required.timeframe.cols$Duration.of.Event <- gsub("s\\(duetolatestartforinclementweather\\)","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sonsite;9 hoursstravel","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sand"," ",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sonsite.4 hoursstravel","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sonsite;3 hoursstravel$","",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("sonsite;3.5 hoursstravel","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub(".inElem.1.5 hours.H.S.","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("1.5=","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("1.25\\(","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("\\)","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("one","1",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("h hours","hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("FullDay","8:00:00",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("allday","8:00:00",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("plus3hourstravelingtime","",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("plus3travelingtime","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("2hoursdrivingtime","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("plus2hourstravelingtime","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("plus2hourstravelingtime$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("2hoursdrivingtime$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("2hourdrivingtime$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("2hourdrivingtime","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub(";2 hoursstravel","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("s;2 hoursstravel","",required.timeframe.cols$Duration.of.Event)


required.timeframe.cols$Duration.of.Event <- gsub("drivetime2hours","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sessionsat1.5eac ","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("sessionsat1.5","hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("120minutes/","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("1/2day","4:00:00",required.timeframe.cols$Duration.of.Event)



required.timeframe.cols$Duration.of.Event <- gsub("l hours$","1 hours",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("i hours","1 hours",required.timeframe.cols$Duration.of.Event)
#required.timeframe.cols$Duration.of.Event <- gsub("^.","0.",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("\\(2days\\)","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("s\\(2days\\)","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("inp.m.","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("Hrs\\(2days","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("s\\(2days","",required.timeframe.cols$Duration.of.Event)


for(tm in 1:nrow(required.timeframe.cols)){
  #CV.Duration.of.Event <- 0
  if(grepl("minutes",required.timeframe.cols$Duration.of.Event[tm])){
    #print(required.timeframe.cols$Duration.of.Event[tm])
    CV.Duration.of.Event <- as.numeric(gsub("minutes","",required.timeframe.cols$Duration.of.Event[tm]))/60
    if(is.na(CV.Duration.of.Event)){
      CV.Duration.of.Event <- gsub("minutes","",required.timeframe.cols$Duration.of.Event[tm])
      CV.Duration.of.Event <- gsub(" ",".",CV.Duration.of.Event)
    }
    required.timeframe.cols$Duration.of.Event[tm] = CV.Duration.of.Event
    print(CV.Duration.of.Event)
  }
}


required.timeframe.cols$Duration.of.Event <- gsub("[hours]+",".",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("[hour]+",".",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("[hourand]+",".",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("\\s$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("\\.+",".",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("\\.$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("\\s$","",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("^\\.","00:",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("^\\:","00:",required.timeframe.cols$Duration.of.Event)





for(tm in 1:nrow(required.timeframe.cols)){
  if(length( strsplit(required.timeframe.cols$Duration.of.Event[tm], ":", fixed=TRUE)[[1]]) == 1){
    if(length( strsplit(required.timeframe.cols$Duration.of.Event[tm], ".", fixed=TRUE)[[1]]) == 1){
      # print("a")
      if(required.timeframe.cols$Duration.of.Event[tm] < 10){
        required.timeframe.cols$Duration.of.Event[tm]<-paste0(required.timeframe.cols$Duration.of.Event[tm],":00:00")
        # print(paste0(required.timeframe.cols$Duration.of.Event[tm],":00:00"))
        # }else{
        #   val <- as.numeric(required.timeframe.cols$Duration.of.Event[tm]) 
        #   print(paste0(val,":00:00"))
        # }
        #required.timeframe.cols$Duration.of.Event[tm] <-paste0(required.timeframe.cols$Duration.of.Event[tm],":00:00")
      }
    }else{
      value <- as.numeric(required.timeframe.cols$Duration.of.Event[tm])
      modifier <- format(round(value, 2), nsmall = 2)
      #print(paste0(modifier))
      # print("sorry although one needs split")
      if(length( strsplit(modifier, ".", fixed=TRUE)[[1]]) > 1){
        hours <- as.numeric(gsub("\\..*","",modifier))
        minutes <- as.numeric(gsub(".*\\.","",modifier))
        if(minutes/60 > 1){
          #  print("b")
          fhours <- as.numeric(gsub("\\..*","",as.character((minutes/60) + hours)))
          fminutes <- gsub(".*\\.","",as.character((minutes/60) + hours))
          required.timeframe.cols$Duration.of.Event[tm] <-paste0(fhours,":",round(as.numeric(fminutes), 2),":00")
          # print(paste0(fhours,":",round(as.numeric(fminutes), 2),":00"))
          # required.timeframe.cols$Duration.of.Event[tm] <-paste0(fhours,":",format(round(as.numeric(fminutes), 2), nsmall = 2),":00")
        }else{
          #print("c")
          required.timeframe.cols$Duration.of.Event[tm] <-paste0(hours,":",round(as.numeric(sub("\\..*","",as.character((minutes)))), 2),":00")
          #print(paste0(hours,":",round(as.numeric(sub("\\..*","",as.character((minutes)))), 2),":00"))
          #required.timeframe.cols$Duration.of.Event[tm] <-paste0(hours,":",format(round(as.numeric(sub("\\..*","",as.character((minutes/60)))), 2), nsmall = 2),":00")
        }
        
      }
    }
  }else if(length( strsplit(required.timeframe.cols$Duration.of.Event[tm], ":", fixed=TRUE)[[1]]) == 2){
    #print("d")
    #print(paste0("00:",required.timeframe.cols$Duration.of.Event[tm]))
    required.timeframe.cols$Duration.of.Event[tm] <- paste0("00:",required.timeframe.cols$Duration.of.Event[tm])
    # required.timeframe.cols$Duration.of.Event[tm] <- paste0("00:",required.timeframe.cols$Duration.of.Event[tm])
  }else{
    #print("e")
    required.timeframe.cols$Duration.of.Event[tm] <- paste0(required.timeframe.cols$Duration.of.Event[tm])
    # print(paste0(required.timeframe.cols$Duration.of.Event[tm]))
    # required.timeframe.cols$Duration.of.Event[tm] <- paste0(required.timeframe.cols$Duration.of.Event[tm])
  }
}


# required.timeframe.cols$Duration.of.Event <- gsub("3","03:00:00",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event <- gsub("6","06:00:00",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event <- gsub("8","08:00:00",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event <- gsub("0","00:00:00",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event <- gsub("2","02:00:00",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event <- gsub("4","04:00:00",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event <- gsub("5","05:00:00",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event <- gsub("45","00:45:00",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event <- gsub("7","07:00:00",required.timeframe.cols$Duration.of.Event)
# required.timeframe.cols$Duration.of.Event <- gsub("10","10:00:00",required.timeframe.cols$Duration.of.Event)

required.timeframe.cols$Duration.of.Event <- gsub("11666666666667","00",required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event <- gsub("42760","00:42:00",required.timeframe.cols$Duration.of.Event)

for(tm in 1:nrow(required.timeframe.cols)){
  if(required.timeframe.cols$Duration.of.Event[tm] == "3"){
    required.timeframe.cols$Duration.of.Event[tm] <- "03:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "6"){
    required.timeframe.cols$Duration.of.Event[tm] <- "06:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "8"){
    required.timeframe.cols$Duration.of.Event[tm] <- "08:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "0"){
    required.timeframe.cols$Duration.of.Event[tm] <- "00:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "2"){
    required.timeframe.cols$Duration.of.Event[tm] <- "02:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "5"){
    required.timeframe.cols$Duration.of.Event[tm] <- "05:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "4"){
    required.timeframe.cols$Duration.of.Event[tm] <- "04:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "45"){
    required.timeframe.cols$Duration.of.Event[tm] <- "00:45:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "7"){
    required.timeframe.cols$Duration.of.Event[tm] <- "07:00:00"
  }else if(required.timeframe.cols$Duration.of.Event[tm] == "10"){
    required.timeframe.cols$Duration.of.Event[tm] <- "10:00:00"
  }
}

unique(required.timeframe.cols$Duration.of.Event)

# cmean_cols <- c("Duration.of.Event")
# required.timeframe.cols[, cmean_cols] <- required.timeframe.cols[, lapply(.SD, as.numeric), .SDcols = cmean_cols]
# cwis.aggregate <- required.timeframe.cols[, lapply(.SD, mean), .SDcols = cmean_cols,by = "State.District.ID"]

required.timeframe.cols$Duration.of.Event  <- hms(required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event
required.timeframe.cols$Duration.of.Event <- period_to_seconds(required.timeframe.cols$Duration.of.Event)
required.timeframe.cols$Duration.of.Event
# Convert time to Period object
# required.timeframe.cols$Date.of.Event.Visit
# required.timeframe.cols$Date.of.Event.Visit <- sum(required.timeframe.cols$Date.of.Event.Visit)            # Calculate sum of seconds
# required.timeframe.cols$Date.of.Event.Visit           

head(required.timeframe.cols$Duration.of.Event)
head(required.timeframe.cols)



required.timeframe.cols$Date.of.Event.Visit <- as.Date(required.timeframe.cols$Date.of.Event.Visit, "%m/%d/%y")
plot(required.timeframe.cols$State.District.ID~rdate,type="l",col = "red",axes=F)


required.timeframe.cols <- as.data.table(required.timeframe.cols)
required.MO_001090 <- required.timeframe.cols[required.timeframe.cols$State.District.ID == "MO-024089",c(1,2)]

srequired.MO_001090<- required.MO_001090[order(rank(Date.of.Event.Visit), Duration.of.Event)]
srequired.timeframe.cols<- required.timeframe.cols[order(rank(Date.of.Event.Visit), Duration.of.Event)]

ts_plot(srequired.MO_001090)

srequired.MO_001090_jd <- ggplot(srequired.MO_001090, aes(Date.of.Event.Visit, Duration.of.Event)) +
  geom_point() +
  ggtitle("Coaching per School in District") +
  xlab("Date") + ylab("Time in seconds") +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=18))

srequired.MO_001090_jd + facet_grid(. ~ Date.of.Event.Visit)

#working 
dt <- head(required.timeframe.cols,10)

srequired.MO_001090<- required.MO_001090[order(rank(Duration.of.Event),Date.of.Event.Visit)]
# srequired.timeframe.cols<- required.timeframe.cols[order(rank(Date.of.Event.Visit), Duration.of.Event)]

required.MO_001090%>%arrange(mdy(required.MO_001090$Duration.of.Event))

srequired.MO_001090$State.District.ID <- "MO_001090"
srequired.MO_001090<- required.MO_001090[order(rank(Date.of.Event.Visit), Duration.of.Event)]
srequired.MO_001090$Duration.of.Event[srequired.MO_001090$Duration.of.Event == "00:60:00"] <- 1
srequired.MO_001090$Duration.of.Event <- gsub(":",".",gsub(":[^:]+$","",srequired.MO_001090$Duration.of.Event)) 
srequired.MO_001090$Duration.of.Event <- gsub("00","0",srequired.MO_001090$Duration.of.Event)
srequired.MO_001090$Duration.of.Event <- as.numeric(srequired.MO_001090$Duration.of.Event)

srequired.MO_001090$period <- ""
for(i in 1:nrow(srequired.MO_001090)){
  date <-srequired.MO_001090$Date.of.Event.Visit[i]
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
  srequired.MO_001090$period[i] <- val 
}

srequired.MO_001090 <- srequired.MO_001090[order(rank(period, Duration.of.Event))]
gg.district.compare <- ggplot(data=srequired.MO_001090, aes(x=factor(period),y=Duration.of.Event))+ 
  geom_point() + 
  facet_wrap(~State.District.ID,nrow=95,ncol=2 ,scales="free_x") + geom_hline(yintercept=4.0,linetype="dashed", color = "red") +
  geom_hline(yintercept=8.0,linetype="dashed", color = "blue")

gg.district.compare + theme(axis.text.x = element_text(face="bold", color="#993333", 
                                     size=10, angle=45),
          axis.text.y = element_text(face="bold", color="#993333", 
                                     size=10, angle=45))


# p + scale_x_continuous(name = "Speed of cars", limits = sort(as.numeric(srequired.MO_001090$Duration.of.Event)) +
#   scale_y_continuous(name = "Stopping distance", limits = c(0, 150))

sort(as.numeric(srequired.MO_001090$Duration.of.Event))



gg.district.compare <- ggplot(data=dt, aes(x=Date.of.Event.Visit,y=Duration.of.Event))+ 
  geom_point() + 
  facet_wrap(~State.District.ID,nrow=95,ncol=2 ,scales="free_x") + geom_hline(yintercept=04:00:00,linetype="dashed", color = "red")
  
sp + geom_hline(yintercept=20)


png(filename="~/Desktop/figure-district-timeseries.png",2000,5000)
print(gg.district.compare)
dev.off()



required.MO_001090 %>% arrange(desc(hm(required.MO_001090$Duration.of.Event)))

my_time_hms <- period_to_seconds(required.MO_001090$Duration.of.Event)  
my_time_sum <- seconds_to_period(required.MO_001090$Duration.of.Event)
# 
# my_time_seconds <- period_to_seconds(my_time_hms)      # Convert to seconds
# my_time_seconds   

my_time <- c("10:05:45", "07:35:51", "17:12:12",       # Create example times
             "13:18:45", "10:53:54", "06:14:25")
my_time 
my_time_hms <- hms(my_time)                            # Convert time to Period object
my_time_hms         



