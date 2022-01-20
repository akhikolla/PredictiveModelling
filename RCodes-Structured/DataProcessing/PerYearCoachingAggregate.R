coachingdata <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/Coaching logs Fall 2017- Spring 2021/Condensed columns-Table 1.csv")#,header = TRUE,check.names=TRUE )
required.coaching.cols <- coachingdata[,c(4,5,6,8,9,21,23,25,27,29,31,33,41,42,43,44,46)]

required.coaching.cols$Year <- as.numeric(paste0("20",sapply(strsplit(as.character(
  required.coaching.cols$Date.of.Event.Visit),'/'), "[", 3)))
required.coaching.cols$Month <- as.numeric(sapply(strsplit(as.character(required.coaching.cols$Date.of.Event.Visit),'/'), "[", 1))


required.coaching.cols$Day <- as.numeric(sapply(strsplit(as.character(required.coaching.cols$Date.of.Event.Visit),'/'), "[", 2))


setcolorder(required.coaching.cols, c("Date.of.Event.Visit", "Year", "Month","Day"))


required.coaching.cols$Duration.of.Event[required.coaching.cols$Duration.of.Event == ""] <- 0
required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == ""] <- "None"


required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "Yes" |
                                             required.coaching.cols$Collaborative.teams == "yes" ] <- 1

required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "No" |
                                             required.coaching.cols$Collaborative.teams == "" ] <- 0

for(j in 9:ncol(required.coaching.cols)){
  required.coaching.cols[,j][is.na(required.coaching.cols[,j])] <- 0
  required.coaching.cols[,j][required.coaching.cols[,j] == "Yes" |
                               required.coaching.cols[,j] == "yes"  ] <- 1 
  required.coaching.cols[,j][required.coaching.cols[,j] == "No" |
                               required.coaching.cols[,j] == ""] <- 0
  
}


required.timeframe.cols <- required.coaching.cols

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


coaching.dt <-as.data.table(required.coaching.cols)

colnames(coaching.dt)[8] <- "Districts"

coaching.dt$Districts <- tolower(coaching.dt$Districts) #5924 obs. of  16 variables
required.timeframe.cols <- as.data.table(required.timeframe.cols)

coaching_or_cols <- c("Collaborative.teams","Common.formative.assessment","Data.based.decision.making","Effective.teaching.learning.practices",
                    "Instructional.Leadership","School.based.implementation.coaching","Collective.teacher.efficacy","Practice.profiles",
                    "Self.assessment.practice.profile...SAPP.","Learning.module.materials..i.e..power.points..handouts.","DESE.virtual.learning.platform",
                    "CWIS")


required.timeframe.cols[, coaching_or_cols] <- required.timeframe.cols[, lapply(.SD, as.numeric), .SDcols = coaching_or_cols]
yearwisedistrictAggregate <-required.timeframe.cols[, lapply(.SD, mean), .SDcols = coaching_or_cols,by = c("State.District.ID","Year")]

write.csv(yearwisedistrictAggregate,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/yearwisedistrictAggregate.csv", row.names = FALSE)
str(yearwisedistrictAggregate)
#	543 obs. of  14 variables:
##Combine with CWIS


cwis.aggregate.df <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/cwis_aggregate_peryear.csv")
cwis.aggregate.df <- as.data.frame(cwis.aggregate.df)
for(j in 1:88){
  cwis.aggregate.df[,j][is.na(cwis.aggregate.df[,j])] <- 0
}
##str(cwis.aggregate.df)
##592 obs. of  88 variables:
cwis.aggregate.df <- as.data.table(cwis.aggregate.df)
colnames(cwis.aggregate.df)[2] <- "Year"

cwis.aggregrate.districts <- yearwisedistrictAggregate[cwis.aggregate.df,on=.(State.District.ID,Year),nomatch = NULL]
#Combined
##str(cwis.aggregrate.districts)
##374 obs. of  100 variables:
head(cwis.aggregrate.districts)

write.csv(cwis.aggregrate.districts,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/year_cwis_aggregrate_districts.csv", row.names = FALSE)


one_district <- cwis.aggregrate.districts[cwis.aggregrate.districts$State.District.ID=="MO-001090"]
str(one_district)
##3 obs. of  100 variables


