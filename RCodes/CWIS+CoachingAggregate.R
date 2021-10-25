coachingdata <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/Coaching logs Fall 2017- Spring 2021/Condensed columns-Table 1.csv")#,header = TRUE,check.names=TRUE )
required.coaching.cols <- coachingdata[,c(5,6,8,9,21,23,25,27,29,31,33,35,37,41,42,43,44,46,53,56)]

interactionType <- required.coaching.cols$Interaction.Type
unique.interactionType<- unique(interactionType)
unique.interactionType
duration <- required.coaching.cols$Duration.of.Event
unique.duration<- unique(duration)
unique.duration

message(sprintf("Uncompiled functions : %s\n",paste(unique.interactionType, collapse=", ")))

required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type ==  "Virtual w/ Video"  
                           | required.coaching.cols$Interaction.Type == "Phone"
                           | required.coaching.cols$Interaction.Type == "Conference Call"
                           | required.coaching.cols$Interaction.Type == "Phone/conference call"
                           | required.coaching.cols$Interaction.Type == "phone"
                           | required.coaching.cols$Interaction.Type == "Virtual w/video"
                           | required.coaching.cols$Interaction.Type == "Phone/Conference Call"] <- "Virtual"

required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type ==  "In-Person"  
                                        | required.coaching.cols$Interaction.Type == "In-person"
                                        | required.coaching.cols$Interaction.Type == "" ] <- "In-Person"


required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type ==  "In-Person & Virtual"  
                                        | required.coaching.cols$Interaction.Type == "In-person and Virtual"] <- "In-Person&Virtual"




colnames.coach.update.dataset<- c("Virtual","In-Person","In-Person&Virtual")

interactionType.list <- list()
for(i in 1:nrow(required.coaching.cols)) {
  row <- required.coaching.cols[i,]
  interactionType <- row$Interaction.Type
  for(col in colnames.coach.update.dataset){
    print(col)
    print(interactionType)
    if(interactionType == col){
      interactionType.list[[col]] = c(interactionType.list[[col]],1)
    }else{
      interactionType.list[[col]] = c(interactionType.list[[col]],0)
    }
  }
}

for(col in colnames.coach.update.dataset){
  print(col)
  print(i)
  new <-interactionType.list[col]                    # Create new column
  required.coaching.cols[ , ncol(required.coaching.cols) + 1] <- new[[1]]                 # Append new column
  colnames(required.coaching.cols)[ncol(required.coaching.cols)] <- gsub(": ", "_",col)  # Rename column name
}



required.coaching.cols$Duration.of.Event[required.coaching.cols$Duration.of.Event == ""] <- 0
#required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == ""] <- "None"


required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "Yes" |
                                             required.coaching.cols$Collaborative.teams == "yes" ] <- 1

required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "No" |
                                             required.coaching.cols$Collaborative.teams == "" ] <- 0

required.coaching.cols$MMD.DCI.expectations.logistics.DESE.specifics[
  required.coaching.cols$MMD.DCI.expectations.logistics.DESE.specifics == "Plans for internal and external reporting of MMD; shared Google folder; invoicing"] <- 0


required.coaching.cols$Alignment.and.systems.planning[required.coaching.cols$Alignment.and.systems.planning == "District analysis of CWIS, district focus/goal ACL aligned to student achievement in CSIP, plans for discussing with all administrators and identification of SAPP(s) to complete"] <- 0


for(j in 5:ncol(required.coaching.cols)){
  required.coaching.cols[,j][is.na(required.coaching.cols[,j])] <- 0
  required.coaching.cols[,j][required.coaching.cols[,j] == "Yes" |
                               required.coaching.cols[,j] == "yes"  ] <- 1 
  required.coaching.cols[,j][required.coaching.cols[,j] == "No" |
                               required.coaching.cols[,j] == ""] <- 0
  
}

unique.values.percol.coaching <- list()
for(i in 1:ncol(required.coaching.cols)){
  print(names(required.coaching.cols)[i])
  col.values <- required.coaching.cols[i]
  unique.col.values <- unique(col.values)
  print(unique.col.values)
  unique.values.percol.coaching[names(required.coaching.cols)[i]] <- unique.col.values
}


for(i in 5:ncol(required.coaching.cols)){
  required.coaching.cols[,i] <- as.numeric(required.coaching.cols[,i])
}

duration <- required.coaching.cols$Duration.of.Event
unique.duration<- unique(duration)
unique.duration

required.coaching.cols$Duration.of.Event[required.coaching.cols$Duration.of.Event == "NA"] <- 0
required.coaching.cols$Duration.of.Event  <- gsub("\\.000","",required.coaching.cols$Duration.of.Event)
# required.coaching.cols$Duration.of.Event  <- gsub("60:00","1",required.coaching.cols$Duration.of.Event)
# required.coaching.cols$Duration.of.Event  <-gsub(":",".",gsub(":00","",required.coaching.cols$Duration.of.Event))

## Minutes Converstion
required.coaching.cols$Duration.of.Event <- gsub("min","minutes",required.coaching.cols$Duration.of.Event,fixed = T)
required.coaching.cols$Duration.of.Event <- gsub("Minutes","minutes",required.coaching.cols$Duration.of.Event,fixed = T)

required.coaching.cols$Duration.of.Event <- gsub("mnutes","minutes",required.coaching.cols$Duration.of.Event,fixed = T)
required.coaching.cols$Duration.of.Event <- gsub(" minutes ","minutes",required.coaching.cols$Duration.of.Event,fixed = T)

required.coaching.cols$Duration.of.Event <- gsub("minutesutes","minutes",required.coaching.cols$Duration.of.Event,fixed = T)



##HOURS REPLACEMENTS
required.coaching.cols$Duration.of.Event <- gsub(",",".",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub(" ","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("houra$"," hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("hour$"," hours",required.coaching.cols$Duration.of.Event)
#required.coaching.cols$Duration.of.Event <- gsub("hour"," hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("hoirs$"," hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("Hours"," hours",required.coaching.cols$Duration.of.Event)

required.coaching.cols$Duration.of.Event <- gsub("hrs.$"," hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("hrs$"," hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("hr$"," hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("hr"," hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("h$"," hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("hh$"," hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("Hrs$"," hours",required.coaching.cols$Duration.of.Event)


##UNNECCESSARY DATA REMOVAL

required.coaching.cols$Duration.of.Event <- gsub("s\\(duetolatestartforinclementweather\\)","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("sonsite;9 hoursstravel","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("sand"," ",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("sonsite.4 hoursstravel","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("sonsite;3 hoursstravel$","",required.coaching.cols$Duration.of.Event)

required.coaching.cols$Duration.of.Event <- gsub("sonsite;3.5 hoursstravel","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub(".inElem.1.5 hours.H.S.","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("1.5=","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("1.25\\(","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("\\)","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("one","1",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("h hours","hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("FullDay","8:00:00",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("allday","8:00:00",required.coaching.cols$Duration.of.Event)

required.coaching.cols$Duration.of.Event <- gsub("plus3hourstravelingtime","",required.coaching.cols$Duration.of.Event)

required.coaching.cols$Duration.of.Event <- gsub("plus3travelingtime","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("2hoursdrivingtime","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("plus2hourstravelingtime","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("plus2hourstravelingtime$","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("2hoursdrivingtime$","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("2hourdrivingtime$","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("2hourdrivingtime","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub(";2 hoursstravel","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("s;2 hoursstravel","",required.coaching.cols$Duration.of.Event)


required.coaching.cols$Duration.of.Event <- gsub("drivetime2hours","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("sessionsat1.5eac ","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("sessionsat1.5","hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("120minutes/","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("1/2day","4:00:00",required.coaching.cols$Duration.of.Event)



required.coaching.cols$Duration.of.Event <- gsub("l hours$","1 hours",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("i hours","1 hours",required.coaching.cols$Duration.of.Event)
#required.coaching.cols$Duration.of.Event <- gsub("^.","0.",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("\\(2days\\)","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("s\\(2days\\)","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("inp.m.","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("Hrs\\(2days","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("s\\(2days","",required.coaching.cols$Duration.of.Event)


unique(required.coaching.cols$Duration.of.Event)






for(tm in 1:nrow(required.coaching.cols)){
  #CV.Duration.of.Event <- 0
   if(grepl("minutes",required.coaching.cols$Duration.of.Event[tm])){
     #print(required.coaching.cols$Duration.of.Event[tm])
  CV.Duration.of.Event <- as.numeric(gsub("minutes","",required.coaching.cols$Duration.of.Event[tm]))/60
  if(is.na(CV.Duration.of.Event)){
    CV.Duration.of.Event <- gsub("minutes","",required.coaching.cols$Duration.of.Event[tm])
    CV.Duration.of.Event <- gsub(" ",".",CV.Duration.of.Event)
  }
  required.coaching.cols$Duration.of.Event[tm] = CV.Duration.of.Event
  print(CV.Duration.of.Event)
   }
}


required.coaching.cols$Duration.of.Event <- gsub("[hours]+",".",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("[hour]+",".",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("[hourand]+",".",required.coaching.cols$Duration.of.Event)

required.coaching.cols$Duration.of.Event <- gsub("\\s$","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("\\.+",".",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("\\.$","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("\\s$","",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("^\\.","00:",required.coaching.cols$Duration.of.Event)
required.coaching.cols$Duration.of.Event <- gsub("^\\:","00:",required.coaching.cols$Duration.of.Event)


for(tm in 1:nrow(required.coaching.cols)){
  if(length( strsplit(required.coaching.cols$Duration.of.Event[tm], ":", fixed=TRUE)[[1]]) == 1){
     if(length( strsplit(required.coaching.cols$Duration.of.Event[tm], ".", fixed=TRUE)[[1]]) == 1){
    print("a")
       print(paste0(required.coaching.cols$Duration.of.Event[tm],":00:00"))
      required.coaching.cols$Duration.of.Event[tm] <-paste0(required.coaching.cols$Duration.of.Event[tm],":00:00")
     }else{
      value <- as.numeric(required.coaching.cols$Duration.of.Event[tm])
      modifier <- format(round(value, 2), nsmall = 2)
       print(paste0(modifier))
       # print("sorry although one needs split")
      if(length( strsplit(modifier, ".", fixed=TRUE)[[1]]) > 1){
        hours <- as.numeric(gsub("\\..*","",modifier))
        minutes <- as.numeric(gsub(".*\\.","",modifier))
        if(minutes/60 > 1){
          print("b")
          fhours <- as.numeric(gsub("\\..*","",as.character((minutes/60) + hours)))
          fminutes <- gsub(".*\\.","",as.character((minutes/60) + hours))
        print(paste0(fhours,":",format(round(as.numeric(fminutes), 2), nsmall = 2),":00"))
            required.coaching.cols$Duration.of.Event[tm] <-paste0(fhours,":",format(round(as.numeric(fminutes), 2), nsmall = 2),":00")
        }else{
          print("c")
          print(paste0(hours,":",round(as.numeric(sub("\\..*","",as.character((minutes/60)))), 2),":00"))
          required.coaching.cols$Duration.of.Event[tm] <-paste0(hours,":",format(round(as.numeric(sub("\\..*","",as.character((minutes/60)))), 2), nsmall = 2),":00")
        }
      
      }
    }
  }else if(length( strsplit(required.coaching.cols$Duration.of.Event[tm], ":", fixed=TRUE)[[1]]) == 2){
    print("d")
    print(paste0("00:",required.coaching.cols$Duration.of.Event[tm]))
     required.coaching.cols$Duration.of.Event[tm] <- paste0("00:",required.coaching.cols$Duration.of.Event[tm])
  }else{
    print("e")
    print(paste0(required.coaching.cols$Duration.of.Event[tm]))
    required.coaching.cols$Duration.of.Event[tm] <- paste0(required.coaching.cols$Duration.of.Event[tm])
  }
}
  




time_pats <- unique(required.coaching.cols$Duration.of.Event)
format(as.Date(str1, "%Y_0%d.%b"), "%Y_%b")
strtoi(as.difftime(Time.Training, format = "%H:%M:%S", units = "mins"))

str1 = "my_hour.check"

#[1] "2018_Feb" or by converting to date class format(as.Date(str1, "%Y_0%d.%b"), "%Y_%b")





format(round(as.numeric(sub("\\..*","",as.character((minutes/60)))), 2), nsmall = 2)



round(as.numeric(sub("\\..*","",as.character((minutes/60)))), 2)


