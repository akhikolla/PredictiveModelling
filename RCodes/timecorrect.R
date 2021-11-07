coachingdata <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/Coaching logs Fall 2017- Spring 2021/Condensed columns-Table 1.csv")
required.coaching.cols <- coachingdata[,c(5,6,8,21,23,25,27,29,31,33,41,42,43,44,46)]

required.coaching.cols$Duration.of.Event[required.coaching.cols$Duration.of.Event == ""] <- 0
required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == ""] <- "None"


required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "Yes" |
                                             required.coaching.cols$Collaborative.teams == "yes" ] <- 1

required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "No" |
                                             required.coaching.cols$Collaborative.teams == "" ] <- 0

for(j in 5:ncol(required.coaching.cols)){
  required.coaching.cols[,j][is.na(required.coaching.cols[,j])] <- 0
  required.coaching.cols[,j][required.coaching.cols[,j] == "Yes" |
                               required.coaching.cols[,j] == "yes"  ] <- 1 
  required.coaching.cols[,j][required.coaching.cols[,j] == "No" |
                               required.coaching.cols[,j] == ""] <- 0
  
}



for(tm in 1:nrow(required.coaching.cols)){
  if(length( strsplit(required.coaching.cols$Duration.of.Event[tm], ":", fixed=TRUE)[[1]]) == 1){
    if(length( strsplit(required.coaching.cols$Duration.of.Event[tm], ".", fixed=TRUE)[[1]]) == 1){
     # print("a")
      if(required.coaching.cols$Duration.of.Event[tm] < 10){
        required.coaching.cols$Duration.of.Event[tm]<-paste0(required.coaching.cols$Duration.of.Event[tm],":00:00")
     # print(paste0(required.coaching.cols$Duration.of.Event[tm],":00:00"))
      # }else{
      #   val <- as.numeric(required.coaching.cols$Duration.of.Event[tm]) 
      #   print(paste0(val,":00:00"))
      # }
      #required.coaching.cols$Duration.of.Event[tm] <-paste0(required.coaching.cols$Duration.of.Event[tm],":00:00")
      }
     }else{
      value <- as.numeric(required.coaching.cols$Duration.of.Event[tm])
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
          required.coaching.cols$Duration.of.Event[tm] <-paste0(fhours,":",round(as.numeric(fminutes), 2),":00")
          # print(paste0(fhours,":",round(as.numeric(fminutes), 2),":00"))
         # required.coaching.cols$Duration.of.Event[tm] <-paste0(fhours,":",format(round(as.numeric(fminutes), 2), nsmall = 2),":00")
        }else{
          #print("c")
          required.coaching.cols$Duration.of.Event[tm] <-paste0(hours,":",round(as.numeric(sub("\\..*","",as.character((minutes)))), 2),":00")
          #print(paste0(hours,":",round(as.numeric(sub("\\..*","",as.character((minutes)))), 2),":00"))
          #required.coaching.cols$Duration.of.Event[tm] <-paste0(hours,":",format(round(as.numeric(sub("\\..*","",as.character((minutes/60)))), 2), nsmall = 2),":00")
        }
        
      }
    }
  }else if(length( strsplit(required.coaching.cols$Duration.of.Event[tm], ":", fixed=TRUE)[[1]]) == 2){
    #print("d")
    #print(paste0("00:",required.coaching.cols$Duration.of.Event[tm]))
    required.coaching.cols$Duration.of.Event[tm] <- paste0("00:",required.coaching.cols$Duration.of.Event[tm])
   # required.coaching.cols$Duration.of.Event[tm] <- paste0("00:",required.coaching.cols$Duration.of.Event[tm])
  }else{
    #print("e")
    required.coaching.cols$Duration.of.Event[tm] <- paste0(required.coaching.cols$Duration.of.Event[tm])
   # print(paste0(required.coaching.cols$Duration.of.Event[tm]))
   # required.coaching.cols$Duration.of.Event[tm] <- paste0(required.coaching.cols$Duration.of.Event[tm])
  }
}

unique(required.coaching.cols$Duration.of.Event)

required.coaching.cols$Duration.of.Event <- gsub("11666666666667","00",required.coaching.cols$Duration.of.Event)

