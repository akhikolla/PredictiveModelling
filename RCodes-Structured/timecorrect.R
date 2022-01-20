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

