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

