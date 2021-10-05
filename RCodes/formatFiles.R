library(tidyverse)
library("readxl")
library("xlsx")
ncesdata <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/Copy of ncesdata_ECCDA30A 2.csv")
head(ncesdata)

locale <- ncesdata$Locale.
unique.locale <- unique(locale)
message(sprintf("Uncompiled functions : %s\n",paste(unique.locale, collapse=", ")))

colnames.nces.update.dataset<- c("Town: Distant","Town: Remote","Suburb: Large","City: Large","Rural: Distant","Rural: Remote","City: Small","City: Midsize","Rural: Fringe","NA","Suburb: Small","Town: Fringe","Suburb: Midsize")

locale.list <- list()
for(i in 1:nrow(ncesdata)) {
  row <- ncesdata[i,]
  localeValue <- row$Locale.
for(col in colnames.nces.update.dataset){
  print(col)
  print(localeValue)
  if(localeValue == col){
    locale.list[[col]] = c(locale.list[[col]],1)
  }else{
    locale.list[[col]]=c(locale.list[[col]],0)
  }
}
}

for(col in colnames.nces.update.dataset){
#for(i in 1:n(ncesdata)) { 
    print(col)
    print(i)
   new <-locale.list[col]                    # Create new column
  ncesdata[ , ncol(ncesdata) + 1] <- new[[1]]                 # Append new column
  colnames(ncesdata)[ncol(ncesdata)] <- gsub(": ", "_",col)  # Rename column name
}

write.csv(ncesdata,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/UpdatedNCES.csv", row.names = FALSE)

newncesupdate <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/UpdatedNCES.csv")

required.nces.cols <- newncesupdate[-c(1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17)]


required.nces.cols$Charter[required.nces.cols$Charter == "Yes"] <- 1 
required.nces.cols$Charter[required.nces.cols$Charter == "No"] <- 0

required.nces.cols$Magnet.[required.nces.cols$Magnet. ==  "†"  
                           | required.nces.cols$Magnet. == "Yes"] <- 1

required.nces.cols$Magnet.[required.nces.cols$Magnet. ==  "–"   
                           | required.nces.cols$Magnet. == "No"] <- 0


required.nces.cols$Title.I.School.[required.nces.cols$Title.I.School. ==  "†"  
                                   | required.nces.cols$Title.I.School. == "Yes"] <- 1

required.nces.cols$Title.I.School.[required.nces.cols$Title.I.School. ==  "–"   
                                   | required.nces.cols$Title.I.School. == "No"] <- 0


required.nces.cols$Title.1.School.Wide.[required.nces.cols$Title.1.School.Wide. ==  "†"  
                                        | required.nces.cols$Title.1.School.Wide. == "Yes"] <- 1

required.nces.cols$Title.1.School.Wide.[required.nces.cols$Title.1.School.Wide. ==  "–"   
                                        | required.nces.cols$Title.1.School.Wide. == "No"] <- 0

required.nces.cols$Students.[required.nces.cols$Students. ==  "†" | 
                               required.nces.cols$Students. ==  "–"  ] <- 0


required.nces.cols$Teachers.[required.nces.cols$Teachers. ==  "†" | 
                               required.nces.cols$Teachers. ==  "–"  ] <- 0

required.nces.cols$Student.Teacher.Ratio.[required.nces.cols$Student.Teacher.Ratio. ==  "†" | 
                                            required.nces.cols$Student.Teacher.Ratio. ==  "–"  ] <- 0


required.nces.cols$Free.Lunch.[required.nces.cols$Free.Lunch. ==  "†" | 
                                 required.nces.cols$Free.Lunch. ==  "–"  ] <- 0


required.nces.cols$Reduced.Lunch.[required.nces.cols$Reduced.Lunch. ==  "†" | 
                                    required.nces.cols$Reduced.Lunch. ==  "–"  ] <- 0

required.nces.cols$Free.Reduced.Rate[is.na(required.nces.cols$Free.Reduced.Rate)] <- 0

unique.values.percol.nces <- list()

for(k in 1:ncol(required.nces.cols)){
  print(names(required.nces.cols)[k])
  kcol.values <- required.nces.cols[k]
  unique.col.values <- unique(kcol.values)
  print(unique.col.values)
  unique.values.percol.nces[names(required.nces.cols)[k]] <- unique.col.values
}


#================================================================================


cwisdata <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/newcwis_survey data/cwis_survey data-Table 1.csv")
required.cwis.cols <- cwisdata[-c(1,2,4,5,6,12,13,14,15)]

required.cwis.cols$experience[is.na(required.cwis.cols$experience)] <- 0
required.cwis.cols$member_grade_span_level[required.cwis.cols$member_grade_span_level == "TRUE" 
                                           | required.cwis.cols$member_grade_span_level == "Yes"
                                           | required.cwis.cols$member_grade_span_level == "t" 
                                           | required.cwis.cols$member_grade_span_level == "1"] <- 1
required.cwis.cols$member_grade_span_level[required.cwis.cols$member_grade_span_level == "FALSE" 
                                           | required.cwis.cols$member_grade_span_level == "No"
                                           | required.cwis.cols$member_grade_span_level == "f" 
                                           | required.cwis.cols$member_grade_span_level == "0"
                                           | required.cwis.cols$member_grade_span_level == "TK"
                                           | required.cwis.cols$member_grade_span_level == ""] <- 0
required.cwis.cols$admin_receive_coaching[is.na(required.cwis.cols$admin_receive_coaching)] <- -1
required.cwis.cols$admin_receive_coaching[required.cwis.cols$admin_receive_coaching == "TRUE"] <- 1
required.cwis.cols$admin_receive_coaching[required.cwis.cols$admin_receive_coaching == "FALSE"] <- 0
required.cwis.cols$district_accept_questions[is.na(required.cwis.cols$district_accept_questions)] <- 0
required.cwis.cols$district_accept_questions[required.cwis.cols$district_accept_questions == "TRUE"] <- 1
required.cwis.cols$ETL.AVERAGE[is.na(required.cwis.cols$ETL.AVERAGE)] <- 0
required.cwis.cols$CFA.AVERAGE[is.na(required.cwis.cols$CFA.AVERAGE)] <- 0
required.cwis.cols$DBDM.AVERAGE[is.na(required.cwis.cols$DBDM.AVERAGE)] <- 0
required.cwis.cols$LEAD.AVERAGE[is.na(required.cwis.cols$LEAD.AVERAGEE)] <- 0
required.cwis.cols$PD.AVERAGE[is.na(required.cwis.cols$PD.AVERAGE)] <- 0
for(j in 12:ncol(required.cwis.cols)){
  required.cwis.cols[,j][is.na(required.cwis.cols[,j])] <- 0
  required.cwis.cols[,j][required.cwis.cols[,j] == "TRUE"] <- 1 
  required.cwis.cols[,j][required.cwis.cols[,j] == "FALSE"] <- 0

}

unique.values.percol <- list()
for(i in 1:ncol(required.cwis.cols)){
  print(names(required.cwis.cols)[i])
  col.values <- required.cwis.cols[i]
  unique.col.values <- unique(col.values)
  print(unique.col.values)
  unique.values.percol[names(required.cwis.cols)[i]] <- unique.col.values
}


#================================================================================
#Rate of Engagement (Coaching logs).

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

unique.values.percol.coaching <- list()
for(i in 1:ncol(required.coaching.cols)){
  print(names(required.coaching.cols)[i])
  col.values <- required.coaching.cols[i]
  unique.col.values <- unique(col.values)
  print(unique.col.values)
  unique.values.percol.coaching[names(required.coaching.cols)[i]] <- unique.col.values
}



nces.coaching <- merge(required.nces.cols, required.coaching.cols, by = "State.District.ID")
write.csv(nces.coaching,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/nces_coachingdata.csv", row.names = TRUE)

cwis.nces.coaching <- merge(required.cwis.cols, nces.coaching, by = "State.District.ID")

system.time(
  cwis.nces.coaching <- merge(required.cwis.cols, nces.coaching, by = "State.District.ID")
)
