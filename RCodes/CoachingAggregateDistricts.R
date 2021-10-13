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

coaching.dt <-as.data.table(required.coaching.cols)

colnames(coaching.dt)[8] <- "Districts"

coaching.dt$Districts <- tolower(coaching.dt$Districts) #5924 obs. of  16 variables

# length(unique(coaching.dt$State.District.ID))

aggregate.dt.list <- list()
for(i in 1:nrow(coaching.dt)){
  row <- coaching.dt[i,]
  col_val <- paste0(row$State.District.ID,"_",row$Districts)
  aggregate.dt.list[[col_val]] <- data.table(
    State.District.ID = 0 , 
    Districts = 0,
    #Duration.of.Event = 0,
    Collaborative.teams = 0,
    Common.formative.assessment = 0,
    Data.based.decision.making = 0,
    Effective.teaching.learning.practices = 0,
    Instructional.Leadership = 0,
    School.based.implementation.coaching = 0, Collective.teacher.efficacy = 0,
    Practice.profiles = 0,Self.assessment.practice.profile...SAPP. = 0,
    Learning.module.materials..i.e..power.points..handouts. = 0,
    DESE.virtual.learning.platform = 0,
    CWIS = 0
    )
}
# 
# Year = row$Year,
# Month = row$Month,
# Day = row$Day,
# Duration.of.Event = row$Duration.of.Event,
# Interaction.Type = row$Interaction.Type,

for(i in 1:nrow(coaching.dt)){
  row <- coaching.dt[i,]
  col_val <- paste0(row$State.District.ID,"_",row$Districts)
  print(col_val)
  print(i)
  aggregate.dt.list[[col_val]] <- data.table(
    State.District.ID = row$State.District.ID , 
    Districts = row$Districts,
    Collaborative.teams = as.numeric(aggregate.dt.list[[col_val]]$Collaborative.teams | as.numeric(row$Collaborative.teams)),
    Common.formative.assessment = as.numeric(aggregate.dt.list[[col_val]]$Common.formative.assessment | as.numeric(row$Common.formative.assessment)),
    Data.based.decision.making = as.numeric(aggregate.dt.list[[col_val]]$Data.based.decision.making | as.numeric(row$Data.based.decision.making)),
    Effective.teaching.learning.practices = as.numeric(aggregate.dt.list[[col_val]]$Effective.teaching.learning.practices | as.numeric(row$Effective.teaching.learning.practices)),
    Instructional.Leadership = as.numeric(aggregate.dt.list[[col_val]]$Instructional.Leadership | as.numeric(row$Instructional.Leadership)),
    School.based.implementation.coaching =as.numeric(aggregate.dt.list[[col_val]]$School.based.implementation.coaching | as.numeric(row$School.based.implementation.coaching)),
    Collective.teacher.efficacy = as.numeric(aggregate.dt.list[[col_val]]$Collective.teacher.efficacy | as.numeric(row$Collective.teacher.efficacy)),
    Practice.profiles =as.numeric(aggregate.dt.list[[col_val]]$Collective.teacher.efficacy | as.numeric(row$Collective.teacher.efficacy)),
    Self.assessment.practice.profile...SAPP. = as.numeric(aggregate.dt.list[[col_val]]$Self.assessment.practice.profile...SAPP. | as.numeric(row$Self.assessment.practice.profile...SAPP.)),
    Learning.module.materials..i.e..power.points..handouts. = as.numeric(aggregate.dt.list[[col_val]]$Learning.module.materials..i.e..power.points..handouts. | as.numeric(row$Learning.module.materials..i.e..power.points..handouts.)),
    DESE.virtual.learning.platform = as.numeric(aggregate.dt.list[[col_val]]$DESE.virtual.learning.platform | as.numeric(row$DESE.virtual.learning.platform)),
    CWIS =as.numeric(aggregate.dt.list[[col_val]]$CWIS | as.numeric(row$CWIS))
   # Duration.of.Event = aggregate.dt.list[[col_val]]$Duration.of.Event + row$Date.of.Event.Visit,
   )

}

districts.aggregate.dt <- do.call(rbind, aggregate.dt.list)
write.csv(districts.aggregate.dt,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/districts.aggregate.csv", row.names = FALSE)

count.districts <- list()
for(i in 1:length(unique(coaching.dt$State.District.ID))){
  count.districts[coaching.dt$State.District.ID] <- 0
}

for(i in 1:nrow(coaching.dt)){
  row <- coaching.dt[i,]
  count.districts[row$State.District.ID] <- as.numeric(count.districts[row$State.District.ID]) + 1
}
one_district <- coaching.dt[coaching.dt$State.District.ID=="MO-001090"]
one_district_manage <- one_district[,-c(1,2,3,4,5,6)]
res_Row <- districts.aggregate.dt[1,]


resultant_for_one_column <- rbind(one_district_manage, res_Row)              # Apply rbind function



write.csv(resultant_for_one_column,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/resultant_for_one_column.csv", row.names = FALSE)








weight.dt <- do.call(rbind, weight.dt.list)

ggplot()+
  geom_point(aes(
    weight, row.name),
    data=weight.dt)

weight.dt[, n.nonzero := sum(weight != 0), by=row.name]


ggplot()+
  facet_grid(n.nonzero ~ ., scales="free", space="free")+
  geom_point(aes(
    weight, row.name),
    data=weight.dt)
