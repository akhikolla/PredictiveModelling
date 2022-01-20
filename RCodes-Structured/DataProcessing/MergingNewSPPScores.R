library(data.table)


coachingdata <- read.csv("/Users/akhilachowdarykolla/Documents/PredictiveModelling/Coaching logs Fall 2017- Spring 2021/Condensed columns-Table 1.csv")#,header = TRUE,check.names=TRUE )
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

periods <- c("Aug17-Feb18"  ,"Mar18-July18" ,
             "Aug18-Feb19", "Mar19-July19" , "Aug19-Feb20" , "Mar20-July20" ,
             "Aug20-Feb21","Mar21-July21" , "Aug21-Feb22")

date.to.period <- function(month,year) 
{
  Month <- month
  Year <- year
  if(Month < 3){
    val <- paste0("Aug", Year-1 , "-" , "Feb", Year)
  }else if(Month > 3 & Month < 8){
    val <- paste0("Mar", Year , "-" , "July" , Year)
    print(val)
  }else{
    val <- paste0("Aug", Year , "-" , "Feb" , Year+1)
  }
  return(val)
}


coaching.dt$Period.of.Event <- ""
coaching.dt$Period.of.Event <- date.to.period(coaching.dt$Month,as.integer(sub('.*(?=.{2}$)', '',coaching.dt$Year, perl=T)))
coaching.dt[,Period.of.Event:=NULL]


aggregate.dt.list <- list()

for(i in 1:nrow(coaching.dt)){
  row <- coaching.dt[i,]
  col_val <- paste0(row$State.District.ID,"_",row$Districts,"_",date.to.period( row$Month,  as.integer(sub('.*(?=.{2}$)', '',row$Year, perl=T))))
  aggregate.dt.list[[col_val]] <- data.table(
    Year = as.integer(sub('.*(?=.{2}$)', '',row$Year, perl=T)),
    Month = row$Month,
    Day = row$Day,
    Period.of.Event = date.to.period(Month,Year),
    Duration.of.Event = row$Duration.of.Event,
    Interaction.Type = row$Interaction.Type,
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


sub.check.coaching.dt <- coaching.dt

for(i in 1:nrow(sub.check.coaching.dt)){
  row <- sub.check.coaching.dt[i,]
  #col_val <- paste0(row$State.District.ID,"_",row$Districts)
  col_val <- paste0(row$State.District.ID,"_",row$Districts,"_",date.to.period(row$Month, as.integer(sub('.*(?=.{2}$)', '',row$Year, perl=T))))
  print(col_val)
  print(i)
  aggregate.dt.list[[col_val]] <- data.table(
    State.District.ID = row$State.District.ID , 
    Districts = row$Districts,
    Period.of.Event = date.to.period(row$Month, as.integer(sub('.*(?=.{2}$)', '',row$Year, perl=T))),
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

districts.aggregate.dt <- districts.aggregate.dt[!(districts.aggregate.dt$Period.of.Event == "Mar1-July1")]
districts.aggregate.dt <- districts.aggregate.dt[!(districts.aggregate.dt$Period.of.Event == "Aug7-Feb8")]

###CWIS data update
cwisdata <- read.csv("/Users/akhilachowdarykolla/Documents/PredictiveModelling/newcwis_survey data/cwis_survey data-Table 1.csv")
required.cwis.cols <- cwisdata[-c(1,2,5,6,8,12,13,14,15,21,22,23,24,25,26,27)]
str(cwisdata) #80267 obs. of  106 variables:
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

required.cwis.cols$created_at [is.na(required.cwis.cols$created_at)] <- "2017"
required.cwis.cols$created_at [required.cwis.cols$created_at == ""] <- "2017"
required.cwis.cols$created_at <- gsub("-.*","",required.cwis.cols$created_at)
unique(required.cwis.cols$created_at)

required.cwis.cols$updated_at [is.na(required.cwis.cols$updated_at)] <- "2017"
required.cwis.cols$updated_at [required.cwis.cols$updated_at == ""] <- "2017"
required.cwis.cols$updated_at <- gsub("-.*","",required.cwis.cols$updated_at)
unique(required.cwis.cols$updated_at)

cwis.dt <- as.data.table(required.cwis.cols)
tesit <-cwis.dt 
nts <- cwis.dt
str(cwis.dt) #80267 obs. of  98 variables
cwis.df <- required.cwis.cols
colnames(cwis.dt)
message(sprintf("Uncompiled functions : %s\n",paste(colnames(cwis.dt), collapse=", ")))


cwis_mean_cols <- c("experience","member_grade_span_level","admin_receive_coaching","district_accept_questions","ETL.AVERAGE","CFA.AVERAGE","DBDM.AVERAGE","LEAD.AVERAGE","PD.AVERAGE","common_practices_addresses_standard","common_practices_teacher_use_cfa",
                    "common_practices_student_use_cfa","common_practices_cfa_reteaching","collab_teams_reviews_data",
                    "collab_teams_positive_interaction","collab_teams_effective_teaming","collab_teams_data_collaboration",
                    "collab_teams_analyze_during_meeting_now","collab_teams_use_data_analysis_system_now",
                    "collab_teams_learning_analyzed_p1","collab_teams_systematically_analyze_p2",
                    "collab_teams_modify_instruction_p3","collab_teams_reflecting_instruction_p4","collab_teams_review_learning_targets_p5","prof_learning_leader_manage_expectations","prof_learning_leader_teacher_observation","prof_learning_leader_committed_instruction","prof_learning_leader_collab_teams","prof_learning_self_dev_instructional_practices","prof_learning_self_receive_coaching","prof_learning_self_dev_monitor_student","prof_learning_self_receive_feedback","admin_clarified_purpose","admin_conv_gone_well","admin_conv_relevant_data","admin_add_suggestions","admin_provide_rationales","admin_provide_opportunity","admin_supported_suggestions","admin_guided_practice","admin_identify_next_steps","admin_paced_conversation","district_identified_strategies","district_deploy_central_office","district_deploy_principals","district_use_aligned_teams","district_using_technology","district_integrate_technology","district_utilize_virtual_learning","district_monitor_focused_improvement","collab_teams_analyze_during_meeting_historical","collab_teams_use_data_analysis_system_historical","employed_last_year","admin_expected_meet_during_covid","admin_collab_teams_reviews_data_now","admin_collab_teams_reviews_data_pre_covid","admin_collab_teams_positive_interaction_now","admin_collab_teams_positive_interaction_pre_covid","admin_collab_teams_effective_teaming_now","admin_collab_teams_effective_teaming_pre_covid","admin_collab_teams_analyze_during_meeting_now","admin_collab_teams_analyze_during_meeting_pre_covid","admin_collab_teams_use_data_analysis_system_now","admin_collab_teams_use_data_analysis_system_pre_covid","admin_prof_learning_self_dev_instructional_practices_now","admin_prof_learning_self_dev_instructional_practices_pre_covid","admin_prof_learning_self_receive_coaching_now","admin_prof_learning_self_receive_coaching_pre_covid","admin_prof_learning_self_dev_monitor_student_now","admin_prof_learning_self_dev_monitor_student_pre_covid","admin_prof_learning_self_receive_feedback_now","admin_prof_learning_self_receive_feedback_pre_covid","admin_common_practices_can_statements_now","admin_common_practices_can_statements_pre_covid","admin_common_practices_student_work_now","admin_common_practices_student_work_pre_covid","admin_common_practices_self_assessment_now","admin_common_practices_self_assessment_pre_covid","admin_common_practices_receive_feedback_now","admin_common_practices_receive_feedback_pre_covid","admin_common_practices_student_feedback_now","admin_common_practices_student_feedback_pre_covid","admin_common_practices_state_criteria_now","admin_common_practices_state_criteria_pre_covid",
                    "admin_common_practices_student_review_cfa_now","admin_common_practices_student_review_cfa_pre_covid")

# dtnew <- dt[, lapply(.SD, as.character), by=ID]
# str(dtnew)

tesit[, cwis_mean_cols] <- tesit[, lapply(.SD, as.numeric), .SDcols = cwis_mean_cols]
nt <-tesit[, lapply(.SD, mean), .SDcols = cwis_mean_cols,by = "State.District.ID"]


cwis.dt[, cwis_mean_cols] <- cwis.dt[, lapply(.SD, as.numeric), .SDcols = cwis_mean_cols]
cwis.aggregate <- cwis.dt[, lapply(.SD, mean), .SDcols = cwis_mean_cols,by = c("State.District.ID","created_at")]

cwis.aggregate.df <- as.data.frame(cwis.aggregate)



for(j in 2:ncol(cwis.aggregate.df)){
  cwis.aggregate.df[,j][is.na(cwis.aggregate.df[,j])] <- 0
  # required.cwis.cols[,j][required.cwis.cols[,j] == "TRUE"] <- 1 
  # required.cwis.cols[,j][required.cwis.cols[,j] == "FALSE"] <- 0
  
}

unique.values.percol.coaching <- list()
for(i in 1:ncol(cwis.aggregate.df)){
  print(names(cwis.aggregate.df)[i])
  col.values <- cwis.aggregate.df[i]
  unique.col.values <- unique(col.values)
  print(unique.col.values)
  unique.values.percol.coaching[names(cwis.aggregate.df)[i]] <- unique.col.values
}



cwis.aggregrate.districts <- districts.aggregate.dt[cwis.aggregate.df,on=.(State.District.ID ),nomatch = NULL]
cwis.aggregrate.districts <- districts.aggregate.dt[cwis.aggregate,on=.(State.District.ID ),nomatch = NULL]




