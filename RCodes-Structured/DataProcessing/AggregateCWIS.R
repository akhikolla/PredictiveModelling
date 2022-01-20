library(data.table)

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



multinomial_matrix <- matrix(0, nrow = 186, ncol = 5)

districts.values <- unique(cwis.dt$State.District.ID)
for(itr in districts.values){
  
  row.value <- cwis.dt[cwis.dt$State.District.ID == itr]
  for(row in 1:nrow(row.value)){
    ETL.score <- row.value[row,]$ETL.AVERAGE
    multinomial_matrix[row]
  }
  heat.data[row.value$State.District.ID,row.value$period] <- heat.data[row.value$State.District.ID,row.value$period] + 1
}


cwis.dt[, cwis_mean_cols] <- cwis.dt[, lapply(.SD, as.numeric), .SDcols = cwis_mean_cols]
cwis.aggregate <- cwis.dt[, lapply(.SD, mean), .SDcols = cwis_mean_cols,by = c("State.District.ID","created_at")]
#cwis.aggregate <- unique(cwis.dt, by = "State.District.ID")
##Previously
cwis.aggregate.df <- as.data.frame(cwis.aggregate)
#'data.frame':	230 obs. of  87 variables:
#'Current

#write.csv(cwis.aggregate.df,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/correct_cwis_aggregate_districts.csv", row.names = FALSE)



#updated
cwis.aggregate.peryear<- as.data.table(cwis.aggregate)
#str(cwis.aggregate.updated)
#592 obs. of  88 variables:

#write.csv(cwis.aggregate.peryear,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/cwis_aggregate_peryear.csv", row.names = FALSE)




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


#write.csv(cwis.aggregate.df,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/correct_cwis_aggregate_districts.csv", row.names = FALSE)

# cwis.aggregate <- cwis.aggregate[-c(2,3,4)] 


# cwis.districts.aggregates <- tesit[, lapply(.SD, mean), by=State.District.ID]
# cwis.aggregates <- unique(tesit, by = "State.District.ID")

# 
nces.dt <- as.data.table(nces.dt)
nces.cwis.dt <-nces.dt[as.data.table(cwis.aggregate.df),on=.(State.School.ID),nomatch = NULL]
#nces+cwis+coaching
districts.aggregate.dt <- data.table::fread("/Users/akhilachowdarykolla/Documents/PredictiveModelling/data/districts.aggregate.csv")

cwis.aggregrate.districts <- districts.aggregate.dt[cwis.aggregate.df,on=.(State.District.ID ),nomatch = NULL]
#Classes ‘data.table’ and 'data.frame':	186 obs. of  100 variables:
#write.csv(cwis.aggregrate.districts,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/coaching_cwis_aggregrate_districts.csv", row.names = FALSE)


cwis.dt[, cwis_required_cols] <- cwis.dt[, lapply(.SD, as.numeric), .SDcols = cwis_required_cols]

cwis.dt[, cwis_required_cols] <- cwis.dt[, lapply(.SD, mean), .SDcols = cwis_required_cols]



