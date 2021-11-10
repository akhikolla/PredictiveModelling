library(data.table)

ETL.num.input.data<- data.table::fread("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/factored_combined_input_data.csv")
head(ETL.num.input.data)

unique.state.ids <- ETL.num.input.data$State.District.ID



cwisdata <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/newcwis_survey data/cwis_survey data-Table 1.csv")
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






