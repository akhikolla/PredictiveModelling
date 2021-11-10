# coachingdata <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/Coaching logs Fall 2017- Spring 2021/Condensed columns-Table 1.csv")#,header = TRUE,check.names=TRUE )
# required.coaching.cols <- coachingdata[,c(5,6,8,9,21,23,25,27,29,31,33,41,42,43,44,46)]
# 
# required.coaching.cols$Duration.of.Event[required.coaching.cols$Duration.of.Event == ""] <- 0
# required.coaching.cols$Interaction.Type[required.coaching.cols$Interaction.Type == ""] <- "None"
# 
# 
# required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "Yes" |
#                                              required.coaching.cols$Collaborative.teams == "yes" ] <- 1
# 
# required.coaching.cols$Collaborative.teams[required.coaching.cols$Collaborative.teams == "No" |
#                                              required.coaching.cols$Collaborative.teams == "" ] <- 0
# 
# for(j in 5:ncol(required.coaching.cols)){
#   required.coaching.cols[,j][is.na(required.coaching.cols[,j])] <- 0
#   required.coaching.cols[,j][required.coaching.cols[,j] == "Yes" |
#                                required.coaching.cols[,j] == "yes"  ] <- 1 
#   required.coaching.cols[,j][required.coaching.cols[,j] == "No" |
#                                required.coaching.cols[,j] == ""] <- 0
#   
# }

#nces data
newncesupdate <-read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/UpdatedNCES.csv")#,check.names=TRUE)
required.nces.cols <- newncesupdate[-c(1,3,5,6,7,9,10,11,12,13,14,15,16,17)]


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


class(required.nces.cols)
class(required.coaching.cols)

nces.dt <- as.data.table(required.nces.cols)
coaching.dt <-as.data.table(required.coaching.cols)

colnames(nces.dt)[3] <- "Districts"

colnames(coaching.dt)[4] <- "Districts"

coaching.dt$Districts <- tolower(coaching.dt$Districts) #5924 obs. of  16 variables
nces.dt$Districts <- tolower(nces.dt$Districts) #2456 obs. of  25 variables


# uy <- nces.dt %>% distinct(State.District.ID, .keep_all = TRUE)
# ndt <-coaching.dt[uy,on=.(State.District.ID),nomatch = NULL]
# 
# ndt_dup <-coaching.dt[nces.dt,on=.(State.District.ID,Districts),nomatch = NULL]


cwisdata <- read.csv("/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/newcwis_survey data/cwis_survey data-Table 1.csv")
required.cwis.cols <- cwisdata[-c(1,2,5,6,12,13,14,15)]
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


nces.dt <- as.data.table(required.nces.cols)
str(nces.dt) #2456 obs. of  26 variables
cwis.dt <- as.data.table(required.cwis.cols)
str(cwis.dt) #80267 obs. of  98 variables

#combining three tables
#uy <- nces.dt %>% distinct(State.District.ID, .keep_all = TRUE)
#nces+cwis
nces.cwis.dt <-nces.dt[cwis.dt,on=.(State.School.ID ),nomatch = NULL]
#nces+cwis+coaching
coaching.aggregrate.districts <- districts.aggregate.dt[nces.cwis.dt,on=.(State.District.ID ),nomatch = NULL]
write.csv(coaching.aggregrate.districts,"/Users/akhilachowdarykolla/Documents/Coding/development/PredictiveModelling/coaching_nces_aggregrate_districts.csv", row.names = FALSE)



nces.cwis.state.school.IDs <- unique(nces.cwis.dt$State.School.ID)
length(nces.cwis.state.school.IDs)
nces.school.IDs <- unique(nces.dt$State.School.ID)
length(nces.school.IDs)

nces_cwis_intersect_SchoolIDS <- intersect(nces.cwis.state.school.IDs,nces.school.IDs)
length(nces_cwis_intersect_SchoolIDS)

nces_cwis_outersect_SchoolIDS <- outersect(nces.cwis.state.school.IDs,nces.school.IDs)
length(nces_cwis_outersect_SchoolIDS)


outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}


nces.school.IDs <- unique(nces.dt$State.School.ID)
length(nces.school.IDs)

cwis.school.IDs <- unique(cwis.dt$State.School.ID)
length(cwis.school.IDs)

nces.cwis.school.IDs <- unique(nces.cwis.dt$State.School.ID)
length(nces.cwis.school.IDs)


nces_cwis_intersect_SchoolIDS <- intersect(cwis.school.IDs,nces.school.IDs)
length(nces_cwis_intersect_SchoolIDS)

nces_cwis_outersect_SchoolIDS <- outersect(nces.cwis.state.school.IDs,nces.school.IDs)
length(nces_cwis_outersect_SchoolIDS)


nces_cwis_outersect_SchoolIDS <- outersect(cwis.school.IDs,nces_cwis_intersect_SchoolIDS)
length(nces_cwis_outersect_SchoolIDS)
# 
# > nces_cwis_outersect_SchoolIDS
# [1] ""                      "MO-014129-7500014129"  "MO-016096-0016096"     "MO-019152-11111019152" "MO-019152-4050019152" 
# [6] "MO-024087-????024087"  "MO-037037-1000037037"  "MO-048072-1060048072"  "MO-048072-4020048072"  "MO-048072-4025048072" 
# [11] "MO-049142-3010049142"  "MO-058112-7500058112"  "MO-062072-9999062072"  "MO-078002-4020078002"  "MO-080125-080125"     
# [16] "MO-085044-2050085044"  "MO-085046-085046"      "MO-092089-4130092089"  "MO-096089-3000096089"  "MO-096089-4010096089" 
# [21] "MO-096089-4100096089"  "MO-096089-4280096089"  "MO-097129-0097129"     "MO-099082-7500099082"  "MO-105124-3000105124" 
# [26] "MO-108144-108144"      "MO-115906-1000115906"  "MO-118118-118118"


nces.school.IDs <- unique(nces.dt$State.School.ID)
length(nces.school.IDs)

coaching.district.IDs <- unique(districts.aggregate.dt$State.District.ID)
length(coaching.district.IDs)

nces.cwis.district.IDs <- unique(nces.cwis.dt$State.District.ID)
length(nces.cwis.district.IDs)

nces_cwis_outersect_DistrictIDS <- intersect(nces.cwis.district.IDs,coaching.district.IDs)
length(nces_cwis_outersect_DistrictIDS)

nces_cwis_outersect_DistrictIDS <- outersect(nces.cwis.district.IDs,coaching.district.IDs)
length(nces_cwis_outersect_DistrictIDS)


districts.aggregate.dt 
arranged<-districts.aggregate.dt[order(rank(State.District.ID))]

newrow <- 
data1 <- rbind(data, new_row)              # Apply rbind function
data1                                      # Print updated data
#   x1 x2 x3
# 1  1  4  1
# 2  2  3  1
# 3  3  2  1
# 4  4  1  1
# 5 77 88 99



required.cwis.cols <- cwisdata[-c(1,2,4,5,6,12,13,14,15)]


required.interaction.cols <- coachingdata[,c(4,5,6,7,8,9)]

library(data.table)    
consistency.Metric <- data.table(District=numeric(), year=numeric(), ETL.FALL_Aug_Mar=numeric(),ETL.Spring.April_July=numeric(),Improvement=ETL.FALL_Aug_Mar-ETL.Spring.April_July)
consistency.Metric

consistency.Metric <- data.table(District=numeric(), year=numeric(), MONTH=numeric(),ETL.FALL_Aug_Mar Avg of ETL for that months from CWIS,ETL.Spring.April_July=,Improvement=ETL.FALL_Aug_Mar-ETL.Spring.April_July)
consistency.Metric

District year semester ETL_Domain
10090 18-19    fall      mean(all fall months)

#how imp rate of coaching is used for predictiong ETL average

# unc <- unique(nces.dt[, .(State.District.ID, Districts), nomatch=0 ])

# ls <- as.list(coaching.dt$Districts)
# ns <- as.list(nces.dt$Districts)

# setkeyv(nces.dt, c('State.District.ID','Districts'))
# uniqdat <- subset(unique(nces.dt))
# class(nces.dt)
# class(coaching.dt)

# dfs <- nces.dt

# uncs = nces.dt.drop_duplicates(c())
# dfs.drop_duplicates(subset=['State.District.ID','Districts'], keep='last')
# unsv <- nces.dt[duplicated('State.District.ID')]



# setkey(nces.dt,State.District.ID)
# setkey(coaching.dt,State.District.ID)



