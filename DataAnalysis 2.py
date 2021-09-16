#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep 14 13:54:18 2021

@author: akhilachowdarykolla
"""


import pandas as pd
import numpy as np



condensedDataCoaching = pd.DataFrame(pd.read_excel("/Users/akhilachowdarykolla/Downloads/Condensed Coaching logs Fall 2017- Spring 2021.xlsx"))
# print(condensedDataCoaching.columns)
condensedDataCoachingUpdated = condensedDataCoaching[['Timestamp', 'Date of Event/Visit ','Duration of Event', 'Interaction Type',
                        'State District ID', 'DISTRICTS', 'Collaborative teams','Common formative assessment',
        'Data-based decision making','Effective teaching/learning practices','Instructional Leadership',
        'School-based implementation coaching','Collective teacher efficacy',
        'MMD/DCI expectations/logistics/DESE specifics','CWIS']]

cwisData = pd.DataFrame(pd.read_excel("/Users/akhilachowdarykolla/Desktop/development/PredictiveModelling/cwis_survey data updated.xlsx"))
cwisDataUpdated = cwisData[['district_name', 'building_name','State District ID', 'State School ID','role',
        'common_practices_can_statements', 'common_practices_student_work', 'common_practices_self_assessment', 
        'common_practices_receive_feedback','common_practices_student_feedback',
        'common_practices_state_criteria','ETL AVERAGE']]

merged_inner = pd.merge(left=condensedDataCoachingUpdated, right=cwisDataUpdated, left_on='State District ID', right_on='State District ID')

print("shape of Condensed data", condensedDataCoachingUpdated.shape)
print("shape of cwisData",cwisDataUpdated.shape)
print("shape of Merged Data",merged_inner.shape)
# merged_inner.to_csv('/Users/akhilachowdarykolla/Desktop/Coaching+CWISUpdated.csv')
# print("Check the file")
merged_inner.drop_duplicates()



merged_inner.dropna(inplace=True)
print("shape of Merged Data",merged_inner.shape)

NCESdata = pd.DataFrame(pd.read_excel("/Users/akhilachowdarykolla/Desktop/development/PredictiveModelling/Copy of ncesdata_ECCDA30A (1).xlsx"))
NCESdataUpdated = NCESdata[['State School ID', 'NCES District ID','State District ID', 'District','Locale Code*',
        'Locale*', 'Students*', 'Teachers*', 
        'Student Teacher Ratio*','Free Lunch*',
        'Reduced Lunch*']]

merged_NCES_CWIS_Coaching = pd.merge(left=merged_inner, right=NCESdataUpdated, left_on='State District ID', right_on='State District ID')
merged_NCES_CWIS_Coaching.drop_duplicates()
merged_NCES_CWIS_Coaching.dropna(inplace=True)
print("shape of merged_NCES_CWIS_Coaching Data",merged_NCES_CWIS_Coaching.shape)
# merged_NCES_CWIS_Coaching.to_csv('/Users/akhilachowdarykolla/Desktop/ReducedCoaching+CWISUpdated+NCES.csv')
# print("Check the file")
# df = df[df['EPS'].notna()]
CombinedLogs = pd.DataFrame(pd.read_csv("/Users/akhilachowdarykolla/Desktop/ReducedCoaching+CWISUpdated+NCES.csv"))
print(CombinedLogs.columns)
# print(condensedDataCoaching.columns)

