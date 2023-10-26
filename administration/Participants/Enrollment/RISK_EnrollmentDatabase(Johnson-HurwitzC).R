#install.packages('P:/Methods/Software/R/StudySupport/Source/StudySupport.tar.gz', repos=NULL, type= 'source')
library(StudySupport)


#Setup for Database info------------------
SurveyID = 'SV_er3BuWp5ZLOjAzP'  
Input_URL = file.path('https://uwmadison.co1.qualtrics.com/jfe/form/', SurveyID)
#Download_URL='https://uwmadison.co1.qualtrics.com'
#Key='et9mgCBpiPT402404Rs0IvRbjG9goUiR0klGcACX'
StaffID = 'Johnson-HurwitzC'



#Download database--------------------------
dC = edbGetCleanDB(SurveyID)
#View(dC)


#Add to database-----------------
edbQualtricsInput(StaffID, Input_URL)


#Get Subject Info--------------------
View(edbGetSubjectRecords(dC, 2))   #all records for a specific SubID (e.g. 2)
View(edbGetSubjectRecords(dC, 2, Type='SCHEDULED'))  #only scheduled/future records


#Get Visit info----------------------
View(edbGetVisits(dC, Start='2017/06/01', End='2020/01/01', VisitType='All'))  #All visits for all subs within specified  window
View(edbGetVisits(dC, Start='2017/06/01', End='2020/01/01', VisitType='Screen'))  #Screen visits for all subs within specified window


#Get Call info----------------------
View(edbGetCalls(dC, Start='2017/06/01', End='2020/01/01', CallType='All'))  #all calls for all subs within specified window
View(edbGetCalls(dC, Start='2017/06/01', End='2020/01/01', CallType='Reminder'))  #reminder calls for all subs within specified window


#Get Active Participants----------------------
View(edbGetActiveParticipants(dC))


#Summary functions----------------------
View(edbSummaryStatus(dC))   #table of Subject Statuses overall and by Sex


#Wrap-up---------------------------------
#Final check that database has no errors
#Check that no errors are present when you download the database a final time
dC = edbGetCleanDB(SurveyID)






library(tidyverse)


#Get final valid subIDs
valid_subs = dC %>% group_by(SubID, Visit.Type) %>% 
  filter(Visit.Outcome == 'COMPLETED') %>%
  tally() %>%
  spread(Visit.Type, n) %>% 
  filter(!is.na(FOLLOWUP1)) %>%
  select (SubID, FOLLOWUP1, FOLLOWUP2, FINAL)
View(valid_subs)


#Get counts by visit ---------------

dCounts = dC %>% group_by(SubID, Visit.Type) %>% 
  filter(Visit.Outcome == 'COMPLETED') %>%
  tally() %>%
  spread(Visit.Type, n) %>% 
  filter(!is.na(FOLLOWUP1)) %>%
  select (SubID, FOLLOWUP1, FOLLOWUP2, FINAL)
  

#Count of FU1
sum(dCounts$FOLLOWUP1, na.rm = TRUE)

#Count of FU2
sum(dCounts$FOLLOWUP2, na.rm = TRUE)

#missing FU2
NoFU2 = dCounts %>% filter(is.na(FOLLOWUP2))
NoFU2$SubID

#Count of finals
sum(dCounts$FINAL, na.rm = TRUE)

#missing FINAL
NoFINAL = dCounts %>% filter(is.na(FINAL))
NoFINAL$SubID

#no FU1
NoFU1 = dC %>% group_by(SubID, Visit.Type) %>% 
  filter(Visit.Outcome == 'COMPLETED') %>%
  tally() %>%
  spread(Visit.Type, n) %>% 
  filter(is.na(FOLLOWUP1)) 

NoFU1$SubID
