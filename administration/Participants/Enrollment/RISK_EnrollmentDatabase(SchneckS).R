#install.packages('P:/Methods/Software/R/StudySupport/Source/StudySupport.tar.gz', repos=NULL, type= 'source')
library(StudySupport)
library(tidyverse)

#Setup for Database info------------------
SurveyID = 'SV_er3BuWp5ZLOjAzP'  
Input_URL = file.path('https://uwmadison.co1.qualtrics.com/jfe/form/', SurveyID)
#Download_URL='https://uwmadison.co1.qualtrics.com'
#Key='et9mgCBpiPT402404Rs0IvRbjG9goUiR0klGcACX'
StaffID = 'SchneckS'



#Download database--------------------------
dR = edbGetCleanDB(SurveyID)
#View(dC)


#Add to database-----------------
edbQualtricsInput(StaffID, Input_URL)


#Get Subject Info--------------------
View(edbGetSubjectRecords(dR, 126))   #all records for a specific SubID (e.g. 2)
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

#get all subID by status
dT = dC %>% select(SubID,Status) %>% filter(Status!='')

#get demographics from qualtrics for annual progress report
DemSurveyID = 'SV_4GfoMwQRJXoTJQ1'
Dem = rdbGetRawDB(DemSurveyID)
Dem = Dem %>% filter(DataType=='Real') %>% select(SubID:DEM_4,-(RA:DEM_1))
#write.table(Dem, 'clipboard', sep='\t')

#Wrap-up---------------------------------
#Final check that database has no errors
#Check that no errors are present when you download the database a final time
dC = edbGetCleanDB(SurveyID)



##CLEANING FUNCTIONS

#Find participants without SMS/Voice files ---------------------------------
RawPath =  'P:/StudyData/RISK/RawData'
DataBasePath = 'P:/StudyData/RISK/Database'
HPath = 'H:/Risk'

#get list of all SubIDs who have folders in RawData ---------------------------------
subIDs = list.dirs(RawPath, full.names = FALSE, recursive = FALSE)
subIDs = enframe(as.list(grep('[[:digit:]].', subIDs, value = TRUE))) %>% select(subID = 'value')



#Counts by visit ---------------------------------

dCounts = dC %>% filter(Visit.Outcome == 'COMPLETED') %>%
  group_by(SubID, Visit.Type) %>% 
  tally() %>%
  spread(Visit.Type, n) %>% 
  select (SubID, SCREEN,INTAKE,FOLLOWUP1, FOLLOWUP2, FINAL) 
# %>%
#   filter(!is.na(FOLLOWUP1))

write.csv(dCounts, file.path(HPath,'SubIDCount.csv'))


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



# #Finished for invite to Dox ---------------------------------
# dF = dC %>% filter(Status=='COMPLETED')
# 
# #open screen battery to find and exclude referrals FROM dox
# RiskScreen = 'SV_8dHQWRHtYVPXjvf'  
# dR = edbGetRawDB(RiskScreen) #inspection showed only c(207,265,256) referred from Dox
# dF = dF %>% filter(!(SubID %in%  c(207,265,256)))
# #get Dox enrollment DB to compare phone numbers
# DoxEID = 'SV_cwfFHPy0qdtuC9v'
# dDox = edbGetCleanDB(DoxEID)
# dNames=dDox %>% select(First,Last,Phone1) 
# dNames$Source='Dox'
# dFNames = dF %>% select(First,Last,Phone1)
# dFNames$Source='Risk'
# dJ = full_join(dNames,dFNames) %>% filter(!(First==''))
# dJFiltered = dJ %>% filter(!(Phone1 %in% c('859-803-2123', '608-359-2888', '608-957-6911',
#                                            '608-515-3965','608-228-5783', '303-280-5835',
#                                            '608-279-6950', '608-468-8551', '615-894-3458',
#                                            '608-239-4386', '608-209-6728', '920-252-2789',
#                                            '608-999-1767', '608-239-6620', '623-262-0290',
#                                            '608-770-4224','608-235-6305', '608-577-4751',
#                                            '608-257-0067', '608-442-3546', '414-526-4099',
#                                            '608-354-3039')))
# 
# dJFiltered = dJFiltered %>% filter(!(Last %in% c('Bormett', 'Daugherty','France',
#                                                  'Feldhus', 'Quella', 'Tobin', 'Webster')))
# # Darrin	Bormett
# # Darrin	Bormetti
# # Seth Daugherty
# # Katy	France	
# # Konner	Feldhus
# # Joshua	Miller	
# # Kerry	Olson	
# # Shane	Quella	
# # Carrie	Tobin	
# # Wesley	Webster	
# dJFiltered[42,3] = 'DELETE'
# dJFiltered[49,3] = 'DELETE'
# dJFiltered = dJFiltered %>% filter(Phone1 !='DELETE') %>% filter(Source !='Dox') %>% filter()
# 
# dMatch = left_join(dJFiltered, dC) %>% select(First,Last,Address,SubID) %>% filter(Address !='')
# 
# write_xlsx(dMatch, 'P:/documentation/administration/recruiting/Ads/2019 risk participant mailing/RiskMail.xlsx')
