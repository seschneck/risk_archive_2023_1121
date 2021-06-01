rm(list=ls())   #clear all objects from workspace

#STAFF SETUP----------------------------------------------
SubID =  215            #List single integer value (e.g.,  5), excluding leading 0s and quotes 
Period = 'Followup2'        #Use All, Followup1, Followup2, FinalVisit

#setup----------------------------------------------------
library(stringr)
library(lmSupport)
#library(xlsx)
library(readxl)
library(tidyverse)
source('P:/StudyData/RISK/Analysis/RISK/fun_RISK.R')

RawPath = 'P:/StudyData/RISK/RawData'
DataBasePath = 'P:/StudyData/RISK/DataBase'

#set start and end dates for period
dDates = read_excel(file.path(RawPath, varPadString(SubID,3), str_c(varPadString(SubID,3), '_VisitDates.xlsx')), na= c('NA', ''))
if (str_to_upper(Period)=='ALL')
{
  StartDate = dDates$Intake
  EndDate =   dDates$FinalVisit
}
if (str_to_upper(Period)=='FOLLOWUP1')
{
  StartDate = dDates$Intake
  EndDate =   dDates$Followup1
}
if (str_to_upper(Period)=='FOLLOWUP2')
{
  StartDate = dDates$Followup1
  EndDate =   dDates$Followup2
}
if (str_to_upper(Period)=='FINALVISIT')
{
  StartDate = dDates$Followup2
  EndDate =   dDates$FinalVisit
}  
rm(dDates)

#covert from numeric excel date to Date class in R
StartDate = as.Date(StartDate, origin = '1899-12-30',tz='America/Chicago')  
EndDate =   as.Date(EndDate, origin = '1899-12-30',tz='America/Chicago')

#Add this subject to database
addSubject2Database(SubID, FALSE, FALSE, TRUE)

#load files and prune to current date range
dC = read_rds(file.path(DataBasePath, varPadString(SubID,3), str_c(varPadString(SubID,3), '_Contacts.rds')))

#dC = removeVarNamePrefix(dC) #simplify variable names by removing Contacts_
#dC$HomePhone = as.double(dC$HomePhone)
#dC$CellPhone = as.double(dC$CellPhone)
#dC$OtherPhone1 = as.double(dC$OtherPhone1)
#dC$OtherPhone2 = as.double(dC$OtherPhone2)

ReportedContacts = c(dC$HomePhone,dC$CellPhone,dC$OtherPhone1,dC$OtherPhone2)
ReportedContacts = ReportedContacts[!is.na(ReportedContacts)]

dV = read_rds(file.path(DataBasePath, varPadString(SubID,3), str_c(varPadString(SubID,3), '_Voice.rds')))

#dV = removeVarNamePrefix(dV) #simplify variable names by removing Voice_
#dV$Phone = as.double(dV$Phone)

dV$Date = as.Date(as.POSIXct(dV$UTC, origin="1970-01-01", tz='America/Chicago'), tz='America/Chicago')
dV = dV[dV$Date>=StartDate,]
dV= dV[dV$Date<=EndDate,]

dS = read_rds(file.path(DataBasePath, varPadString(SubID,3), str_c(varPadString(SubID,3), '_SMS.rds')))

#dS = removeVarNamePrefix(dS) #simplify variable names by removing SMS_
#dS$Phone = as.double(dS$Phone)

dS$Date = as.Date(as.POSIXct(dS$UTC, origin="1970-01-01", tz='America/Chicago'), tz='America/Chicago')
dS = dS[dS$Date>=StartDate,]
dS= dS[dS$Date<=EndDate,]

tV = table(dV$Phone)
dUV= data.frame(Phone=as.double(names(tV)),nVoice=as.double(tV),stringsAsFactors = FALSE)

tS = table(dS$Phone)
dUS= data.frame(Phone=as.double(names(tS)), nSMS=as.double(tS),stringsAsFactors = FALSE)

dU = dfMerge(dUS,dUV, ByX='Phone',ByY ='Phone', AddVars=TRUE)
dU$nSMS[is.na(dU$nSMS)]=0
dU$nVoice[is.na(dU$nVoice)]=0

#select only valid phone numbers
dU = dU[trunc(log10(dU$Phone))+1 ==10,]

#select only numbers NOT already in Reported Contacts
dU = dU[dU$Phone %in% ReportedContacts==FALSE,]

#select on if contated > 1x  in the month
dU$nTot = dU$nSMS+dU$nVoice
dU = dU[dU$nTot>1,]

#sort by nTot
dU = dU[order(dU$nTot,decreasing = TRUE),]

#Remove known staff and survey signal numbers
StaffNumbers = c(6088904796, 6085722496, 3126472185, 3126472169, 3126472154, 3126472125, 3126472161, 3126472178, 3126472139, 3126472126, 3126472175, 3126472400)
dU = dU[!(dU$Phone %in% StaffNumbers),]

#Remove previously reported phone numbers
dU = dU[!(dU$Phone %in% ReportedContacts),]

#format phone numbers as string
if(nrow(dU)>0) dU$Phone = str_c('(',varPadString(varParse(dU$Phone,1000000000,10000000),3),') ',varPadString(varParse(dU$Phone,1000000,10000),3),'-',varPadString(varParse(dU$Phone,1000,1),4))

#SAVE to TMP FOLDER FOR INTERVIEWS
TMPPath = 'P:/StudyData/RISK/TMPInterview'
write_csv(dU, file.path(TMPPath, str_c(varPadString(SubID,3),'_UnreportedContacts.csv')))
View(dU)  #display the dataframe in R
