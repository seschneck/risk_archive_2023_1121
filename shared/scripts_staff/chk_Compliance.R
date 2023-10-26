#install.packages('P:/Documentation/Software/R/StudySupport/Source/StudySupport.tar.gz', repos=NULL, type= 'source')
#install.packages('P:/Documentation/Software/R/lmSupport/Source/lmSupport.tar.gz', repos=NULL, type= 'source')

#STAFF SETUP----------------------------------------------
#set to NULL, an integer, or vector of numeric SubIDs if we want to reduce a subset of SubIDs
nSubIDs = 270        #= 5 OR = c(1,2,5) OR = NULL to reduce ALL SubIDs
Period = 'Followup2'        #All, Followup1, Followup2, FinalVisit

#General setup----------------------------
source('P:/StudyData/RISK/Analysis/RISK/fun_RISK.R')

InPath = 'P:/StudyData/RISK/RawData'

#If only one subject, first remake their data
#Assumes that if multiple subjects are checked, their data have been updated/made already
if(length(nSubIDs)==1){
  addSubject2Database(nSubIDs, TRUE, TRUE, TRUE)  
  #addSubject2Database(nSubIDs, FALSE, FALSE, TRUE)  
  
}

#Get all SubIDs if NULL specified above and delete database
if(is.null(nSubIDs)){
  nSubIDs = list.dirs(path=InPath, full.names = FALSE, recursive = FALSE)
  nSubIDs = suppressWarnings(as.numeric(nSubIDs))
  nSubIDs = subset(nSubIDs,!is.na(nSubIDs))
  StartDate = NULL
  EndDate = NULL
}

#Subject looping---------------------------
dC = data.frame(NULL)
for (nASubID in nSubIDs){
  print(str_c('Checking SubID: ', varPadString(nASubID,3)))
  dRow = chkDataIntegrity(nASubID,Period)
  if(nrow(dC)==0) dC = dRow  else dC = bind_rows(dC,dRow)
}

View(dC)
