#RISK weeks of sobriety at intake and lapse rate

library(stringr)
library(lmSupport)
library(StudySupport)
library(xlsx)

#Load data----------------------------
DataPath = 'P:/StudyData/RISK/Analysis/GPS/Data' #These files are the most recent
dYWeek = readRDS(file.path(DataPath, 'YWeek.rds'))
dYDay = readRDS(file.path(DataPath, 'YDay.rds'))

#Get lapse rate by subject----------------------------------
dYLapse = data.frame(SubID=unique(dYDay$SubID),TotalL = NA, Mean=NA, TotalObs=NA)
for(i in unique(dYDay$SubID)){
  dYi = dYDay[dYDay$SubID==i,]
  dYLapse$TotalL[dYLapse$SubID==i] = sum(dYi$Lapse)
  dYLapse$Mean[dYLapse$SubID==i] = sum(dYi$Lapse)/nrow(dYi)
  dYLapse$TotalObs[dYLapse$SubID==i] = nrow(dYi)
}
#Pad zeros
dYLapse$SubID = varPadString(dYLapse$SubID, 3)

#Get weeks of sobriety at intake-------------------------------
#Load Database
dScreen = read.csv('P:/StudyData/RISK/Database/Screen.csv', stringsAsFactors = FALSE)

#remove unneeded vars
dScreen = dScreen[,c('SubID','AUH_8_Month','AUH_8_Day','AUH_8_Year')]

#Pad days with 0s
dScreen$AUH_8_Day= varPadString(dScreen$AUH_8_Day, 2)
#Pad months with 0s
dScreen$AUH_8_Month = varPadString(dScreen$AUH_8_Month, 2)

#Combine AUH vars to one date
for (i in 1:length(dScreen$SubID)){
  dScreen$SobDate[i] = paste(dScreen$AUH_8_Year[i],dScreen$AUH_8_Month[i],dScreen$AUH_8_Day[i],sep='-')
}

#Change to date format
dScreen$SobDate= strptime(as.character(dScreen$SobDate), "%Y-%m-%d")

#Get screen date -----------------------------------
#Load Visit Dates csv for each person, add to dscreen

#Pad SubIDs with 0s
dScreen$SubID = varPadString(dScreen$SubID, 3)

#Remove 167 - EMPTY DATABASE FOLDER, HAS DATA IN RAWDATA
dScreen = dScreen[!(dScreen$SubID=="167"),]

for (aSub in dScreen$SubID){
  #Check if its in cleaned database. Get discontinued Visit dates too?
    if (aSub %in% list.files('P:/StudyData/RISK/Database')){
    dVD = read.csv(file.path('P:/StudyData/RISK/Database',aSub,str_c(aSub,'_VisitDates.csv')), stringsAsFactors = FALSE)
    }else{dScreen = dScreen[-which(dScreen$SubID==aSub),] #If they are not in database, they did not do FU 1 and wouldnt have lapse data
    }
  
  dScreen$StartDate[dScreen$SubID == aSub] = dVD$Screen
}

#Compare Visit date to sobriety date------------------------------

#Remove old vars
dScreen$AUH_8_Month = NULL
dScreen$AUH_8_Day = NULL
dScreen$AUH_8_Year = NULL

#as.Date for better calculations
dScreen$StartDate = as.Date(dScreen$StartDate, format ="%Y-%m-%d")
dScreen$SobDate = as.Date(dScreen$SobDate, format ="%Y-%m-%d")

#subtract dates - get answer in weeks
dScreen$Diff = difftime(dScreen$StartDate, dScreen$SobDate, units = c("days"))

#Add dYLapse to dScreen
dScreen = dfMerge(dScreen,dYLapse, ByX='SubID', ByY='SubID')

#Remove problem Subs

#Remove 163 hasnt completed FU 1
dScreen = dScreen[!(dScreen$SubID=="163"),]


#Model --------------------------------------
m1=lm(Mean ~ 1 + Diff, data=dScreen)
modelSummary(m1, t=FALSE)
modelEffectSizes(m1)
confint(m1)

#Graph-----------------
windows()
plot(dScreen$Diff, dScreen$Mean, col = 'pink', pch = 19)


