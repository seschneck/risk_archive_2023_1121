#Setup ---------------------------------------------------------------------------------------
#script that makes the Y lapse labels and the X features for all valid subjects

#source('P:/StudyData/RISK/Analysis/RISK/fun_ML.R')  #for makedataframes()
source('P:/StudyData/RISK/Analysis/GPS/fun_makeGPSFeatures.R')   #for makeGPSFeatures()

DataPath = 'P:/StudyData/RISK/Analysis/Data/GPS'

LastSub = 185 #enter last person to complete FU 1 -- find this by checking the Visit dates in database folders

#Functionfor makeY for day -------------------------------------------------
#Make Y dataframe with lapse labels and N with count of labels by SubID
makeY = function(dE,Period='Day')
{
  RawDatabasePath = 'P:\\StudyData\\RISK\\database'
  
  #make long sequence of days from study start to study end
  Dates=seq(as.POSIXct('2017-01-01 00:00:00', tz='America/Chicago'),as.POSIXct('2020-12-12 00:00:00', tz='America/Chicago'),by='days')
  Dates[hour(Dates)==1] = Dates[hour(Dates)==1] - hours(1) #correct for daylight savings
  Dates = as.numeric(Dates) #convert to unix time
  
  if(Period=='Day'){
    WinWidth = 60*60*24  #seconds in a day
  }else{
    WinWidth = 60*60*24*7  #seconds in a week
  }
  
  dY = data.frame(NULL) #initial empty dY
  
  #dE = dE[,c('UTC', 'SubID', 'EMA_1', 'EMA_1.1', 'EMA_1.5', 'EMA_8','EMA_9','EMA_10', 'Type')] #Sarah added in last 4 vars to look at expectations with lapses
  dE = dE[,c('UTC', 'SubID', 'EMA_1', 'EMA_1.1', 'EMA_1.5')]
  dE$EMA_1.1 = as.numeric(dE$EMA_1.1) #convert lapse times to unix time
  
  SubIDs = unique(dE$SubID)
  for(ASubID in SubIDs){
    dSub = subset(dE,SubID==ASubID)
    
    #Open visit dates to get start and end dates
    dVisitDates = read.csv(file.path(RawDatabasePath,varPadString(ASubID,3), str_c(varPadString(ASubID,3), '_VisitDates.csv')), header=TRUE, as.is=TRUE)
    StudyStartDate = as.numeric(as.POSIXct(dVisitDates$StartStudy, tz='America/Chicago'))
    StudyEndDate = as.numeric(as.POSIXct(dVisitDates$EndStudy, tz='America/Chicago'))
    
    #Subset dates down to relevant for this Sub
    #Start on day two b/c no features before that
    #end on last day for day period and 6 days before for week so full period at end
    SubDates = Dates[Dates>StudyStartDate & Dates<=(StudyEndDate-WinWidth+86400)]
    
    #Check that there are dates for this subject
    if(length(SubDates>0)){
      dSubY = data.frame(SubID = ASubID, UTC=SubDates, Lapse=0) #make subjects dY with Lapse=0
      
      #Update lapses to 1 for lapse events
      Lapses = dSub$EMA_1.1[!is.na(dSub$EMA_1.1)]
       FutureIntent = dSub$EMA_1.5[!is.na(dSub$EMA_1.1)]#get future intent too
       #When more than one lapse on same day, remove earlier events
       #when using as.Date wrapped around as.POSIXct, need to specify tz for as.Date!! - SJS
      LastEvents = duplicated(as.Date(as.POSIXct(Lapses,origin = "1970-01-01"), tz='America/Chicago'), fromLast=TRUE)==FALSE
     Lapses = Lapses[LastEvents]
      FutureIntent = FutureIntent[LastEvents]
      
      
      if(length(Lapses)>0){
        for(i in 1:length(Lapses))
        {
          dSubY$Lapse[dSubY$UTC<=Lapses[i] & dSubY$UTC+WinWidth>Lapses[i]] = 1
          if(Period=='Day') dSubY$FutureIntent[dSubY$UTC<=Lapses[i] & dSubY$UTC+WinWidth>Lapses[i]] = FutureIntent[i]
        }
      }
      
      #merge subject into dY
      if(nrow(dY)>0)
      {
        dY = dfMerge(dY,dSubY, AddVars=FALSE)
      } 
      else
      {
        dY = dSubY
      }
    }
    else
    {
      warning(str_c('No valid dates for SubID: ', ASubID))
    }
  }
  
  dY = dY[order(dY$SubID,dY$UTC),]
  
  return(dY)
}


#Make Y --------------------------------------------------------------------------
#Make EMA data frame
makeDataframes(Type = 'EMA', OutDataframePath = DataPath)
dEMA =    readRDS(file.path(DataPath,'EMA.rds'))

#Make list of Valid SubIDs
SubIDs = unique(dEMA$SubID)
#Select SubIDs for people who have completed FU 1
SubIDs = SubIDs[SubIDs<LastSub] #Check visitdates file in database folders, compare to today
SubIDs
length(SubIDs)

#Make Y for Day
dY = makeY(dEMA,'Day') 
dY = dY[dY$SubID %in% SubIDs,]  #Select subjects with at LEAST follow-up 1 completed
table(dY$SubID)
saveRDS(dY,file.path(DataPath,'YDay.rds')) #save Y
rm(dY)

#Make Y for Week
dY = makeY(dEMA,'Week')
#shouldn't we remove subIDs before making dEMA? Get warnings for people without EMA
dY = dY[dY$SubID %in% SubIDs,]  #Select subjects with at LEAST follow-up 1 completed
table(dY$SubID)
saveRDS(dY,file.path(DataPath,'YWeek.rds'))  #save Y

#Make X--------------------------------------------------------------------
#Make GPS and locations reports data frames
makeDataframes(Type = 'GPS', OutDataframePath = DataPath)

dGPS =readRDS(file.path(DataPath,'GPS.rds'))

#Restrict to list of Valid SubIDs from YWeek
dGPS = dGPS[dGPS$SubID %in% SubIDs,]
table(dGPS$SubID)

dX = NULL  #empty feature matrix (retained so that this function can be used with other functions that add other Features to X)
dX = makeGPSFeatures(Y=dY, X=dX, dGPS=dGPS)
saveRDS(dX,file.path(DataPath,'XWeek.rds'))
