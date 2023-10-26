#DeviceID = '11965923'  #John
DeviceID = '11965978'
StartDate ='2018-07-19'
EndDate = '2018-07-20'
  

#libraries and source
library(stringr)
library(lmSupport)
source('P:/StudyData/RISK/Analysis/Dev/Scripts/fun_GPS.R')

#Get FollowMee trackpoints and process
dNew =  apiFollowMeeGPS(DeviceID,StartDate,EndDate)

#Rename Lat/Lon,Type, and add empty Type
dNew = varRename(dNew,'Latitude','Lat')
dNew = varRename(dNew,'Longitude','Long')
dNew = varRename(dNew,'Type','DataType')
dNew = varRename(dNew,'Date', 'Time')
dNew = varRename(dNew,'Speed(km/h)', 'SpeedKMH')
dNew = varRename(dNew,'Altitude(m)', 'AltitudeMeters')

#Add/Delete variables to match Moves
dNew$Type = ''
dNew[,'Speed(mph)'] = NULL
dNew[,'Altitude(ft)'] = NULL
dNew$AppSource = 'FOLLOWMEE'

#Process date variables
dNew$RawOrigTime = dNew$Time #save original timestamp
dNew$Time = str_c(str_sub(dNew$Time,1,22),'00')  #reformat timezone stamp
dNew$Time=as.POSIXct(dNew$Time,format='%Y-%m-%dT%H:%M:%S%z',tz='America/Chicago')

#Calc NextDist/Next Time
dNew$NextTime = NA
dNew$NextTime[1:(nrow(dNew)-1)] = round(difftime(dNew$Time[2:(nrow(dNew))], dNew$Time[1:(nrow(dNew)-1)], units='mins'),1)
dNew$NextDist = NA
dNew$NextDist[1:(nrow(dNew)-1)] = round(distGeo(dNew[2:(nrow(dNew)),c('Long', 'Lat')], dNew[1:(nrow(dNew)-1),c('Long', 'Lat')]),0)

#reorder columns
dNew = dNew[, c("Lat", "Long", "Time", "NextTime", "NextDist",  "Accuracy",
                "Type", "AppSource", "DataType", "SpeedKMH", "AltitudeMeters",
                "Direction", "RawOrigTime")]
dGPS = dNew[1:(nrow(dNew)-1),]

dGPS$Labels = str_c(1:nrow(dGPS), '; Time: ',dGPS$NextTime,'; Dist: ',dGPS$NextDist,
                           '; Acc: ',dGPS$Accuracy,'; Sd: ',ifelse(is.na(dGPS$SpeedKMH),'NA',dGPS$SpeedKMH))
# #3 minute place definition
# dGPS$Type = ifelse(dGPS$NextTime>3,'Place', 'Movement')
# m3=plotTrackPoints(Lat=dGPS$Lat,Long = dGPS$Long, Labels = dGPS$Labels,Types=dGPS$Type)
# m3


#Combined place definition
dP = dGPS
dP$Type='Movement'
dP$Count = 1
DistTol = 65
iRow = 1
while(iRow<=nrow(dP)){
  message(str_c('Checking iRow: ', iRow))
  if(dP$NextTime[iRow]>3){  #found a starting place
    dP$Type[iRow] = 'Place'
    
    #determine distance to entry before and after if they exist
    if(iRow>1) {
      BDist = distGeo(c(dP$Long[iRow],dP$Lat[iRow]),c(dP$Long[iRow-1],dP$Lat[iRow-1]))
    }else{
      BDist = Inf
    }
    if(iRow<nrow(dP)) {
      ADist = distGeo(c(dP$Long[iRow],dP$Lat[iRow]),c(dP$Long[iRow+1],dP$Lat[iRow+1]))
    }else{
      ADist = Inf
    }
    
    Update = FALSE
    #if BDist is closer and within DistTol update iRow entry with weighted mean including entry before
    if(BDist<ADist && BDist <=DistTol){
      CombinePoints = matrix(cbind(dP$Long[(iRow-1):iRow],dP$Lat[(iRow-1):iRow]), nrow=2,ncol=2)
      NewPoints = geomean(xy=CombinePoints,w=dP$Count[(iRow-1):iRow])
      dP$Long[iRow]=NewPoints[1,1]
      dP$Lat[iRow]=NewPoints[1,2]
      dP$Count[iRow]=dP$Count[iRow]+1
      dP$Time[iRow] = dP$Time[iRow-1]
      dP = dP[-(iRow-1),] #remove previous entry
      iRow = iRow-1 #account for removal
      Update=TRUE
    }
    
    #if ADist is closer and within DistTol update iRow entry with weighted mean including entry after
    if(ADist<=BDist && ADist <=DistTol){
      CombinePoints = matrix(cbind(dP$Long[iRow:(iRow+1)],dP$Lat[iRow:(iRow+1)]), nrow=2,ncol=2)
      NewPoints = geomean(xy=CombinePoints,w=dP$Count[iRow:(iRow+1)])
      dP$Long[iRow]=NewPoints[1,1]
      dP$Lat[iRow]=NewPoints[1,2]
      dP$Count[iRow]=dP$Count[iRow]+1
      dP = dP[-(iRow+1),] #remove next entry
      Update=TRUE
    }
    if(!Update) iRow = iRow+1
    
  }else{
    iRow =  iRow+1
  }
}
#Recalc NextDist/Next Time (need to recalc for last value previously NA)
dP$NextTime = NA
dP$NextTime[1:(nrow(dP)-1)] = round(difftime(dP$Time[2:(nrow(dP))], dP$Time[1:(nrow(dP)-1)], units='mins'),1)
dP$NextDist = NA
dP$NextDist[1:(nrow(dP)-1)] = round(distGeo(dP[2:(nrow(dP)),c('Long', 'Lat')], dP[1:(nrow(dP)-1),c('Long', 'Lat')]),0)
dP$Labels = str_c(rownames(dP), '; Time: ',dP$Time,'; Duration: ',dP$NextTime, '; Count: ', dP$Count)


mP=plotTrackPoints(Lat=dP$Lat,Long = dP$Long, Labels = dP$Labels,Types=dP$Type)
mP
                