  #STAFF SETUP----------------------------------------------
  nSubID =  240          #List single integer value (e.g.,  5), excluding leading 0s and quotes 
  Period = 'FinalVisit'        #Use All, Followup1, Followup2, FinalVisit
  
  #setup----------------------------------------------------
  #install.packages('P:/Methods/Software/R/Packages/Source/ggmap.zip', repos=NULL, type= 'source') #2.7.9 installer
  library(ggmap)  #geocode, revgeocode, register_google (which requires 2.7.9)
  #API key to override query limit
  register_google(key = 'AIzaSyBBwIG1tC4QZi5QrTnhXXLktIPD8NlVVI0')
  
  library(readxl)
  library(lmSupport)
  library(stringr)
  library(tidyverse)
  source('P:/StudyData/RISK/Analysis/RISK/fun_GPS.R')
  source('P:/StudyData/RISK/Analysis/RISK/fun_RISK.R')
   
  
  RawPath = 'P:/StudyData/RISK/RawData'
  DataBasePath = 'P:/StudyData/RISK/DataBase'
  
  sSubID = varPadString(nSubID,3)
  
  #set start and end dates for period
  dDates = read_excel(file.path(RawPath, sSubID, str_c(sSubID, '_VisitDates.xlsx')), na= c('NA', ''))
  if (str_to_upper(Period)=='ALL'){
    StartDate = dDates$Intake
    EndDate =   dDates$EndStudy
  }
  if (str_to_upper(Period)=='FOLLOWUP1'){
    StartDate = dDates$Intake
    EndDate =   dDates$Followup1
  }
  if (str_to_upper(Period)=='FOLLOWUP2'){
    StartDate = dDates$Followup1
    EndDate =   dDates$Followup2
  }
  if (str_to_upper(Period)=='FINALVISIT'){
    StartDate = dDates$Followup2
    EndDate =   dDates$EndStudy
  }  
  rm(dDates)
  
  #covert from numeric excel date to Date class in R
  StartDate = as.Date(StartDate, origin = '1899-12-30',tz='America/Chicago')  
  EndDate =   as.Date(EndDate, origin = '1899-12-30',tz='America/Chicago')
  
  #Add this subject to database
  #addSubject2Database(nSubID, FALSE, FALSE, TRUE)
  
  #load reported locations----------------------------------------------------
  dLoc = read_rds(file.path(DataBasePath, sSubID, str_c(sSubID, '_Locations.rds')))
  
  #simplify variable names by removing Locations_
  #dLoc = removeVarNamePrefix(dLoc)
  dLoc$PopupLabel = str_c(dLoc$Type, 'at', dLoc$StreetAddress, sep=' ')
  
  #load GPS Moves data----------------------------------------------------
  dGPS = read_rds(file.path(DataBasePath, sSubID, str_c(sSubID, '_GPS.rds')))
  
  dGPS$Time= as.POSIXct(dGPS$Time,format='%Y-%m-%d %H:%M:%S', tz = "America/Chicago")
  dGPS$Date = as.Date(dGPS$Time, tz='America/Chicago')
  dGPS = dGPS[dGPS$Date>=StartDate,]
  dGPS= dGPS[dGPS$Date<=EndDate,]
  
  #Find places that are unreported----------------------------------------------------
  dU = gpsNearestReference(dGPS,dLoc)
  dU = dU[-which(dU$NearestReference<=60),]
  
  #find unreported places > 1X--------------------------------------------------------
  dU = gpsCountMatchs(dU,60)
  dU = dU[-which(dU$NumMatchs==1),]
  
  #Add Index
  nPlaces = sum(dU$Type=='PLACE', na.rm=TRUE)
  dU$Index = NA
  dU$Index[!is.na(dU$Type) & dU$Type=='PLACE'] = 1:nPlaces
  
  # #plot---------------------------------------------------------------------------------
  # m=gpsPlot(dU)
  # m = addCircleMarkers(m, data = dLoc, lng = ~Long, lat = ~Lat, radius = 1.5, color = 'green', opacity = 1, popup = ~PopupLabel, group = 'Reported Places')
  # m = addLayersControl(m, baseGroups = c('Color', 'B&W'),overlayGroups = c('Reported Places', 'Places', 'Tracks'), options = layersControlOptions(collapsed = FALSE))
  # m = removeControl(m,layerId='legend')
  # m = addLegend(m, "bottomright", colors = c('purple', 'blue', 'cyan', 'yellow', 'orange', 'red', 'green'), labels = c('walking', 'runnng', 'cycling', 'transport', 'airplane', 'unreported place', 'reported place'),
  #               title = 'Legend',opacity = 1)
  # m
  
  #Get addresses of unreported locations------------
  dUP = dU[dU$Type =='PLACE',]  #get only places
  
  if(nrow(dUP)>0){  #only do if there are unreported locations to check
    dUP$Address=''
    for (i in 1:nrow(dUP)){
      Address= revgeocode(c(dUP$Long[i], dUP$Lat[i]),output='address')
      dUP$Address[i]=ifelse(is.na(Address), 'NA', Address)
      #cludge to fix weird google bug with NAs returned occassionally
      while(dUP$Address[i]=='NA'){
        Address = revgeocode(c(dUP$Long[i], dUP$Lat[i]),output='address')
        dUP$Address[i]=ifelse(is.na(Address), 'NA', Address)
      }
    }
    
    #sort on address
    dUP= dUP[order(dUP$Address),]
  }
  
  #remove unnecessary variables
  dUP$UTC=NULL
  dUP$NextDist=NULL
  dUP$Date=NULL
  dUP$AppSource=NULL
  dUP$Type=NULL
  dUP$DataType=NULL
  dUP$SpeedKMH=NULL
  dUP$AltitudeMeters=NULL
  dUP$Direction=NULL
  dUP$RawOrigTime=NULL
  
  #remove duplicate addresses
  dUP = as_tibble(dUP)
  dUP = distinct(dUP, Address, .keep_all=TRUE)
  
  #SAVE to TMP FOLDER FOR INTERVIEWS
  TMPPath = 'P:/StudyData/RISK/TMPInterview'
  write_csv(dUP, file.path(TMPPath, str_c(sSubID,'_UnreportedLocations.csv')))
  View(dUP)  #display the dataframe in R
