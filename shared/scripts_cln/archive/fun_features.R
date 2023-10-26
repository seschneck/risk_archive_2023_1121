#libraries needed for ML functions--------
library(lmSupport)
library(lubridate)
library(stringr)
library(timeDate)

#devtools::install_github("dkahle/ggmap", ref = "tidyup") # Code to install 2.7.9 branch of ggmap
library(ggmap)  #geocode, revgeocode, register_google (which requires 2.7.9)

library(geosphere)  #distGeo
library(tm)  #text mining
#library(qdap) #replace_abbreviation, replace_contraction, replace_symbol
library(text2vec)
library(iterators)
library(doParallel)
library(tidyverse)
#library(SnowballC) #for stemming

#make_x-------------
#Make x dataframe with all features
make_x = function(period, y, id=NULL, sr=NULL, ema=NULL, visits=NULL, dates=NULL, gps=NULL, 
                 voice=NULL, sms=NULL, audio=NULL)
{
  x = NULL
  
  if(!is.null(id)){
    print('Adding ID features to x')
    x = add_x_ID(y, x, id)
  }
  
  if(!is.null(sr)){
    print('Adding SR features to x')
    x = add_x_SR(y, x, sr)
  }
  
  if(!is.null(ema)){
    print('Adding EMA features to x')
    x = add_x_EMA(y, x, ema)
  }
  
  if(!(is.null(ema) || is.null(visits) || is.null(dates)) ){
    print('Adding GEN features to x')
    x = add_x_GEN(period, y, x, ema, visits, dates)
  }
  
  if(!is.null(gps)){
    print('Adding GPS features to x')
    x = add_x_gps(y, x, gps)
  }

  if(!is.null(voice)){
    print('Adding Voice features to x')
    x = add_x_Voice(y, x, voice)
  }
  
  if(!is.null(sms)){
    print('Adding SMS Meta features to x')
    x = add_x_SMSMeta(y, x, sms)
  }
  
  if(!is.null(audio)){
    print('Adding Audio text features to x: TEMP DISABLE')
   # x = add_x_Audio(y, x, audio)
  }
  
  return(x)
}


#add_x_ID------------------------
#Adds ID features to x
add_x_ID = function(y,x,id)
{
  #Set up fID with ID features for first y
  fID = get_some_data(id, y$subid[1],y$utc[1])
  fID$utc[1] = y$utc[1]  #stamp the ID features with the y utc for data checking
  
  #loop through the remaining Ys
  for (i in 2:nrow(y)){
    fID = bind_rows(fID,get_some_data(id, y$subid[i],y$utc[i]))
    fID$utc[i] = y$utc[i]  #stamp the ID features with the y utc for data checking
  }
  
  
  #handle categorical x
  Vars = c('DEM_3','DEM_4','DEM_6','DEM_8')
  for(AVar in Vars){
    fID[,AVar] = factor(fID[,AVar])
    dmy = dummyVars(formula(str_c("~", AVar)), data = fID, fullRank=TRUE)
    p = predict(dmy,fID)
    fID[,AVar] = NULL
    fID = bind_cols(fID,p)
  }
  
  #sort by subid and utc
  fID = fID[order(fID$subid,fID$utc),]
  
  #Add ID_ tag to all variable names
  names(fID) = str_c('ID_', names(fID))
  
  #merge fID into x
  if(is.null(x))
  {
    x = fID  #x was null so set to fID
    x = varRename(x,c('ID_SubID', 'ID_UTC'),c('subid', 'utc'))  #rename but retain subid and utc
  }
  else
  {
    #check that fID and x match
    if(nrow(x)!=nrow(fID)) stop('x and fID do not have same number of rows')
    if(any(fID$ID_SubID !=x$subid) | any(fID$ID_UTC !=x$utc)) stop('x and fID have mismatched subid and/or utc')
    
    #Remove subid and utc from fID b/c already in x
    fID$ID_SubID = NULL
    fID$ID_UTC = NULL
    x = data.frame(x,fID, stringsAsFactors=FALSE)
  }
}

#add_x_SR-----------------
add_x_SR = function(y,x,sr)
{
  fSR = NULL #initial SR feature dataframe to Null to start
  
  #loop through the Ys
  for (i in 1:nrow(y)){
    dSR1 = get_some_data(sr, y$subid[i],y$utc[i])  #SR data frame for 1 y
    dSR1$utc=NULL
    dSR1$subid=NULL
    
    #Feature: most recent for all signals in sr
    t=dSR1[nrow(dSR1),]
    names(t) = str_c(names(t),'_MR')
    fSR1 = t  #fSR1 for 1 y is set to t b/c these are the first features added 
    
    # #Feature:  Most recent - mean of all previous for all signals in sr
    # if(nrow(dSR1)==1){
    #   FeatureNames = str_c(names(dSR1), '_MRvB')
    #   t = data.frame(matrix(data = NA,nrow=1,ncol=ncol(dSR1),dimnames=list(c('1'),FeatureNames)))
    # }else{
    #   t=dSR1[nrow(dSR1),] - sapply(dSR1[1:(nrow(dSR1)-1),],na.rm=TRUE,mean)
    #   names(t) = str_c(names(t), '_MRvB')
    # }
    # fSR1 = data.frame(fSR1,t,stringsAsFactors=FALSE) #merge in these features into fSR1

    
    #Feature extraction done for SR1.  Now clean up and add to fSR
    #Stamp features in fSR1 for this y with subid and utc for this y
    fSR1$subid =  y$subid[i]
    fSR1$utc =  y$utc[i]

    #Merge all SR features for 1 y into full feature dataframe fSR
    if(is.null(fSR)){ #if merging features for first y
      fSR = fSR1
    }else{
      fSR = dfMerge(fSR,fSR1,AddVars = FALSE)
    }
  }
  
  #fSR created for all y.  Now clean up and add to x
  #sort by subid and utc
  fSR = fSR[order(fSR$subid,fSR$utc),]
  
  #Add SR_ tag to all feature names
  names(fSR) = str_c('SR_', names(fSR))
  
  #merge fSR into x
  if(is.null(x))
  {
    x = fSR  #x was null so set to fSR
    x = varRename(x,c('SR_SubID', 'SR_UTC'),c('subid', 'utc'))  #rename but retain subid and utc
  }
  else
  {
    #check that fSR and x match
    if(nrow(x)!=nrow(fSR)) stop('x and fSR do not have same number of rows')
    if(any(fSR$SR_SubID !=x$subid) | any(fSR$SR_UTC !=x$utc)) stop('x and fSR have mismatched subid and/or utc')
    
    #Remove subid and utc from fSR b/c already in x
    fSR$SR_SubID = NULL
    fSR$SR_UTC = NULL
    x = data.frame(x,fSR, stringsAsFactors=FALSE)
  } 
  
  return(x)
  
}

#add_x_EMA-----------------
add_x_EMA = function(y,x,dE)
{
  fE = NULL #initial EMA feature dataframe to Null to start
  
  #Remove unnecssary EMA signals
  dE$EndDate=NULL
  dE$EMA_1=NULL
  dE$EMA_1.1=NULL
  dE$EMA_1.3=NULL
  dE$EMA_1.5=NULL
  dE$type = NULL
  
  #loop through the Ys
  for (i in 1:nrow(y))
  {
    #EMA data frame for 1 y
    dE1 = get_some_data(dE, y$subid[i],y$utc[i], period='All')  
    
    #Feature: most recent non-missing EMA for all signals in dE
    t=data.frame(t(sapply(dE1[,3:ncol(dE1)],'safe_last',na.rm=TRUE)))
    names(t) = str_c(names(t),'_MR')
    fE1 = t  #fE for 1 y is set to t b/c these are the first features added 

    #Feature: Mean/Max/SD for past Day/3Day/Week for all signals in dE
    periods = c('Day', '3Day', 'Week')
    Methods = c('safe_mean', 'safe_max', 'safe_sd')
    for(period in periods){
      for(Method in Methods){
      dEP = get_some_data(dE, y$subid[i],y$utc[i], period=period)  #past period of EMA relative to utc
      t=data.frame(t(sapply(dEP[,3:ncol(dEP)],Method,na.rm=TRUE)))
      
      if(Method=='safe_max') Label = str_c('_Max', period)
      if(Method=='safe_mean') Label = str_c('_Mean', period)
      if(Method=='safe_sd') Label = str_c('_SD', period)

      names(t) = str_c(names(t),Label)
      fE1 = data.frame(fE1,t,stringsAsFactors=FALSE) #merge in these features into fE1
      }
    }
    
    #Feature: most recent non-missing EMA - mean of all EMA for all signals in dE
    t=data.frame(t(sapply(dE1[,3:ncol(dE1)],'safe_last',na.rm=TRUE) - sapply(dE1[,3:ncol(dE1)],'safe_mean',na.rm=TRUE)))
    names(t) = str_c(names(t),'_MRvB')
    fE1 = data.frame(fE1,t,stringsAsFactors=FALSE) #merge in these features into fE1
    
    #Feature: Mean/Max for past Day/3Day/Week - mean of all EMA for all signals in dE
    periods = c('Day', '3Day', 'Week')
    Methods = c('safe_mean', 'safe_max')
    for(period in periods){
      for(Method in Methods){
        dEP = get_some_data(dE, y$subid[i],y$utc[i], period=period)  #past period of EMA relative to utc
        t=data.frame(t(sapply(dEP[,3:ncol(dEP)],Method,na.rm=TRUE) - sapply(dE1[,3:ncol(dE1)],'safe_mean',na.rm=TRUE)))
        
        if(Method=='safe_max') Label = str_c('_Max', period, 'vB')
        if(Method=='safe_mean') Label = str_c('_Mean', period, 'vB')
        
        names(t) = str_c(names(t),Label)
        fE1 = data.frame(fE1,t,stringsAsFactors=FALSE) #merge in these features into fE1
      }
    }    
    
    #Feature: SD for past Day/3Day/Week - SD of all EMA for all signals in dE
    periods = c('Day', '3Day', 'Week')
    for(period in periods){
      dEP = get_some_data(dE, y$subid[i],y$utc[i], period=period)  #past period of EMA relative to utc
      t=data.frame(t(sapply(dEP[,3:ncol(dEP)],'safe_sd',na.rm=TRUE) - sapply(dE1[,3:ncol(dE1)],'safe_sd',na.rm=TRUE)))
     
      Label = str_c('_SD', period, 'vB')
      names(t) = str_c(names(t),Label)
      fE1 = data.frame(fE1,t,stringsAsFactors=FALSE) #merge in these features into fE1
    }   
    
    #Feature extraction done for E1  Now clean up and add to fE
    #Stamp features in fE1 for this y with subid and utc for this y
    fE1$subid =  y$subid[i]
    fE1$utc =  y$utc[i]
    
    #Merge all EMA features for 1 y into full feature dataframe fE
    if(is.null(fE)){ #if merging features for first y
      fE = fE1
    }else{
      fE = dfMerge(fE,fE1,AddVars = FALSE)
    }
  }
  
  #fE created for all y.  Now clean up and add to x
  
  #Delete useless features (e.g., expected to be all NA)
  fE$EMA_8_SDDay = NULL  #only 1 per day so should be NA for SD
  fE$EMA_9_SDDay = NULL
  fE$EMA_10_SDDay = NULL
  fE$EMA_8_SDDayvB = NULL  
  fE$EMA_9_SDDayvB = NULL
  fE$EMA_10_SDDayvB = NULL
  
  #sort by subid and utc
  fE = fE[order(fE$subid,fE$utc),]
  
  #Add EMA_ tag to all variable names
  #names(fE) = str_c('EMA_', names(fE))
  
  #merge fSR into x
  if(is.null(x))
  {
    x = fE  #x was null so set to fSR
    #x = varRename(x,c('EMA_SubID', 'EMA_UTC'),c('subid', 'utc'))  #rename but retain subid and utc
  }
  else
  {
    #check that fE and x match
    if(nrow(x)!=nrow(fE)) stop('x and fE do not have same number of rows')
    if(any(fE$EMA_SubID !=x$subid) | any(fE$EMA_UTC !=x$utc)) stop('x and fE have mismatched subid and/or utc')
    
    #Remove subid and utc from fE b/c already in x
    # fE$EMA_SubID = NULL
    # fE$EMA_UTC = NULL
    fE$subid = NULL
    fE$utc = NULL
    x = data.frame(x,fE, stringsAsFactors=FALSE)
  } 
  
  return(x)
  
}

#add_x_GEN-----------------
add_x_GEN = function(period, y,x, dE, dVi,dD)
{
  #Holidays- used later in loop
  HolidayYears = c(2017,2018,2019,2020)
  HolidayList = as_date(c(USNewYearsDay(HolidayYears),USMLKingsBirthday(HolidayYears),USWashingtonsBirthday(HolidayYears),
                  USMemorialDay(HolidayYears), USIndependenceDay(HolidayYears), USLaborDay(HolidayYears), 
                  USColumbusDay(HolidayYears),USVeteransDay(HolidayYears),USThanksgivingDay(HolidayYears),
                  USChristmasDay(HolidayYears)))
  HolidayList = c(HolidayList, HolidayList-1)
  
  f_g= NULL #initial GEN feature dataframe to Null to start
  
  #loop through the Ys
  for (i in 1:nrow(y)){
    #Features: total days on study and total days^2
    g1 = dVi[dVi$subid==y$subid[i],]
    YDate = as_datetime(y$utc[i], tz='America/Chicago')
    Days = as.numeric(floor(difftime(YDate, g1$StartStudy, units='day')))
    t = data.frame(GEN_DaysEnrolled= Days, GEN_DaysEnrolled2= Days^2)
    f_g1 = t #f_g1 for 1 y is set to t b/c this is the first feature added 
    
    #Features:  Num of lapses, lapse rate (mean)
    dE1 = filter(dE,subid==y$subid[i] & EMA_1==2)  #get ema from only this subject and lapse records
    dE1$utc = as.numeric(dE1$EMA_1.1)  #set utc to the actual time lapse occurred
    g1 = get_some_data(dE1, y$subid[i],y$utc[i], period='All')  
    
    GEN_NumLapsesTot = nrow(g1)
    GEN_MeanLapsesTot = nrow(g1)/Days  #(days on study from above)
    t = data.frame(GEN_NumLapsesTot,GEN_MeanLapsesTot)
    f_g1 = data.frame(f_g1,t,stringsAsFactors=FALSE) #merge in these features into f_g1

    #Features:  Lapse in past day
    g1 = get_some_data(dE1, y$subid[i],y$utc[i], period='Day')  
    GEN_NumLapseDay = nrow(g1)
    GEN_AnyLapseDay = ifelse(GEN_NumLapseDay==0,0,1)
    t = data.frame(GEN_NumLapseDay, GEN_AnyLapseDay)
    f_g1 = data.frame(f_g1,t,stringsAsFactors=FALSE) #merge in these features into f_g1
    
    #Features:  Lapses in past 3 days
    g1 = get_some_data(dE1, y$subid[i],y$utc[i], period='3Day')  
    GEN_NumLapse3Day = nrow(g1)
    GEN_AnyLapse3Day = ifelse(GEN_NumLapse3Day==0,0,1)
    t = data.frame(GEN_NumLapse3Day, GEN_AnyLapse3Day)
    f_g1 = data.frame(f_g1,t,stringsAsFactors=FALSE) #merge in these features into f_g1
    
    #Features:  Lapses in past week
    g1 = get_some_data(dE1, y$subid[i],y$utc[i], period='Week')  
    GEN_NumLapseWeek = nrow(g1)
    GEN_AnyLapseWeek = ifelse(GEN_NumLapseWeek==0,0,1)
    t = data.frame(GEN_NumLapseWeek, GEN_AnyLapseWeek)
    f_g1 = data.frame(f_g1,t,stringsAsFactors=FALSE) #merge in these features into f_g1
    
    #Features:  Day of week, day of month, month for prediction period start
    Date = as_datetime(y$utc[i], tz="America/Chicago")
    t = data.frame(weekdays(Date), day(Date), month(Date))
    names(t) = c('GEN_Weekday','GEN_Day', 'GEN_Month') 
    f_g1 = data.frame(f_g1,t,stringsAsFactors=FALSE) #merge in these features into f_g1

    #Features:  Day before or day of Holiday falls into lapse period
    StartDate = as_date(as_datetime(y$utc[i], tz="America/Chicago"))
    if(period=="Day"){
      EndDate = StartDate
    }else{
      EndDate = StartDate + 6
    }
    GEN_Holiday = as.numeric(any(HolidayList >=StartDate & HolidayList <=EndDate))
    t = data.frame(GEN_Holiday)
    f_g1 = data.frame(f_g1,t,stringsAsFactors=FALSE) #merge in these features into f_g1

    #Features:  Subject Specific risk Date falls into lapse period
    dD1 = as_date(dD$Date[dD$subid ==y$subid[i]])
    if(period=="Day"){
      EndDate = StartDate
    }else{
      EndDate = StartDate + 6
    }
    GEN_RiskDate = as.numeric(any(dD1 >=StartDate & dD1 <=EndDate))
    t = data.frame(GEN_RiskDate)
    f_g1 = data.frame(f_g1,t,stringsAsFactors=FALSE) #merge in these features into f_g1
    
    #Feature extraction done for G1  Now clean up and add to f_g
    
    #Stamp features in f_g1 for this y with subid and utc for this y
    f_g1$subid =  y$subid[i]
    f_g1$utc =  y$utc[i]
    
    #Merge all GEN features for 1 y into full feature dataframe f_g
    if(is.null(f_g)){ #if merging features for first y
      f_g = f_g1
    }else{
      f_g = dfMerge(f_g,f_g1,AddVars = FALSE)
    }
  }
  
  #f_g created for all y.  Now clean up and add to x
  
  #Add reduced options of GEN_Day down to start, middle, and end of month
  f_g$GEN_DayPeriods = f_g$GEN_Day
  f_g$GEN_DayPeriods[f_g$GEN_Day<6] = 1
  f_g$GEN_DayPeriods[f_g$GEN_Day>5 & f_g$GEN_Day<25] = 2
  f_g$GEN_DayPeriods[f_g$GEN_Day>24] = 3
  
  #Handle categorical variables
  Vars = c('GEN_Weekday','GEN_DayPeriods','GEN_Month')
  for(AVar in Vars){
    f_g[,AVar] = factor(f_g[,AVar])
    dmy = dummyVars(formula(str_c("~", AVar)), data = f_g, fullRank=TRUE)
    p = predict(dmy,f_g)
    f_g[,AVar] = NULL
    f_g = bind_cols(f_g,p)
  }
  
  
  #sort by subid and utc
  f_g = f_g[order(f_g$subid,f_g$utc),]
  
  #merge f_g into x
  if(is.null(x)){
    x = f_g  #x was null so set to f_g
  }else{
    #check that f_g and x match
    if(nrow(x)!=nrow(f_g)) stop('x and f_g do not have same number of rows')
    if(any(f_g$subid !=x$subid) | any(f_g$utc !=x$utc)) stop('x and f_g have mismatched subid and/or utc')
    
    #Remove subid and utc from fE b/c already in x
    f_g$subid = NULL
    f_g$utc = NULL
    x = data.frame(x,f_g, stringsAsFactors=FALSE)
  } 
  
  return(x)
}

#add_x_gps-----------------
add_x_gps = function(y, x, gps)
{

  gps_p = subset(gps, type=='PLACE')
  f_g= NULL #initial GPS feature dataframe to Null to start
  
  #loop through the Ys
  for (i in 1:nrow(y)){
    if (i %% 1000 == 0) message("Processing label index: ", i)
    
    #Setup feature vector/row for 1 lapse label with subid and timestamp
    f_g1 = data.frame(subid = y$subid[i], utc =  y$utc[i])
    
    #Features: Tot number of drank/alcohol/avoid places, 
    #proportion1 (vs. no) of drank/alcohol/avoid places, 
    #proportion2 (vs. all) of drank/alcohol/avoid places, 
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty temporary dataframe with 1 row
    
    periods = c('All', 'Week', '3Day')
    for(a_period in periods){
      
      g1 = get_some_data(gps_p, y$subid[i], y$utc[i], period = a_period)
      
      gps_vars = c('drank', 'alcohol', 'avoid')
      for(a_gps_var in gps_vars){

        if(a_gps_var == 'drank') d_yes = filter(g1, drank == 'YES')
        if(a_gps_var == 'alcohol') d_yes = filter(g1, alcohol == 'YES')
        if(a_gps_var == 'avoid') d_yes = filter(g1, avoid == 'YES')
        
        #counts
        count_yes = nrow(d_yes)
        count_defined = nrow(filter(g1, !is.na(place_type)))  #if defined for placetype, defined for all records
        count_all = nrow(g1)
        
        t = tibble()
        t[1,str_c('gps_num_', a_gps_var, '_',a_period)] = count_yes
        t[1,str_c('gps_np1_', a_gps_var, '_',a_period)] = ifelse(count_defined, count_yes/count_defined, NA)
        t[1,str_c('gps_np2_', a_gps_var, '_',a_period)]  = count_yes/count_all
        
        #time at places including home
        time_yes =           summarize(d_yes, sum(next_time, na.rm=TRUE))
        time_yes = as.numeric(time_yes)
        
        time_defined_places = filter(g1, !is.na(place_type)) %>%  #gets all defined rows
                            summarize(sum(next_time, na.rm=TRUE))
        time_defined_places = as.numeric(time_defined_places)
        
        time_all_places =     summarize(g1, sum(next_time, na.rm=TRUE))  #all places
        time_all_places = as.numeric(time_all_places)
        
        t[1,str_c('gps_time1_', a_gps_var, '_',a_period)] = time_yes
        t[1,str_c('gps_t1p1_', a_gps_var, '_',a_period)] = ifelse(time_defined_places, time_yes/time_defined_places, NA)
        t[1,str_c('gps_t1p2_', a_gps_var, '_',a_period)] = ifelse(time_all_places, time_yes/time_all_places, NA)
        
        #time at places excluding home (NOTE:  NEED COUNTS AT PLACES NOT INCLUDING HOME???)
        time_yes =           filter(d_yes, place_type!='HOME') %>%  #gets all YES for not HOME rows (and not NA rows but dont want NA rows)
                            summarize(sum(next_time, na.rm=TRUE))
        time_yes = as.numeric(time_yes)
        
        time_defined_places = filter(g1, place_type!='HOME') %>%  #gets all not home rows (and not NA rows but dont want NA)
                            summarize(sum(next_time, na.rm=TRUE))
        time_defined_places = as.numeric(time_defined_places)
        
        time_all_places =     filter(g1, place_type!='HOME' |  is.na(!!sym('place_type'))) %>%  #gets all not home OR NA rows (excludes just home rows)
                            summarize(sum(next_time, na.rm=TRUE))
        time_all_places = as.numeric(time_all_places)
        
        t[1,str_c('gps_time2_', a_gps_var, '_',a_period)] = time_yes
        t[1,str_c('gps_t2p1_', a_gps_var, '_',a_period)] = ifelse(time_defined_places, time_yes / time_defined_places, NA)
        t[1,str_c('gps_t2p2_', a_gps_var, '_',a_period)] = ifelse(time_all_places, time_yes / time_all_places, NA)
      }
    }
    f_g1 = bind_cols(f_g1,t) #merge in these features into f_g1
    

    #Features: Tot number of High, High/Medium, and Any risk places, 
    #proportion1 (vs. no) of High, High/Medium, and Any risk places, 
    #proportion2 (vs. all) of High, High/Medium, and Any risk places, 
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    
    periods = c('All', 'Week', '3Day')
    for(a_period in periods){
      g1 = get_some_data(gps_p, y$subid[i],y$utc[i], period=a_period)
      
      risk_levels = c('HighRisk', 'HighMedRisk', 'AnyRisk')
      for(a_risk_level in risk_levels){
          
        #counts
        if(a_risk_level == 'HighRisk') d_yes = filter(g1, risk=='HIGH')
        if(a_risk_level == 'HighMedRisk') d_yes = filter(g1, risk=='HIGH' | risk=='MEDIUM')
        if(a_risk_level == 'AnyRisk') d_yes = filter(g1, risk=='HIGH' |  risk=='MEDIUM' |  risk=='LOW')
        count_yes = nrow(d_yes)
        count_defined = nrow(filter(g1, !is.na(risk)))
        count_all = nrow(g1)
        
        t[1,str_c('gps_num_', a_risk_level, '_',a_period)] = count_yes
        t[1,str_c('gps_np1_', a_risk_level, '_',a_period)] = ifelse(count_defined, count_yes/count_defined, NA)
        t[1,str_c('gps_np2_', a_risk_level, '_',a_period)]  = count_yes/count_all
        
        #time at places including home
        time_yes =           summarize(d_yes, sum(next_time, na.rm=TRUE))
        time_yes = as.numeric(time_yes)
        
        time_defined_places = filter(g1, !is.na(risk)) %>%  #gets all defined rows
                            summarize(sum(next_time, na.rm=TRUE))
        time_defined_places = as.numeric(time_defined_places)
        
        time_all_places =     summarize(g1, sum(next_time, na.rm=TRUE))  #all rows
        time_all_places = as.numeric(time_all_places)
        
        t[1,str_c('gps_time1_', a_risk_level, '_',a_period)] = time_yes
        t[1,str_c('gps_t1p1_', a_risk_level, '_',a_period)] = ifelse(time_defined_places, time_yes/time_defined_places, NA)
        t[1,str_c('gps_t1p2_', a_risk_level, '_',a_period)] = ifelse(time_all_places, time_yes/time_all_places, NA)
      
        
        #time at places excluding home (NOTE:  NEED COUNTS AT PLACES NOT INCLUDING HOME???)
        time_yes =           filter(d_yes, place_type!='HOME') %>%  #gets all YES for not HOME rows (and not NA rows but dont want NA rows)
                            summarize(sum(next_time, na.rm=TRUE))
        time_yes =           as.numeric(time_yes)
        
        time_defined_places = filter(g1, place_type!='HOME') %>%  #gets all not home rows (and not NA rows but dont want NA)
                            summarize(sum(next_time, na.rm=TRUE))
        time_defined_places = as.numeric(time_defined_places)
        
        time_all_places =     filter(g1, place_type!='HOME' |  is.na(!!sym('place_type'))) %>%  #gets all not home OR NA rows (excludes just home rows)
                            summarize(sum(next_time, na.rm=TRUE))
        time_all_places =     as.numeric(time_all_places)
        
        t[1,str_c('gps_time2_', a_risk_level, '_',a_period)] = time_yes
        t[1,str_c('gps_t2p1_', a_risk_level, '_',a_period)] = ifelse(time_defined_places, time_yes / time_defined_places, NA)
        t[1,str_c('gps_t2p2_', a_risk_level, '_',a_period)] = ifelse(time_all_places, time_yes / time_all_places, NA)
      }
    }
    f_g1 = bind_cols(f_g1,t) #merge in these features into f_g1
    
    #Features: Tot number of Unpleasant, Mixed, Pleasant and Any affect places, 
    #proportion1 (vs. no) of Unpleasant, Mixed, Pleasant and Any affect  places, 
    #proportion2 (vs. all) of Unpleasant, Mixed, Pleasant and Any affect  places, 
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    
    periods = c('All', 'Week', '3Day')
    for(a_period in periods){
      g1 = get_some_data(gps_p, y$subid[i],y$utc[i], period=a_period)
      
      EmotionLevels = c('UNPLEASANT', 'MIXED', 'PLEASANT', 'ANYNEG', 'ANYPOS', 'ANYAFFECT')
      for(AEmoLev in EmotionLevels){
          
        #counts
        if(AEmoLev == 'UNPLEASANT') d_yes = filter(g1, emotion=='UNPLEASANT')
        if(AEmoLev == 'MIXED') d_yes = filter(g1, emotion=='MIXED')
        if(AEmoLev == 'PLEASANT') d_yes = filter(g1, emotion=='PLEASANT')
        if(AEmoLev == 'ANYNEG') d_yes = filter(g1, emotion=='UNPLEASANT' |  emotion=='MIXED')
        if(AEmoLev == 'ANYPOS') d_yes = filter(g1, emotion=='PLEASANT' |  emotion=='MIXED' )
        if(AEmoLev == 'ANYAFFECT') d_yes = filter(g1, emotion=='UNPLEASANT' |  emotion=='MIXED' |  emotion=='PLEASANT')
        count_yes = nrow(d_yes)
        count_defined = nrow(filter(g1, !is.na(emotion)))
        count_all = nrow(g1)
        
        t[1,str_c('gps_num_', AEmoLev, '_',a_period)] = count_yes
        t[1,str_c('gps_np1_', AEmoLev, '_',a_period)] = ifelse(count_defined, count_yes/count_defined, NA)
        t[1,str_c('gps_np2_', AEmoLev, '_',a_period)]  = count_yes/count_all

        #time at places including home
        time_yes =           summarize(d_yes, sum(next_time, na.rm=TRUE))
        time_yes = as.numeric(time_yes)
        
        time_defined_places = filter(g1, !is.na(emotion)) %>%  #gets all defined rows
                            summarize(sum(next_time, na.rm=TRUE))
        time_defined_places = as.numeric(time_defined_places)
        
        time_all_places =     summarize(g1, sum(next_time, na.rm=TRUE))  #all rows
        time_all_places =     as.numeric(time_all_places)
        
        t[1,str_c('gps_time1_', AEmoLev, '_',a_period)] = time_yes
        t[1,str_c('gps_t1p1_', AEmoLev, '_',a_period)] = ifelse(time_defined_places, time_yes/time_defined_places, NA)
        t[1,str_c('gps_t1p2_', AEmoLev, '_',a_period)] = ifelse(time_all_places, time_yes/time_all_places, NA)
        
        #time at places excluding home (NOTE:  NEED COUNTS AT PLACES NOT INCLUDING HOME???)
        time_yes =           filter(d_yes, place_type!='HOME') %>%  #gets all YES for not HOME rows (and not NA rows but dont want NA rows)
                            summarize(sum(next_time, na.rm=TRUE))
        time_yes =           as.numeric(time_yes)
        
        time_defined_places = filter(g1, place_type!='HOME') %>%  #gets all not home rows (and not NA rows but dont want NA)
                            summarize(sum(next_time, na.rm=TRUE))
        time_defined_places = as.numeric(time_defined_places)
        
        time_all_places =     filter(g1, place_type!='HOME' |  is.na(!!sym('place_type'))) %>%  #gets all not home OR NA rows (excludes just home rows)
                            summarize(sum(next_time, na.rm=TRUE))
        time_all_places =     as.numeric(time_all_places)
        
        t[1,str_c('gps_time2_', AEmoLev, '_',a_period)] = time_yes
        t[1,str_c('gps_t2p1_', AEmoLev, '_',a_period)] = ifelse(time_defined_places, time_yes / time_defined_places, NA)
        t[1,str_c('gps_t2p2_', AEmoLev, '_',a_period)] = ifelse(time_all_places, time_yes / time_all_places, NA)
      }
    }
    f_g1 = bind_cols(f_g1,t) #merge in these features into f_g1
    
    
    #Features: Time at home, work, volunteer, bar, etc 
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    
    periods = c('All', 'Week', '3Day')
    for(a_period in periods){
      g1 = get_some_data(gps_p, y$subid[i],y$utc[i], period=a_period)
      
      locations = c('Home', 'Work', 'Volunteer', 'School', 'AA', 'Bar', 'Gym', 'HealthCare', 'LiquorStore','Restaurant', 'Cafe', 'Friend', 'Family')
      for(a_location in locations){
          
        #counts
        if(a_location == 'Home') d_yes = filter(g1, place_type=='HOME')
        if(a_location == 'Work') d_yes = filter(g1, place_type=='WORK')
        if(a_location == 'Volunteer') d_yes = filter(g1, place_type=='VOLUNTEER')
        if(a_location == 'School') d_yes = filter(g1, place_type=='SCHOOL')
        if(a_location == 'AA') d_yes = filter(g1, place_type=='AA/RECOVERY MEETING')
        if(a_location == 'Bar') d_yes = filter(g1, place_type=='BAR')
        if(a_location == 'Gym') d_yes = filter(g1, place_type=='GYM/FITNESS CENTER')
        if(a_location == 'HealthCare') d_yes = filter(g1, place_type=='HEALTH CARE')
        if(a_location == 'LiquorStore') d_yes = filter(g1, place_type=='LIQUOR STORE')
        if(a_location == 'Restaurant') d_yes = filter(g1, place_type=='RESTAURANT')
        if(a_location == 'Cafe') d_yes = filter(g1, place_type=='COFFEE SHOP/CAFE')
        if(a_location == 'Friend') d_yes = filter(g1, place_type=='HOME OF FRIEND')
        if(a_location == 'Family') d_yes = filter(g1, place_type=='HOME OF FAMILY MEMBER')
        count_yes = nrow(d_yes)
        count_defined = nrow(filter(g1, !is.na(emotion)))
        count_all = nrow(g1)
        
        t[1,str_c('gps_num_', a_location, '_',a_period)] = count_yes
        t[1,str_c('gps_np1_', a_location, '_',a_period)] = ifelse(count_defined, count_yes/count_defined, NA)
        t[1,str_c('gps_np2_', a_location, '_',a_period)]  = count_yes/count_all
        
        
        #time at places
        time_yes =           summarize(d_yes, sum(next_time, na.rm=TRUE))
        time_yes =           as.numeric(time_yes)
        
        time_defined_places = filter(g1, !is.na(place_type)) %>%  #gets all defined rows
                            summarize(sum(next_time, na.rm=TRUE))
        time_defined_places = as.numeric(time_defined_places)
        
        time_all_places =     summarize(g1, sum(next_time, na.rm=TRUE))  #all rows
        time_all_places =     as.numeric(time_all_places)
        
        t[1,str_c('gps_time1_', a_location, '_',a_period)] = time_yes
        t[1,str_c('gps_t1p1_', a_location, '_',a_period)] = ifelse(time_defined_places, time_yes/time_defined_places, NA)
        t[1,str_c('gps_t1p2_', a_location, '_',a_period)] = ifelse(time_all_places, time_yes/time_all_places, NA)
      }
    }
    f_g1 = bind_cols(f_g1,t) #add these features to f_g1 
    
    
    #Feature extraction done for f_g1  Now add to f_g
    
    #Merge all GPS features for 1 y into full feature dataframe f_g
    if(is.null(f_g)){ #if merging features for first y
      f_g = f_g1
    }else{
      f_g = bind_rows(f_g,f_g1)
    }
  }
  
  #f_g created for all y.  Now clean up and add to x
  
  #sort by subid and utc
  f_g = f_g[order(f_g$subid,f_g$utc),]
  
  #merge f_g into x
  if(is.null(x)){
    x = f_g  #x was null so set to f_g
  }else{
    #check that f_g and x match
    if(nrow(x)!=nrow(f_g)) stop('x and f_g do not have same number of rows')
    if(any(f_g$subid !=x$subid) | any(f_g$utc !=x$utc)) stop('x and f_g have mismatched subid and/or utc')
    
    #Remove subid and utc from f_g b/c already in x
    f_g$subid = NULL
    f_g$utc = NULL
    x = data.frame(x,f_g, stringsAsFactors=FALSE)
  } 
  
  return(x)
}


#add_x_Voice-----------------
add_x_Voice = function(y, x, dV){
  # #setup parallel backend if available
  # nCore =detectCores()
  # if(nCore>2) {
  #   cl = makeCluster(nCore-1)
  #   registerDoParallel(cl)
  #   getDoParWorkers()
  # }
  
  #2 level voice type collapsing missed and rejected into incoming
  dV$Type2 = dV$type
  dV$Type2[dV$Type2>2 &  dV$Type2<7] = 1 
  dV$Type2[dV$Type2>2] = NA
  
  dV$Duration[dV$Duration<0] = 0  #set negative values to 0
  
  periods = c('All', 'Week', '3Day')
  
  #, .export = c('get_some_data', 'periods', 'Inf2NA', 'dV', 'y')
  #loop through the Ys
  fV =  foreach (i = 1:nrow(y), .combine='rbind', .packages = c('stringr'), .export = c('get_some_data', 'Inf2NA')) %do%{     
    #message(str_c('ROW: ', i)) #for serial testing
    fV1 = data.frame(subid = y$subid[i], utc = y$utc[i])  #holds all voice features for 1 lapse label
    
    #Features: Tot number(NUM) of incoming, outgoing, all 
    #Features: Tot number(NUM2) of incoming, outgoing, all not missed (duration > 0) 
    #Features: Tot time of incoming, outgoing, and all calls 
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row

    
    for(a_period in periods){
      dV1 = get_some_data(dV, y$subid[i],y$utc[i], period=a_period)
      
      if(nrow(dV1)>0){
        
        #NUM and DUR incoming/outgoing/all
        indices = dV1[,'Type2']==1
        t[1,str_c('VOICE_Num_In_',a_period)] = sum(indices, na.rm=TRUE)
        t[1,str_c('VOICE_Dur_In_',a_period)] = round(sum(dV1$Duration[indices], na.rm=TRUE))
        
        indices = dV1[,'Type2']==2
        t[1,str_c('VOICE_Num_Out_',a_period)] = sum(indices, na.rm=TRUE)
        t[1,str_c('VOICE_Dur_Out_',a_period)] = round(sum(dV1$Duration[indices], na.rm=TRUE))
        
        t[1,str_c('VOICE_Num_All_',a_period)] = nrow(dV1)
        t[1,str_c('VOICE_Dur_All_',a_period)] = round(sum(dV1$Duration, na.rm=TRUE))
        
        #NUM2 incoming/outgoing/all > 0 duration
        indices = dV1[,'Type2']==1 & dV1[,'Duration']>0
        t[1,str_c('VOICE_Num2_In_',a_period)] = sum(indices, na.rm=TRUE)
        
        indices = dV1[,'Type2']==2 & dV1[,'Duration']>0
        t[1,str_c('VOICE_Num2_Out_',a_period)] = sum(indices, na.rm=TRUE)
        
        indices = dV1[,'Duration']>0
        t[1,str_c('VOICE_Num2_All_',a_period)] = sum(indices, na.rm=TRUE)        
        
      }else{
        t[1,str_c('VOICE_Num_In_',a_period)] = 0
        t[1,str_c('VOICE_Dur_In_',a_period)] = 0
        t[1,str_c('VOICE_Num2_In_',a_period)] = 0
        
        t[1,str_c('VOICE_Num_Out_',a_period)] = 0
        t[1,str_c('VOICE_Dur_Out_',a_period)] = 0
        t[1,str_c('VOICE_Num2_Out_',a_period)] = 0
        
        t[1,str_c('VOICE_Num_All_',a_period)] = 0
        t[1,str_c('VOICE_Dur_All_',a_period)] = 0
        t[1,str_c('VOICE_Num2_All_',a_period)] = 0
      }
      
    }
    fV1 = data.frame(fV1,t,stringsAsFactors=FALSE) #merge in these features into fV1 
    
    #DrankPast levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    AVoiceVar = 'DrankPast'
    for(a_period in periods){
      dV1 = get_some_data(dV, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('Always', 'AlwaysOccas', 'Never')
      for(ALevel in Levels){
        if(ALevel == 'Always') indices = dV1[,AVoiceVar]=='ALMOST ALWAYS/ALWAYS'
        if(ALevel == 'AlwaysOccas') indices = dV1[,AVoiceVar]=='ALMOST ALWAYS/ALWAYS' |  dV1[,AVoiceVar]=='OCCASIONALLY'
        if(ALevel == 'Never') indices = dV1[,AVoiceVar]=='NEVER/ALMOST NEVER'

        if(nrow(dV1)>0){
          
          #Counts of all calls by drank past level
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dV1) - sum(is.na(dV1[,AVoiceVar]))>0){
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dV1) - sum(is.na(dV1[, AVoiceVar])))
          }else{
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dV1)
          
          #duration of all calls by drank past level
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(dV1$Duration[indices], na.rm=TRUE) 
          if(sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE)>0){
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)] =  sum(dV1$Duration[indices], na.rm=TRUE)  / sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE) 
          }else{
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(dV1$Duration[indices], na.rm=TRUE) / sum(dV1$Duration, na.rm=TRUE)  
          
        }else{
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fV1 = data.frame(fV1,t,stringsAsFactors=FALSE) #merge in these features into fV1 
    
    #DrinkerStatus levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    AVoiceVar = 'DrinkerStatus'
    for(a_period in periods){
      dV1 = get_some_data(dV, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('Drinker', 'NonDrinker')
      for(ALevel in Levels){
        if(ALevel == 'Drinker') indices = dV1[,AVoiceVar]=='DRINKER'
        if(ALevel == 'NonDrinker') indices = dV1[,AVoiceVar]=='NONDRINKER'
        
        if(nrow(dV1)>0){
          
          #Counts of all calls by level
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dV1) - sum(is.na(dV1[,AVoiceVar]))>0){
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dV1) - sum(is.na(dV1[, AVoiceVar])))
          }else{
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dV1)
          
          #duration of all calls by drank past level
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(dV1$Duration[indices], na.rm=TRUE) 
          if(sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE)>0){
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)] =  sum(dV1$Duration[indices], na.rm=TRUE)  / sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE) 
          }else{
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(dV1$Duration[indices], na.rm=TRUE) / sum(dV1$Duration, na.rm=TRUE)  
          
        }else{
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fV1 = data.frame(fV1,t,stringsAsFactors=FALSE) #merge in these features into fV1 
    
    #DrinkFuture levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    AVoiceVar = 'DrinkFuture'
    for(a_period in periods){
      dV1 = get_some_data(dV, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('No', 'Yes')
      for(ALevel in Levels){
        if(ALevel == 'No') indices = dV1[,AVoiceVar]=='NO'
        if(ALevel == 'Yes') indices = dV1[,AVoiceVar]=='YES'
        
        if(nrow(dV1)>0){
          
          #Counts of all calls by level
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dV1) - sum(is.na(dV1[,AVoiceVar]))>0){
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dV1) - sum(is.na(dV1[, AVoiceVar])))
          }else{
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dV1)
          
          #duration of all calls by level
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(dV1$Duration[indices], na.rm=TRUE) 
          if(sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE)>0){
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)] =  sum(dV1$Duration[indices], na.rm=TRUE)  / sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE) 
          }else{
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(dV1$Duration[indices], na.rm=TRUE) / sum(dV1$Duration, na.rm=TRUE)  
          
        }else{
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fV1 = data.frame(fV1,t,stringsAsFactors=FALSE) #merge in these features into fV1     
    
    #Recovery levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    AVoiceVar = 'Recovery'
    for(a_period in periods){
      dV1 = get_some_data(dV, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('No', 'Yes')
      for(ALevel in Levels){
        if(ALevel == 'No') indices = dV1[,AVoiceVar]=='NO'
        if(ALevel == 'Yes') indices = dV1[,AVoiceVar]=='YES'
        
        if(nrow(dV1)>0){
          
          #Counts of all calls by level
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dV1) - sum(is.na(dV1[,AVoiceVar]))>0){
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dV1) - sum(is.na(dV1[, AVoiceVar])))
          }else{
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dV1)
          
          #duration of all calls by level
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(dV1$Duration[indices], na.rm=TRUE) 
          if(sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE)>0){
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)] =  sum(dV1$Duration[indices], na.rm=TRUE)  / sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE) 
          }else{
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(dV1$Duration[indices], na.rm=TRUE) / sum(dV1$Duration, na.rm=TRUE)  
          
        }else{
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fV1 = data.frame(fV1,t,stringsAsFactors=FALSE) #merge in these features into fV1  
    
    
    #Support levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    AVoiceVar = 'Support'
    for(a_period in periods){
      dV1 = get_some_data(dV, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('Unsupportive', 'UnsupMixed', 'Supportive')
      for(ALevel in Levels){
        if(ALevel == 'Unsupportive') indices = dV1[,AVoiceVar]=='UNSUPPORTIVE'
        if(ALevel == 'UnsupMixed') indices = dV1[,AVoiceVar]=='UNSUPPORTIVE' |  dV1[,AVoiceVar]=='MIXED'
        if(ALevel == 'Supportive') indices = dV1[,AVoiceVar]=='SUPPORTIVE'
        
        if(nrow(dV1)>0){
          
          #Counts of all calls by  level
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dV1) - sum(is.na(dV1[,AVoiceVar]))>0){
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dV1) - sum(is.na(dV1[, AVoiceVar])))
          }else{
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dV1)
          
          #duration of all calls by drank past level
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(dV1$Duration[indices], na.rm=TRUE) 
          if(sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE)>0){
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)] =  sum(dV1$Duration[indices], na.rm=TRUE)  / sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE) 
          }else{
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(dV1$Duration[indices], na.rm=TRUE) / sum(dV1$Duration, na.rm=TRUE)  
          
        }else{
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fV1 = data.frame(fV1,t,stringsAsFactors=FALSE) #merge in these features into fV1 
 
    #emotion levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    AVoiceVar = 'emotion'
    for(a_period in periods){
      dV1 = get_some_data(dV, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('Unpleasant', 'UnpleasMixed', 'Pleasant')
      for(ALevel in Levels){
        if(ALevel == 'Unpleasant') indices = dV1[,AVoiceVar]=='UNPLEASANT'
        if(ALevel == 'UnpleasMixed') indices = dV1[,AVoiceVar]=='UNPLEASANT' |  dV1[,AVoiceVar]=='MIXED'
        if(ALevel == 'Pleasant') indices = dV1[,AVoiceVar]=='PLEASANT'
        
        if(nrow(dV1)>0){
          
          #Counts of all calls by  level
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dV1) - sum(is.na(dV1[,AVoiceVar]))>0){
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dV1) - sum(is.na(dV1[, AVoiceVar])))
          }else{
            t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dV1)
          
          #duration of all calls by drank past level
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_',a_period)] = sum(dV1$Duration[indices], na.rm=TRUE) 
          if(sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE)>0){
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)] =  sum(dV1$Duration[indices], na.rm=TRUE)  / sum(dV1$Duration[!is.na(dV1[,AVoiceVar])], na.rm=TRUE) 
          }else{
            t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_',a_period)]  = sum(dV1$Duration[indices], na.rm=TRUE) / sum(dV1$Duration, na.rm=TRUE)  
          
        }else{
          t[1,str_c('VOICE_Num_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_NP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          
          t[1,str_c('VOICE_Dur_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP1_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('VOICE_DP2_', AVoiceVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fV1 = data.frame(fV1,t,stringsAsFactors=FALSE) #merge in these features into fV1 
    
    
    
    #Feature extraction done for V1  Now clean up and add to fV
    
    #Stamp features in fV1 for this y with subid and utc for this y
    #fV1$subid =  y$subid[i]
    #fV1$utc =  y$utc[i]
    
    fV1  #return fV1 for binding in each iteration
    # #Merge all Voice features for 1 y into full feature dataframe fV
    # if(is.null(fV)){ #if merging features for first y
    #   fV = fV1
    # }else{
    #   fV = dfMerge(fV,fV1,AddVars = FALSE)
    # }
  }
  
  #fV created for all y.  Now clean up and add to x
  
  # #close parallel backend
  # if(nCore>2) {
  #   stopCluster(cl)
  #   remove(cl)
  #   registerDoSEQ()
  # }  
  
  #calcalate ratio score features
  for (a_period in periods){
    fV[,str_c('VOICE_RatioNum_InOut_', a_period)] = fV[,str_c('VOICE_Num_In_', a_period)] / fV[,str_c('VOICE_Num_Out_', a_period)] 
    fV[,str_c('VOICE_RatioNum2_InOut_', a_period)] = fV[,str_c('VOICE_Num2_In_', a_period)] / fV[,str_c('VOICE_Num2_Out_', a_period)] 
    fV[,str_c('VOICE_RatioDur_InOut_', a_period)] = fV[,str_c('VOICE_Dur_In_', a_period)] / fV[,str_c('VOICE_Dur_Out_', a_period)] 
 
    #drankpast
    fV[,str_c('VOICE_RatioNum_DrankPast_', a_period)] = fV[,str_c('VOICE_Num_DrankPast_Always_', a_period)] / fV[,str_c('VOICE_Num_DrankPast_Never_', a_period)] 
    fV[,str_c('VOICE_RatioNP1_DrankPast_', a_period)] = fV[,str_c('VOICE_NP1_DrankPast_Always_', a_period)] / fV[,str_c('VOICE_NP1_DrankPast_Never_', a_period)] 
    fV[,str_c('VOICE_RatioNP2_DrankPast_', a_period)] = fV[,str_c('VOICE_NP2_DrankPast_Always_', a_period)] / fV[,str_c('VOICE_NP2_DrankPast_Never_', a_period)] 
    fV[,str_c('VOICE_RatioDur_DrankPast_', a_period)] = fV[,str_c('VOICE_Dur_DrankPast_Always_', a_period)] / fV[,str_c('VOICE_Dur_DrankPast_Never_', a_period)] 
    fV[,str_c('VOICE_RatioDP1_DrankPast_', a_period)] = fV[,str_c('VOICE_DP1_DrankPast_Always_', a_period)] / fV[,str_c('VOICE_DP1_DrankPast_Never_', a_period)] 
    fV[,str_c('VOICE_RatioDP2_DrankPast_', a_period)] = fV[,str_c('VOICE_DP2_DrankPast_Always_', a_period)] / fV[,str_c('VOICE_DP2_DrankPast_Never_', a_period)] 

    #drinker status
    fV[,str_c('VOICE_RatioNum_DrinkerStatus_', a_period)] = fV[,str_c('VOICE_Num_DrinkerStatus_Drinker_', a_period)] / fV[,str_c('VOICE_Num_DrinkerStatus_NonDrinker_', a_period)] 
    fV[,str_c('VOICE_RatioNP1_DrinkerStatus_', a_period)] = fV[,str_c('VOICE_NP1_DrinkerStatus_Drinker_', a_period)] / fV[,str_c('VOICE_NP1_DrinkerStatus_NonDrinker_', a_period)] 
    fV[,str_c('VOICE_RatioNP2_DrinkerStatus_', a_period)] = fV[,str_c('VOICE_NP2_DrinkerStatus_Drinker_', a_period)] / fV[,str_c('VOICE_NP2_DrinkerStatus_NonDrinker_', a_period)] 
    fV[,str_c('VOICE_RatioDur_DrinkerStatus_', a_period)] = fV[,str_c('VOICE_Dur_DrinkerStatus_Drinker_', a_period)] / fV[,str_c('VOICE_Dur_DrinkerStatus_NonDrinker_', a_period)] 
    fV[,str_c('VOICE_RatioDP1_DrinkerStatus_', a_period)] = fV[,str_c('VOICE_DP1_DrinkerStatus_Drinker_', a_period)] / fV[,str_c('VOICE_DP1_DrinkerStatus_NonDrinker_', a_period)] 
    fV[,str_c('VOICE_RatioDP2_DrinkerStatus_', a_period)] = fV[,str_c('VOICE_DP2_DrinkerStatus_Drinker_', a_period)] / fV[,str_c('VOICE_DP2_DrinkerStatus_NonDrinker_', a_period)] 

    #drinker status
    fV[,str_c('VOICE_RatioNum_DrinkFuture_', a_period)] = fV[,str_c('VOICE_Num_DrinkFuture_Yes_', a_period)] / fV[,str_c('VOICE_Num_DrinkFuture_No_', a_period)] 
    fV[,str_c('VOICE_RatioNP1_DrinkFuture_', a_period)] = fV[,str_c('VOICE_NP1_DrinkFuture_Yes_', a_period)] / fV[,str_c('VOICE_NP1_DrinkFuture_No_', a_period)] 
    fV[,str_c('VOICE_RatioNP2_DrinkFuture_', a_period)] = fV[,str_c('VOICE_NP2_DrinkFuture_Yes_', a_period)] / fV[,str_c('VOICE_NP2_DrinkFuture_No_', a_period)] 
    fV[,str_c('VOICE_RatioDur_DrinkFuture_', a_period)] = fV[,str_c('VOICE_Dur_DrinkFuture_Yes_', a_period)] / fV[,str_c('VOICE_Dur_DrinkFuture_No_', a_period)] 
    fV[,str_c('VOICE_RatioDP1_DrinkFuture_', a_period)] = fV[,str_c('VOICE_DP1_DrinkFuture_Yes_', a_period)] / fV[,str_c('VOICE_DP1_DrinkFuture_No_', a_period)] 
    fV[,str_c('VOICE_RatioDP2_DrinkFuture_', a_period)] = fV[,str_c('VOICE_DP2_DrinkFuture_Yes_', a_period)] / fV[,str_c('VOICE_DP2_DrinkFuture_No_', a_period)] 

    #Recovery
    fV[,str_c('VOICE_RatioNum_Recovery_', a_period)] = fV[,str_c('VOICE_Num_Recovery_Yes_', a_period)] / fV[,str_c('VOICE_Num_Recovery_No_', a_period)] 
    fV[,str_c('VOICE_RatioNP1_Recovery_', a_period)] = fV[,str_c('VOICE_NP1_Recovery_Yes_', a_period)] / fV[,str_c('VOICE_NP1_Recovery_No_', a_period)] 
    fV[,str_c('VOICE_RatioNP2_Recovery_', a_period)] = fV[,str_c('VOICE_NP2_Recovery_Yes_', a_period)] / fV[,str_c('VOICE_NP2_Recovery_No_', a_period)] 
    fV[,str_c('VOICE_RatioDur_Recovery_', a_period)] = fV[,str_c('VOICE_Dur_Recovery_Yes_', a_period)] / fV[,str_c('VOICE_Dur_Recovery_No_', a_period)] 
    fV[,str_c('VOICE_RatioDP1_Recovery_', a_period)] = fV[,str_c('VOICE_DP1_Recovery_Yes_', a_period)] / fV[,str_c('VOICE_DP1_Recovery_No_', a_period)] 
    fV[,str_c('VOICE_RatioDP2_Recovery_', a_period)] = fV[,str_c('VOICE_DP2_Recovery_Yes_', a_period)] / fV[,str_c('VOICE_DP2_Recovery_No_', a_period)] 

    #Support
    fV[,str_c('VOICE_RatioNum_Support_', a_period)] = fV[,str_c('VOICE_Num_Support_Unsupportive_', a_period)] / fV[,str_c('VOICE_Num_Support_Supportive_', a_period)] 
    fV[,str_c('VOICE_RatioNP1_Support_', a_period)] = fV[,str_c('VOICE_NP1_Support_Unsupportive_', a_period)] / fV[,str_c('VOICE_NP1_Support_Supportive_', a_period)] 
    fV[,str_c('VOICE_RatioNP2_Support_', a_period)] = fV[,str_c('VOICE_NP2_Support_Unsupportive_', a_period)] / fV[,str_c('VOICE_NP2_Support_Supportive_', a_period)] 
    fV[,str_c('VOICE_RatioDur_Support_', a_period)] = fV[,str_c('VOICE_Dur_Support_Unsupportive_', a_period)] / fV[,str_c('VOICE_Dur_Support_Supportive_', a_period)] 
    fV[,str_c('VOICE_RatioDP1_Support_', a_period)] = fV[,str_c('VOICE_DP1_Support_Unsupportive_', a_period)] / fV[,str_c('VOICE_DP1_Support_Supportive_', a_period)] 
    fV[,str_c('VOICE_RatioDP2_Support_', a_period)] = fV[,str_c('VOICE_DP2_Support_Unsupportive_', a_period)] / fV[,str_c('VOICE_DP2_Support_Supportive_', a_period)] 

    #emotion
    fV[,str_c('VOICE_RatioNum_Emotion_', a_period)] = fV[,str_c('VOICE_Num_Emotion_Unpleasant_', a_period)] / fV[,str_c('VOICE_Num_Emotion_Pleasant_', a_period)] 
    fV[,str_c('VOICE_RatioNP1_Emotion_', a_period)] = fV[,str_c('VOICE_NP1_Emotion_Unpleasant_', a_period)] / fV[,str_c('VOICE_NP1_Emotion_Pleasant_', a_period)] 
    fV[,str_c('VOICE_RatioNP2_Emotion_', a_period)] = fV[,str_c('VOICE_NP2_Emotion_Unpleasant_', a_period)] / fV[,str_c('VOICE_NP2_Emotion_Pleasant_', a_period)] 
    fV[,str_c('VOICE_RatioDur_Emotion_', a_period)] = fV[,str_c('VOICE_Dur_Emotion_Unpleasant_', a_period)] / fV[,str_c('VOICE_Dur_Emotion_Pleasant_', a_period)] 
    fV[,str_c('VOICE_RatioDP1_Emotion_', a_period)] = fV[,str_c('VOICE_DP1_Emotion_Unpleasant_', a_period)] / fV[,str_c('VOICE_DP1_Emotion_Pleasant_', a_period)] 
    fV[,str_c('VOICE_RatioDP2_Emotion_', a_period)] = fV[,str_c('VOICE_DP2_Emotion_Unpleasant_', a_period)] / fV[,str_c('VOICE_DP2_Emotion_Pleasant_', a_period)] 
  }
  #remove (set to NA) INF ratio scores
  fV = data.frame(sapply(fV, Inf2NA))
  
  #sort by subid and utc
  fV = arrange(fV, subid, utc)
  
  #merge fV into x
  if(is.null(x)){
    x = fV  #x was null so set to fV
  }else{
    #check that fV and x match
    if(nrow(x)!=nrow(fV)) stop('x and fV do not have same number of rows')
    if(any(fV$subid !=x$subid) | any(fV$utc !=x$utc)) stop('x and fV have mismatched subid and/or utc')
    
    #Remove subid and utc from fV b/c already in x
    fV$subid = NULL
    fV$utc = NULL
    x = data.frame(x,fV, stringsAsFactors=FALSE)
  } 
  
  return(x)
}


#add_x_SMSMeta-----------------
add_x_SMSMeta = function(y, x, dS)
{
  #setup parallel backend if available
  # nCore =detectCores()
  # if(nCore>2) {
  #   cl = makeCluster(nCore-1)
  #   registerDoParallel(cl)
  #   getDoParWorkers()
  # }
  # 
  periods = c('All', 'Week', '3Day')
  
  #loop through the Ys
  fS =  foreach (i = 1:nrow(y), .combine='rbind', .packages = c('stringr'), .export = c('get_some_data', 'Inf2NA')) %do%{ 
    #message(str_c('ROW: ', i)) #for serial testing
    
    #Features: Tot number(NUM) of incoming, outgoing, all 
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    
    for(a_period in periods){
      #message(a_period)
      dS1 = get_some_data(dS, y$subid[i],y$utc[i], period=a_period)
      
      if(nrow(dS1)>0){
        
        #NUM incoming/outgoing/all
        indices = dS1[,'type']==1
        t[1,str_c('SMS_Num_In_',a_period)] = sum(indices, na.rm=TRUE)
        indices = dS1[,'type']==2
        t[1,str_c('SMS_Num_Out_',a_period)] = sum(indices, na.rm=TRUE)
        t[1,str_c('SMS_Num_All_',a_period)] = nrow(dS1)
        
      }else{
        t[1,str_c('SMS_Num_In_',a_period)] = 0
        t[1,str_c('SMS_Num_Out_',a_period)] = 0
        t[1,str_c('SMS_Num_All_',a_period)] = 0
      }
      
    }
    fS1 = t #fS1 for 1 y is set to t b/c this is the first feature added 
    
    #DrankPast levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    ASMSVar = 'DrankPast'
    for(a_period in periods){
      dS1 = get_some_data(dS, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('Always', 'AlwaysOccas', 'Never')
      for(ALevel in Levels){
        if(ALevel == 'Always') indices = dS1[,ASMSVar]=='ALMOST ALWAYS/ALWAYS'
        if(ALevel == 'AlwaysOccas') indices = dS1[,ASMSVar]=='ALMOST ALWAYS/ALWAYS' |  dS1[,ASMSVar]=='OCCASIONALLY'
        if(ALevel == 'Never') indices = dS1[,ASMSVar]=='NEVER/ALMOST NEVER'

        if(nrow(dS1)>0){
          
          #Counts of all SMS by drank past level
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dS1) - sum(is.na(dS1[,ASMSVar]))>0){
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dS1) - sum(is.na(dS1[, ASMSVar])))
          }else{
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dS1)
          
        }else{
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fS1 = data.frame(fS1,t,stringsAsFactors=FALSE) #merge in these features into fS1 
    
    #DrinkerStatus levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    ASMSVar = 'DrinkerStatus'
    for(a_period in periods){
      dS1 = get_some_data(dS, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('Drinker', 'NonDrinker')
      for(ALevel in Levels){
        if(ALevel == 'Drinker') indices = dS1[,ASMSVar]=='DRINKER'
        if(ALevel == 'NonDrinker') indices = dS1[,ASMSVar]=='NONDRINKER'
        
        if(nrow(dS1)>0){
          
          #Counts of all calls by level
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dS1) - sum(is.na(dS1[,ASMSVar]))>0){
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dS1) - sum(is.na(dS1[, ASMSVar])))
          }else{
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dS1)
          
        }else{
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fS1 = data.frame(fS1,t,stringsAsFactors=FALSE) #merge in these features into fS1 
    
    #DrinkFuture levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    ASMSVar = 'DrinkFuture'
    for(a_period in periods){
      dS1 = get_some_data(dS, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('No', 'Yes')
      for(ALevel in Levels){
        if(ALevel == 'No') indices = dS1[,ASMSVar]=='NO'
        if(ALevel == 'Yes') indices = dS1[,ASMSVar]=='YES'
        
        if(nrow(dS1)>0){
          
          #Counts of all calls by level
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dS1) - sum(is.na(dS1[,ASMSVar]))>0){
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dS1) - sum(is.na(dS1[, ASMSVar])))
          }else{
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dS1)
        
        }else{
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fS1 = data.frame(fS1,t,stringsAsFactors=FALSE) #merge in these features into fS1     
    
    #Recovery levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    ASMSVar = 'Recovery'
    for(a_period in periods){
      dS1 = get_some_data(dS, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('No', 'Yes')
      for(ALevel in Levels){
        if(ALevel == 'No') indices = dS1[,ASMSVar]=='NO'
        if(ALevel == 'Yes') indices = dS1[,ASMSVar]=='YES'
        
        if(nrow(dS1)>0){
          
          #Counts of all calls by level
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dS1) - sum(is.na(dS1[,ASMSVar]))>0){
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dS1) - sum(is.na(dS1[, ASMSVar])))
          }else{
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dS1)
          
        }else{
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fS1 = data.frame(fS1,t,stringsAsFactors=FALSE) #merge in these features into fS1  
    
    
    #Support levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    ASMSVar = 'Support'
    for(a_period in periods){
      dS1 = get_some_data(dS, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('Unsupportive', 'UnsupMixed', 'Supportive')
      for(ALevel in Levels){
        if(ALevel == 'Unsupportive') indices = dS1[,ASMSVar]=='UNSUPPORTIVE'
        if(ALevel == 'UnsupMixed') indices = dS1[,ASMSVar]=='UNSUPPORTIVE' |  dS1[,ASMSVar]=='MIXED'
        if(ALevel == 'Supportive') indices = dS1[,ASMSVar]=='SUPPORTIVE'
        
        if(nrow(dS1)>0){
          
          #Counts of all calls by  level
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dS1) - sum(is.na(dS1[,ASMSVar]))>0){
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dS1) - sum(is.na(dS1[, ASMSVar])))
          }else{
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dS1)
          
        }else{
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fS1 = data.frame(fS1,t,stringsAsFactors=FALSE) #merge in these features into fS1 
 
    #emotion levels
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    ASMSVar = 'emotion'
    for(a_period in periods){
      dS1 = get_some_data(dS, y$subid[i],y$utc[i], period=a_period)
      
      Levels = c('Unpleasant', 'UnpleasMixed', 'Pleasant')
      for(ALevel in Levels){
        if(ALevel == 'Unpleasant') indices = dS1[,ASMSVar]=='UNPLEASANT'
        if(ALevel == 'UnpleasMixed') indices = dS1[,ASMSVar]=='UNPLEASANT' |  dS1[,ASMSVar]=='MIXED'
        if(ALevel == 'Pleasant') indices = dS1[,ASMSVar]=='PLEASANT'
        
        if(nrow(dS1)>0){
          
          #Counts of all calls by  level
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE)
          if(nrow(dS1) - sum(is.na(dS1[,ASMSVar]))>0){
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)] = sum(indices, na.rm=TRUE) / (nrow(dS1) - sum(is.na(dS1[, ASMSVar])))
          }else{
            t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_',a_period)]=NA
          }
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_',a_period)]  = sum(indices, na.rm=TRUE)/nrow(dS1)
          
        }else{
          t[1,str_c('SMS_Num_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP1_', ASMSVar, '_', ALevel, '_', a_period)] = 0
          t[1,str_c('SMS_NP2_', ASMSVar, '_', ALevel, '_', a_period)] = 0
        }
      }
    }
    fS1 = data.frame(fS1,t,stringsAsFactors=FALSE) #merge in these features into fS1 
    
    
    
    #Feature extraction done for S1  Now clean up and add to fS
    
    #Stamp features in fS1 for this y with subid and utc for this y
    fS1$subid =  y$subid[i]
    fS1$utc =  y$utc[i]
    
    fS1  #return fS1 for binding in each iteration
  }
  
  #fS created for all y.  Now clean up and add to x
  
  #close parallel backend
  # if(nCore>2) {
  #   stopCluster(cl)
  #   remove(cl)
  #   registerDoSEQ()
  # }  
  
  #calcalate ratio score features
  for (a_period in periods){
    fS[,str_c('SMS_RatioNum_InOut_', a_period)] = fS[,str_c('SMS_Num_In_', a_period)] / fS[,str_c('SMS_Num_Out_', a_period)] 

    #drankpast
    fS[,str_c('SMS_RatioNum_DrankPast_', a_period)] = fS[,str_c('SMS_Num_DrankPast_Always_', a_period)] / fS[,str_c('SMS_Num_DrankPast_Never_', a_period)] 
    fS[,str_c('SMS_RatioNP1_DrankPast_', a_period)] = fS[,str_c('SMS_NP1_DrankPast_Always_', a_period)] / fS[,str_c('SMS_NP1_DrankPast_Never_', a_period)] 
    fS[,str_c('SMS_RatioNP2_DrankPast_', a_period)] = fS[,str_c('SMS_NP2_DrankPast_Always_', a_period)] / fS[,str_c('SMS_NP2_DrankPast_Never_', a_period)] 

    #drinker status
    fS[,str_c('SMS_RatioNum_DrinkerStatus_', a_period)] = fS[,str_c('SMS_Num_DrinkerStatus_Drinker_', a_period)] / fS[,str_c('SMS_Num_DrinkerStatus_NonDrinker_', a_period)] 
    fS[,str_c('SMS_RatioNP1_DrinkerStatus_', a_period)] = fS[,str_c('SMS_NP1_DrinkerStatus_Drinker_', a_period)] / fS[,str_c('SMS_NP1_DrinkerStatus_NonDrinker_', a_period)] 
    fS[,str_c('SMS_RatioNP2_DrinkerStatus_', a_period)] = fS[,str_c('SMS_NP2_DrinkerStatus_Drinker_', a_period)] / fS[,str_c('SMS_NP2_DrinkerStatus_NonDrinker_', a_period)] 

    #drinker status
    fS[,str_c('SMS_RatioNum_DrinkFuture_', a_period)] = fS[,str_c('SMS_Num_DrinkFuture_Yes_', a_period)] / fS[,str_c('SMS_Num_DrinkFuture_No_', a_period)] 
    fS[,str_c('SMS_RatioNP1_DrinkFuture_', a_period)] = fS[,str_c('SMS_NP1_DrinkFuture_Yes_', a_period)] / fS[,str_c('SMS_NP1_DrinkFuture_No_', a_period)] 
    fS[,str_c('SMS_RatioNP2_DrinkFuture_', a_period)] = fS[,str_c('SMS_NP2_DrinkFuture_Yes_', a_period)] / fS[,str_c('SMS_NP2_DrinkFuture_No_', a_period)] 

    #Recovery
    fS[,str_c('SMS_RatioNum_Recovery_', a_period)] = fS[,str_c('SMS_Num_Recovery_Yes_', a_period)] / fS[,str_c('SMS_Num_Recovery_No_', a_period)] 
    fS[,str_c('SMS_RatioNP1_Recovery_', a_period)] = fS[,str_c('SMS_NP1_Recovery_Yes_', a_period)] / fS[,str_c('SMS_NP1_Recovery_No_', a_period)] 
    fS[,str_c('SMS_RatioNP2_Recovery_', a_period)] = fS[,str_c('SMS_NP2_Recovery_Yes_', a_period)] / fS[,str_c('SMS_NP2_Recovery_No_', a_period)] 

    #Support
    fS[,str_c('SMS_RatioNum_Support_', a_period)] = fS[,str_c('SMS_Num_Support_Unsupportive_', a_period)] / fS[,str_c('SMS_Num_Support_Supportive_', a_period)] 
    fS[,str_c('SMS_RatioNP1_Support_', a_period)] = fS[,str_c('SMS_NP1_Support_Unsupportive_', a_period)] / fS[,str_c('SMS_NP1_Support_Supportive_', a_period)] 
    fS[,str_c('SMS_RatioNP2_Support_', a_period)] = fS[,str_c('SMS_NP2_Support_Unsupportive_', a_period)] / fS[,str_c('SMS_NP2_Support_Supportive_', a_period)] 
 
    #emotion
    fS[,str_c('SMS_RatioNum_Emotion_', a_period)] = fS[,str_c('SMS_Num_Emotion_Unpleasant_', a_period)] / fS[,str_c('SMS_Num_Emotion_Pleasant_', a_period)] 
    fS[,str_c('SMS_RatioNP1_Emotion_', a_period)] = fS[,str_c('SMS_NP1_Emotion_Unpleasant_', a_period)] / fS[,str_c('SMS_NP1_Emotion_Pleasant_', a_period)] 
    fS[,str_c('SMS_RatioNP2_Emotion_', a_period)] = fS[,str_c('SMS_NP2_Emotion_Unpleasant_', a_period)] / fS[,str_c('SMS_NP2_Emotion_Pleasant_', a_period)] 
  }
  #remove (set to NA) INF ratio scores
  fS = data.frame(sapply(fS, Inf2NA))
  
  #sort by subid and utc
  fS = fS[order(fS$subid,fS$utc),]
  
  #merge fS into x
  if(is.null(x)){
    x = fS  #x was null so set to fS
  }else{
    #check that fS and x match
    if(nrow(x)!=nrow(fS)) stop('x and fS do not have same number of rows')
    if(any(fS$subid !=x$subid) | any(fS$utc !=x$utc)) stop('x and fS have mismatched subid and/or utc')
    
    #Remove subid and utc from fS b/c already in x
    fS$subid = NULL
    fS$utc = NULL
    x = data.frame(x,fS, stringsAsFactors=FALSE)
  } 
  
  return(x)
}



#add_x_Audio-----------------
add_x_Audio = function(y, x, dA)
{

  #extract features from raw SVDs
  fA= NULL #initial Voice feature dataframe to Null to start
  
  #loop through the Ys
  for (i in 1:nrow(y))
  {
    
    #Features: Mean of All Bag of Words
    periods = c('All')
    BOWs = names(dA)[str_detect(names(dA),'BOW')]
    t=data.frame(matrix(NA, nrow=1, ncol=length(periods)*length(BOWs))) #NA dataframe with 1 row
    names(t)= str_c('AUDIO_Mean', BOWs, '_',rep(periods,each=length(BOWs)))
    for(a_period in periods){
      dA1 = get_some_data(dA, y$subid[i],y$utc[i], period=a_period)
      dA1 = dA1[,BOWs]
      r = sapply(dA1, safe_mean, na.rm=TRUE)
      t[1,str_c('AUDIO_Mean', BOWs, '_',a_period)] = r
      # for(aBOW in BOWs){
      #   t[1,str_c('AUDIO_Mean', aBOW, '_',a_period)] = safe_mean(dA1[,aBOW], na.rm=TRUE)
      # }
    }
    fA1 = t #fA1 for 1 y is set to t b/c this is the first feature added
   
    
    #Features: Mean,Min, Max in 3 day, 1 week, and All for K SVDs
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    
    periods = c('All')
    for(a_period in periods){
      dA1 = get_some_data(dA, y$subid[i],y$utc[i], period=a_period)
      
      SVDs = names(dA)[str_detect(names(dA),'SVD')]
      for(aSVD in SVDs){
        t[1,str_c('AUDIO_Mean', aSVD, '_',a_period)] = safe_mean(dA1[,aSVD], na.rm=TRUE)
        t[1,str_c('AUDIO_Max', aSVD, '_',a_period)] = safe_max(dA1[,aSVD], na.rm=TRUE)
        t[1,str_c('AUDIO_Min', aSVD, '_',a_period)] = safe_min(dA1[,aSVD], na.rm=TRUE)
      }
    }
    fA1 = data.frame(fA1,t,stringsAsFactors=FALSE) #merge in these features into fA1 

    
    #Features: Mean, Min, Max in 3 day, 1 week, and All for K Word2Vec Vectors
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    
    periods = c('All')
    for(a_period in periods){
      dA1 = get_some_data(dA, y$subid[i],y$utc[i], period=a_period)
      
      WVs = names(dA)[str_detect(names(dA),'WV')]
      for(aWV in WVs){
        t[1,str_c('AUDIO_Mean', aWV, '_',a_period)] = safe_mean(dA1[,aWV], na.rm=TRUE)
        t[1,str_c('AUDIO_Max', aWV, '_',a_period)] = safe_max(dA1[,aWV], na.rm=TRUE)
        t[1,str_c('AUDIO_Min', aWV, '_',a_period)] = safe_min(dA1[,aWV], na.rm=TRUE)
      }
    }
    fA1 = data.frame(fA1,t,stringsAsFactors=FALSE) #merge in these features into fA1 
    
    
    #Feature extraction done for fA1  Now clean up and add to fA
    
    #Stamp features in fA1 for this y with subid and utc for this y
    fA1$subid =  y$subid[i]
    fA1$utc =  y$utc[i]
    
    #Merge all Voice features for 1 y into full feature dataframe fV
    if(is.null(fA)){ #if merging features for first y
      fA = fA1
    }else{
      fA = dfMerge(fA,fA1,AddVars = FALSE)
    }
  }
  
  #fA created for all y.  Now clean up and add to x
  
  #sort by subid and utc
  fA = fA[order(fA$subid,fA$utc),]
  
  #merge fA into x
  if(is.null(x)){
    x = fA  #x was null so set to fA
  }else{
    #check that fA and x match
    if(nrow(x)!=nrow(fA)) stop('x and fA do not have same number of rows')
    if(any(fA$subid !=x$subid) | any(fA$utc !=x$utc)) stop('x and fA have mismatched subid and/or utc')
    
    #Remove subid and utc from fA b/c already in x
    fA$subid = NULL
    fA$utc = NULL
    x = data.frame(x,fA, stringsAsFactors=FALSE)
  } 
  
  return(x)
}


#get_some_data------------------------
#get all data relative to utc time stamp
#Can use All, Day, 3Day, Week
get_some_data = function(d, ASubID , AUTC, period='All', Names=NULL)
{
  #quick integrity checks
  if(any(is.na(d$utc))) stop('Missing UTCs in dataframe')
  if(any(is.na(d$subid))) stop('Missing SubIDs in dataframe')
  
  #select only subid, utc, and Names columns
  if(!is.null(Names)) {
    d = select(d, c('utc', 'subid', Names))
  }
  
  if(period=='All') {
    d = filter(d,subid==ASubID & utc<AUTC)
  }
  if(period == 'Day'){
    d = filter(d,subid==ASubID & utc<AUTC & utc >=(AUTC-24*60*60))
  }
  if(period == '3Day'){
    d = filter(d,subid==ASubID & utc<AUTC & utc >=(AUTC-3*24*60*60))
  }
  if(period == 'Week'){
    d = filter(d,subid==ASubID & utc<AUTC & utc >=(AUTC-7*24*60*60))
  }
  
  return(d)
}


#safe_mean ------------------------
safe_mean = function(v, na.rm=FALSE) {
  if(na.rm==TRUE) v=na.omit(v)
  if (length(v)==0) {
    return(NA) }
  else {
    return(mean(v,na.rm=na.rm))
  }
}


#safe_max ------------------------
safe_max = function(v, na.rm=FALSE) {
  if(na.rm==TRUE) v=na.omit(v)
  if (length(v)==0) {
    return(NA) }
  else {
    return(max(v,na.rm=na.rm))
  }
}

#safe_min ------------------------
safe_min = function(v, na.rm=FALSE) {
  if(na.rm==TRUE) v=na.omit(v)
  if (length(v)==0) {
    return(NA) }
  else {
    return(min(v,na.rm=na.rm))
  }
}

#safe_sd ------------------------
safe_sd = function(v, na.rm=FALSE) {
  if (all(is.na(v))) {
    return(NA) }
  else {
    return(sd(v,na.rm=na.rm))
  }
}

#safe_last ------------------------
safe_last = function(v, na.rm=FALSE) {
  if(na.rm==TRUE) v = na.omit(v)
  r = v[length(v)]
  if(length(r)==0) r=NA
  return(r)
}
# 
# #formulaSet ------------------------ 
# formulaSet = function(Names,String, Match=TRUE)
# {
#   if(Match){
#     NameMatches = Names[str_detect(Names, String)]
#   }else{
#     NameMatches = Names[!str_detect(Names, String)]
#   }
#   if (length(NameMatches)<2) stop('Names vector must include at least 2 matches')
#   
#   Final = NameMatches[length(NameMatches)]
#   NameMatches = NameMatches[1:(length(NameMatches)-1)]
#   NameMatches = str_c(NameMatches,collapse=' + ')
#   NameMatches = str_c('(', NameMatches, ' + ', Final, ')')
#   return(NameMatches)
# }