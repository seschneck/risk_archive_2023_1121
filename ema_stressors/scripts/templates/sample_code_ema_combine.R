#TAKEN FROM mak_EMACombined.R FROM STRESSOR USE PROJECT
#TO BE USED AS AN EXAMPLE ON HOW TO COMINE REQUESTED/UNREQUESTED EMA

# Create a loop to determine which EMA responses are valid (i.e., linked to just one rolodex EMA prompt):
#This dataframe will NOT be big enough.  Need to adjust when it is filled
dF = tibble(SubID=double(), RoloTime = character(), EMATime = character(),TimeToComplete=double(), Craving=double(), Anxious=double(), Irritable=double(), Sad=double(), Happy=double(), StressfulEvent=double(),
            PositiveEvent=double(), Cigs=double(), Lozenges=double(), Patch=double())
dF$RoloTime = as.POSIXct(dF$RoloTime, tz = "GMT")
dF$EMATime = as.POSIXct(dF$EMATime, tz = "GMT")
dFCtr = 0

SubIDs = unique(dEMA$SubID)

for(ASubID in SubIDs)
{
  print(ASubID)
  dE = filter(dEMA, SubID ==ASubID)
  
  # Open Rolodex for this SubID
  if(file.exists(file.path('P:/StudyData/NRT1/RawData', ASubID, str_c(ASubID, '_EMARolodex.txt'))))
  {
    dR = read.csv(file.path('P:/StudyData/NRT1/RawData', ASubID, str_c(ASubID, '_EMARolodex.txt')))
    dR$RequestTime = as.POSIXct(x=str_c(dR$SendDate, dR$SendTime), format='%d-%b-%y %I:%M %p', tz = "America/Chicago")
    attributes(dR$RequestTime)$tzone = 'GMT'
  }else{
    dR = read.csv(file.path('P:/StudyData/NRT1/RawData', ASubID, str_c('EMArolodex', ASubID, '.csv')))
    dR$RequestTime = as.POSIXct(x=str_c(dR$SendDate, dR$SendTime), format='%d-%b-%y %I:%M %p', tz = "GMT")
  }
  
  #CLUDGE FIX for 12139.  WILL ASK SUSAN TO FIGURE OUT WHY IN MILITARY RATHER THAN AM/PM hours
  if (ASubID == 12139) {
    dR = read.csv(file.path('P:/StudyData/NRT1/RawData', ASubID, str_c('EMArolodex', ASubID, '.csv')))
    dR$RequestTime = as.POSIXct(x=str_c(dR$SendDate, dR$SendTime), format='%d-%b-%y %H:%M', tz = "GMT")
  }
  
  if(nrow(dR)<84) warning('Incorrect number of rows in Rolodex for ', ASubID, immediate.=TRUE)
  
  dR$UserResponse=NULL
  dR$ReminderCount =NULL
  dR$SendDate = NULL
  dR$SendTime = NULL
  dR = filter(dR,!is.na(RequestTime)) #remove requests if the Requesttime was malformed
  
  dECtr =1
  dRCtr=1
  while(dRCtr<= (nrow(dR)-1))
  {
    #print(dRCtr)
    #If no EMA before next rolo request
    if ((dECtr<= nrow(dE)) && (dE$StartDate[dECtr] >= dR$RequestTime[dRCtr+1]))
    {
      dFCtr = dFCtr + 1
      dF[dFCtr, ] = NA
      dF$SubID[dFCtr] = ASubID
      dF$RoloTime[dFCtr] = dR$RequestTime[dRCtr]
    }
    else
    {
      FirstEMAResp = TRUE
      while((dECtr<= nrow(dE)) && (dE$StartDate[dECtr] < dR$RequestTime[dRCtr+1]))
      {
        dFCtr = dFCtr + 1
        dF[dFCtr, ] = NA
        dF$SubID[dFCtr] = ASubID
        
        if(FirstEMAResp) dF$RoloTime[dFCtr] = dR$RequestTime[dRCtr]
        
        dF$EMATime[dFCtr] = dE$StartDate[dECtr]
        dF$TimeToComplete[dFCtr] = dE$TimeToComplete[dECtr]
        dF$Craving[dFCtr] = dE$Craving[dECtr]
        dF$Anxious[dFCtr] = dE$Anxious[dECtr]
        dF$Irritable[dFCtr] = dE$Irritable[dECtr]
        dF$Sad[dFCtr] = dE$Sad[dECtr]
        dF$Happy[dFCtr] = dE$Happy[dECtr]
        dF$StressfulEvent[dFCtr] = dE$StressfulEvent[dECtr]
        dF$PositiveEvent[dFCtr] = dE$PositiveEvent[dECtr]
        dF$Cigs[dFCtr] = dE$Cigs[dECtr]
        dF$Lozenges[dFCtr] = dE$Lozenges[dECtr]
        dF$Patch[dFCtr] = dE$Patch[dECtr]
        FirstEMAResp = FALSE
        dECtr = dECtr+1
      }
    }
    dRCtr= dRCtr + 1
  }
  
  #handle last Rolodex
  dFCtr = dFCtr + 1
  dF[dFCtr, ] = NA
  dF$SubID[dFCtr] = ASubID
  dF$RoloTime[dFCtr] = dR$RequestTime[dRCtr]
  
  FirstEMAResp = TRUE
  while(dECtr <= nrow(dE))
  {  
    if (!FirstEMAResp)
    {
      dFCtr = dFCtr + 1
      dF[dFCtr, ] = NA
      dF$SubID[dFCtr] = ASubID
    }
    dF$EMATime[dFCtr] = dE$StartDate[dECtr]
    dF$TimeToComplete[dFCtr] = dE$TimeToComplete[dECtr]
    dF$Craving[dFCtr] = dE$Craving[dECtr]
    dF$Anxious[dFCtr] = dE$Anxious[dECtr]
    dF$Irritable[dFCtr] = dE$Irritable[dECtr]
    dF$Sad[dFCtr] = dE$Sad[dECtr]
    dF$Happy[dFCtr] = dE$Happy[dECtr]
    dF$StressfulEvent[dFCtr] = dE$StressfulEvent[dECtr]
    dF$PositiveEvent[dFCtr] = dE$PositiveEvent[dECtr]
    dF$Cigs[dFCtr] = dE$Cigs[dECtr]
    dF$Lozenges[dFCtr] = dE$Lozenges[dECtr]
    dF$Patch[dFCtr] = dE$Patch[dECtr]
    FirstEMAResp = FALSE
    dECtr = dECtr+1
  }
}
################################################################################################
#TAKEN FROM mak_EMAConcurrent.R FROM STRESSOR USE PROJECT
#TO BE USED AS AN EXAMPLE FOR HOW TO COMBINE REQUESTED/UNREQUESTED EMA


#RULES FOR CLEANING/COMBINING
#Use all emas from post-quit till last rolo ema. Ignore extra emas after last rolo
#report max stressful event in period
#if multiple max stressful events (or all equal), choose row for first max
#report "right now" items (e.g, craving, affect items) from same EMA as chosen max stressful event
#report sum of cigs and lozenges
#report mean of patch use
#report max for positive event (note: may be from a different EMA/row than max stressful event)
d_fin = NULL
for (isubid in unique(d$subid)){
  
  print(str_c('Cleaning EMA for: ', isubid))
  d_s_raw = filter(d,subid == isubid)
  rolo_rows = which(!is.na(d_s_raw$rolo_time))
  
  d_s_fin = foreach(irolo = 1:length(rolo_rows), .combine = 'rbind') %do%{   #
    
    #get rows for this rolo.  1st rolo is special case
    if(irolo==1){
      d_s_ema = slice(d_s_raw, 1:rolo_rows[irolo])
    }else{
      d_s_ema = slice(d_s_raw, (rolo_rows[irolo-1]+1):rolo_rows[irolo])
    }
    
    if(nrow(d_s_ema)==1){ #only one EMA
      d_s_fin = d_s_ema
      d_s_fin$ema_time_start = d_s_ema$ema_time
      d_s_fin$ema_time_end = d_s_ema$ema_time
      d_s_fin$n_ema = 1
    }else{
      d_s_fin = slice(d_s_ema, which(stressful_event==max(stressful_event, na.rm=TRUE)))
      d_s_fin = slice(d_s_fin,1)  #if there was more than one max, take the first/earliest ema in window
      d_s_fin$cigs = sum(d_s_ema$cigs, na.rm=TRUE)
      d_s_fin$lozenges = sum(d_s_ema$lozenges, na.rm=TRUE)
      d_s_fin$patch = mean(d_s_ema$patch, na.rm=TRUE)
      d_s_fin$positive_event = max(d_s_ema$positive_event, na.rm=TRUE)
      
      d_s_fin$rolo_time = d_s_ema$rolo_time[nrow(d_s_ema)]
      ema_times = d_s_ema$ema_time
      ema_times = ema_times[!is.na(ema_times)]
      d_s_fin$ema_time_start = ema_times[1]
      d_s_fin$ema_time_end = ema_times[length(ema_times)]
      d_s_fin$n_ema = sum(!is.na(d_s_ema$ema_time))
      
    }
    d_s_fin
  }
  d_fin = bind_rows(d_fin,d_s_fin)
}
