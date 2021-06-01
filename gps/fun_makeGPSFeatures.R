#Function to make GPS features-------------------------------------------------------
#Y = labels
#X = feature matrix of other features produced by other fuctions
#dGPS = raw GPS data for creating GPS features

makeGPSFeatures = function(Y, X, dGPS)
{
  
  dGPSp = subset(dGPS, Type=='PLACE')
  fG= NULL #initial GPS feature dataframe to Null to start
  
  #loop through the Ys
  for (i in 1:nrow(Y))
  {
    #Setup feature vector/row for 1 lapse label with SubID and timestamp
    fG1 = data.frame(SubID = Y$SubID[i], UTC =  Y$UTC[i])
    
    #Features: Tot number of drank/alcohol/avoid places, 
    #proportion1 (vs. no) of drank/alcohol/avoid places, 
    #proportion2 (vs. all) of drank/alcohol/avoid places, 
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty temporary dataframe with 1 row
    
    Periods = c('All', 'Week', '3Day')
    for(APeriod in Periods){
      dG1 = getSomeData(dGPSp, Y$SubID[i],Y$UTC[i], Period=APeriod)
      
      GPSVars = c('Drank', 'Alcohol', 'Avoid')
      for(AGPSVar in GPSVars){
        if(nrow(dG1)>0){
          indices = dG1[,AGPSVar]=='YES'
          
          #counts
          t[1,str_c('GPS_Num_', AGPSVar, '_',APeriod)] = sum(indices, na.rm=TRUE)
          if(nrow(dG1) - sum(is.na(dG1[,AGPSVar]))>0){
            t[1,str_c('GPS_NP1_', AGPSVar, '_',APeriod)] = sum(indices, na.rm=TRUE) / (nrow(dG1) - sum(is.na(dG1[,AGPSVar])))
          }else{
            t[1,str_c('GPS_NP1_', AGPSVar, '_',APeriod)]=NA
          }
          t[1,str_c('GPS_NP2_', AGPSVar, '_',APeriod)]  = sum(indices, na.rm=TRUE)/nrow(dG1)
          
          #time at places including home
          t[1,str_c('GPS_Time1_', AGPSVar, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE) 
          if(sum(subset(dG1$NextTime,!is.na(dG1[,AGPSVar])), na.rm=TRUE)>0){
            t[1,str_c('GPS_T1P1_', AGPSVar, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE)/ sum(subset(dG1$NextTime,!is.na(dG1[,AGPSVar])), na.rm=TRUE) 
          }else{
            t[1,str_c('GPS_T1P1_', AGPSVar, '_',APeriod)] = NA
          }
          t[1,str_c('GPS_T1P2_', AGPSVar, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE) / sum(dG1$NextTime, na.rm=TRUE) 
          
          #time at places excluding home
          dG1a = subset(dG1,PlaceType!='HOME' | is.na(dG1$PlaceType))
          indices = dG1a[,AGPSVar]=='YES'
          t[1,str_c('GPS_Time2_', AGPSVar, '_',APeriod)] = sum(subset(dG1a$NextTime,indices), na.rm=TRUE) 
          if(sum(subset(dG1a$NextTime,!is.na(dG1a[,AGPSVar])), na.rm=TRUE)>0){
            t[1,str_c('GPS_T2P1_', AGPSVar, '_',APeriod)] = sum(subset(dG1a$NextTime,indices), na.rm=TRUE)/ sum(subset(dG1a$NextTime,!is.na(dG1a[,AGPSVar])), na.rm=TRUE) 
          }else{
            t[1,str_c('GPS_T2P1_', AGPSVar, '_',APeriod)] = NA
          }
          if(sum(dG1a$NextTime, na.rm=TRUE)){ 
            t[1,str_c('GPS_T2P2_', AGPSVar, '_',APeriod)] = sum(subset(dG1a$NextTime,indices), na.rm=TRUE) / sum(dG1a$NextTime, na.rm=TRUE) 
          }else{
            t[1,str_c('GPS_T2P2_', AGPSVar, '_',APeriod)] =NA
          }
        }else{
          t[1,str_c('GPS_Num_', AGPSVar, '_',APeriod)] = NA
          t[1,str_c('GPS_NP1_', AGPSVar, '_',APeriod)] = NA
          t[1,str_c('GPS_NP2_', AGPSVar, '_',APeriod)] = NA
          
          t[1,str_c('GPS_Time1_', AGPSVar, '_',APeriod)] = NA
          t[1,str_c('GPS_T1P1_', AGPSVar, '_',APeriod)] = NA
          t[1,str_c('GPS_T1P2_', AGPSVar, '_',APeriod)] = NA
          
          t[1,str_c('GPS_Time2_', AGPSVar, '_',APeriod)] = NA
          t[1,str_c('GPS_T2P1_', AGPSVar, '_',APeriod)] = NA
          t[1,str_c('GPS_T2P2_', AGPSVar, '_',APeriod)] = NA
        }
      }
    }
    fG1 = data.frame(fG1,t,stringsAsFactors=FALSE) #merge in these features into fG1
    #fG1 = t #fG1 for 1 Y is set to t b/c this is the first feature added 
    
    
    #Features: Tot number of High, High/Medium, and Any Risk places, 
    #proportion1 (vs. no) of High, High/Medium, and Any Risk places, 
    #proportion2 (vs. all) of High, High/Medium, and Any Risk places, 
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    
    Periods = c('All', 'Week', '3Day')
    for(APeriod in Periods){
      dG1 = getSomeData(dGPSp, Y$SubID[i],Y$UTC[i], Period=APeriod)
      
      RiskLevels = c('HighRisk', 'HighMedRisk', 'AnyRisk')
      for(ARiskLev in RiskLevels){
        if(ARiskLev == 'HighRisk') indices = dG1$Risk=='HIGH'
        if(ARiskLev == 'HighMedRisk') indices = dG1$Risk=='HIGH' |  dG1$Risk=='MEDIUM'
        if(ARiskLev == 'AnyRisk') indices = dG1$Risk=='HIGH' |  dG1$Risk=='MEDIUM' |  dG1$Risk=='LOW'
        
        if(nrow(dG1)>0){
          
          #counts
          t[1,str_c('GPS_Num_', ARiskLev, '_',APeriod)] = sum(indices, na.rm=TRUE)
          if(nrow(dG1) - sum(is.na(dG1$Risk))>0){
            t[1,str_c('GPS_NP1_', ARiskLev, '_',APeriod)] = sum(indices, na.rm=TRUE)/(nrow(dG1) - sum(is.na(dG1$Risk)))
          }else{
            t[1,str_c('GPS_NP1_', ARiskLev, '_',APeriod)] =NA
          }
          t[1,str_c('GPS_NP2_', ARiskLev, '_',APeriod)]  = sum(indices, na.rm=TRUE)/nrow(dG1)
          
          #time at places including home
          t[1,str_c('GPS_Time1_', ARiskLev, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE) 
          if(sum(subset(dG1$NextTime,!is.na(dG1$Risk)), na.rm=TRUE)>0){
            t[1,str_c('GPS_T1P1_', ARiskLev, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE)/ sum(subset(dG1$NextTime,!is.na(dG1$Risk)), na.rm=TRUE) 
          }else{
            t[1,str_c('GPS_T1P1_', ARiskLev, '_',APeriod)] = NA
          }
          t[1,str_c('GPS_T1P2_', ARiskLev, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE) / sum(dG1$NextTime, na.rm=TRUE) 
          
          #time at places excluding home
          dG1a = subset(dG1,PlaceType!='HOME' | is.na(dG1$PlaceType))
          if(ARiskLev == 'HighRisk') indices = dG1a$Risk=='HIGH'
          if(ARiskLev == 'HighMedRisk') indices = dG1a$Risk=='HIGH' |  dG1a$Risk=='MEDIUM'
          if(ARiskLev == 'AnyRisk') indices = dG1a$Risk=='HIGH' |  dG1a$Risk=='MEDIUM' |  dG1a$Risk=='LOW'
          
          t[1,str_c('GPS_Time2_', ARiskLev, '_',APeriod)] = sum(subset(dG1a$NextTime,indices), na.rm=TRUE) 
          if(sum(subset(dG1a$NextTime,!is.na(dG1a$Risk)), na.rm=TRUE)>0){
            t[1,str_c('GPS_T2P1_', ARiskLev, '_',APeriod)] = sum(subset(dG1a$NextTime,indices), na.rm=TRUE)/ sum(subset(dG1a$NextTime,!is.na(dG1a$Risk)), na.rm=TRUE) 
          }else{
            t[1,str_c('GPS_T2P1_', ARiskLev, '_',APeriod)] = NA
          }
          if(sum(dG1a$NextTime, na.rm=TRUE)){ 
            t[1,str_c('GPS_T2P2_', ARiskLev, '_',APeriod)] = sum(subset(dG1a$NextTime,indices), na.rm=TRUE) / sum(dG1a$NextTime, na.rm=TRUE) 
          }else{
            t[1,str_c('GPS_T2P2_', ARiskLev, '_',APeriod)] =NA
          }
          
        }else{
          t[1,str_c('GPS_Num_', ARiskLev, '_',APeriod)] = NA
          t[1,str_c('GPS_NP1_', ARiskLev, '_',APeriod)] = NA
          t[1,str_c('GPS_NP2_', ARiskLev, '_',APeriod)] = NA
          
          t[1,str_c('GPS_Time1_', ARiskLev, '_',APeriod)] = NA
          t[1,str_c('GPS_T1P1_', ARiskLev, '_',APeriod)] = NA
          t[1,str_c('GPS_T1P2_', ARiskLev, '_',APeriod)] = NA
          
          t[1,str_c('GPS_Time2_', ARiskLev, '_',APeriod)] = NA
          t[1,str_c('GPS_T2P1_', ARiskLev, '_',APeriod)] = NA
          t[1,str_c('GPS_T2P2_', ARiskLev, '_',APeriod)] = NA
        }
      }
    }
    fG1 = data.frame(fG1,t,stringsAsFactors=FALSE) #merge in these features into fG1
    
    #Features: Tot number of Unpleasant, Mixed, Pleasant and Any affect places, 
    #proportion1 (vs. no) of Unpleasant, Mixed, Pleasant and Any affect  places, 
    #proportion2 (vs. all) of Unpleasant, Mixed, Pleasant and Any affect  places, 
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    
    Periods = c('All', 'Week', '3Day')
    for(APeriod in Periods){
      dG1 = getSomeData(dGPSp, Y$SubID[i],Y$UTC[i], Period=APeriod)
      
      EmotionLevels = c('UNPLEASANT', 'MIXED', 'PLEASANT', 'ANYNEG', 'ANYPOS', 'ANYAFFECT')
      for(AEmoLev in EmotionLevels){
        if(AEmoLev == 'UNPLEASANT') indices = dG1$Emotion=='UNPLEASANT'
        if(AEmoLev == 'MIXED') indices = dG1$Emotion=='MIXED'
        if(AEmoLev == 'PLEASANT') indices = dG1$Emotion=='PLEASANT'
        if(AEmoLev == 'ANYNEG') indices = dG1$Emotion=='UNPLEASANT' |  dG1$Emotion=='MIXED' 
        if(AEmoLev == 'ANYPOS') indices = dG1$Emotion=='PLEASANT' |  dG1$Emotion=='MIXED' 
        if(AEmoLev == 'ANYAFFECT') indices = dG1$Emotion=='UNPLEASANT' |  dG1$Emotion=='MIXED' |  dG1$Emotion=='PLEASANT'
        
        if(nrow(dG1)>0){
          
          #counts
          t[1,str_c('GPS_Num_', AEmoLev, '_',APeriod)] = sum(indices, na.rm=TRUE)
          if(nrow(dG1) - sum(is.na(dG1$Emotion))>0){
            t[1,str_c('GPS_NP1_', AEmoLev, '_',APeriod)] = sum(indices, na.rm=TRUE)/(nrow(dG1) - sum(is.na(dG1$Emotion)))
          }else
          {
            t[1,str_c('GPS_NP1_', AEmoLev, '_',APeriod)] =NA
          }
          t[1,str_c('GPS_NP2_', AEmoLev, '_',APeriod)]  = sum(indices, na.rm=TRUE)/nrow(dG1)
          
          #time at places including home
          t[1,str_c('GPS_Time1_', AEmoLev, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE) 
          if(sum(subset(dG1$NextTime,!is.na(dG1$Emotion)), na.rm=TRUE)>0){
            t[1,str_c('GPS_T1P1_', AEmoLev, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE)/ sum(subset(dG1$NextTime,!is.na(dG1$Emotion)), na.rm=TRUE) 
          }else{
            t[1,str_c('GPS_T1P1_', AEmoLev, '_',APeriod)] = NA
          }
          t[1,str_c('GPS_T1P2_', AEmoLev, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE) / sum(dG1$NextTime, na.rm=TRUE) 
          
          #time at places excluding home
          dG1a = subset(dG1,PlaceType!='HOME' | is.na(dG1$PlaceType))
          if(AEmoLev == 'UNPLEASANT') indices = dG1a$Emotion=='UNPLEASANT'
          if(AEmoLev == 'MIXED') indices = dG1a$Emotion=='MIXED'
          if(AEmoLev == 'PLEASANT') indices = dG1a$Emotion=='PLEASANT'
          if(AEmoLev == 'ANYNEG') indices = dG1a$Emotion=='UNPLEASANT' |  dG1a$Emotion=='MIXED' 
          if(AEmoLev == 'ANYPOS') indices = dG1a$Emotion=='PLEASANT' |  dG1a$Emotion=='MIXED' 
          if(AEmoLev == 'ANYAFFECT') indices = dG1a$Emotion=='UNPLEASANT' |  dG1a$Emotion=='MIXED' |  dG1a$Emotion=='PLEASANT'
          
          t[1,str_c('GPS_Time2_', AEmoLev, '_',APeriod)] = sum(subset(dG1a$NextTime,indices), na.rm=TRUE) 
          if(sum(subset(dG1a$NextTime,!is.na(dG1a$Emotion)), na.rm=TRUE)>0){
            t[1,str_c('GPS_T2P1_', AEmoLev, '_',APeriod)] = sum(subset(dG1a$NextTime,indices), na.rm=TRUE)/ sum(subset(dG1a$NextTime,!is.na(dG1a$Emotion)), na.rm=TRUE) 
          }else{
            t[1,str_c('GPS_T2P1_', AEmoLev, '_',APeriod)] = NA
          }
          if(sum(dG1a$NextTime, na.rm=TRUE)){ 
            t[1,str_c('GPS_T2P2_', AEmoLev, '_',APeriod)] = sum(subset(dG1a$NextTime,indices), na.rm=TRUE) / sum(dG1a$NextTime, na.rm=TRUE) 
          }else{
            t[1,str_c('GPS_T2P2_', AEmoLev, '_',APeriod)] =NA
          }    
          
        }else{
          t[1,str_c('GPS_Num_', AEmoLev, '_',APeriod)] = NA
          t[1,str_c('GPS_NP1_', AEmoLev, '_',APeriod)] = NA
          t[1,str_c('GPS_NP2_', AEmoLev, '_',APeriod)] = NA
          
          t[1,str_c('GPS_Time1_', AEmoLev, '_',APeriod)] = NA
          t[1,str_c('GPS_T1P1_', AEmoLev, '_',APeriod)] = NA
          t[1,str_c('GPS_T1P2_', AEmoLev, '_',APeriod)] = NA
          
          t[1,str_c('GPS_Time2_', AEmoLev, '_',APeriod)] = NA
          t[1,str_c('GPS_T2P1_', AEmoLev, '_',APeriod)] = NA
          t[1,str_c('GPS_T2P2_', AEmoLev, '_',APeriod)] = NA
        }
      }
    }
    fG1 = data.frame(fG1,t,stringsAsFactors=FALSE) #merge in these features into fG1
    
    
    #Features: Time at home, work, volunteer, bar, etc 
    t=data.frame(matrix(NA, nrow=1, ncol=0)) #empty dataframe with 1 row
    
    Periods = c('All', 'Week', '3Day')
    for(APeriod in Periods){
      dG1 = getSomeData(dGPSp, Y$SubID[i],Y$UTC[i], Period=APeriod)
      
      Locations = c('Home', 'Work', 'Volunteer', 'School', 'AA', 'Bar', 'Gym', 'HealthCare', 'LiquorStore','Restaurant', 'Cafe', 'Friend', 'Family')
      for(ALocation in Locations){
        if(ALocation == 'Home') indices = dG1a$PlaceType=='HOME'
        if(ALocation == 'Work') indices = dG1a$PlaceType=='WORK'
        if(ALocation == 'Volunteer') indices = dG1a$PlaceType=='VOLUNTEER'
        if(ALocation == 'School') indices = dG1a$PlaceType=='SCHOOL'
        if(ALocation == 'AA') indices = dG1a$PlaceType=='AA/RECOVERY MEETING'
        if(ALocation == 'Bar') indices = dG1a$PlaceType=='BAR'
        if(ALocation == 'Gym') indices = dG1a$PlaceType=='GYM/FITNESS CENTER'
        if(ALocation == 'HealthCare') indices = dG1a$PlaceType=='HEALTH CARE'
        if(ALocation == 'LiquorStore') indices = dG1a$PlaceType=='LIQUOR STORE'
        if(ALocation == 'Restaurant') indices = dG1a$PlaceType=='RESTAURANT'
        if(ALocation == 'Cafe') indices = dG1a$PlaceType=='COFFEE SHOP/CAFE'
        if(ALocation == 'Friend') indices = dG1a$PlaceType=='HOME OF FRIEND'
        if(ALocation == 'Family') indices = dG1a$PlaceType=='HOME OF FAMILY MEMBER'
        
        if(nrow(dG1)>0){
          
          #counts
          t[1,str_c('GPS_Num_', ALocation, '_',APeriod)] = sum(indices, na.rm=TRUE)
          if(nrow(dG1) - sum(is.na(dG1$PlaceType))>0){
            t[1,str_c('GPS_NP1_', ALocation, '_',APeriod)] = sum(indices, na.rm=TRUE)/(nrow(dG1) - sum(is.na(dG1$PlaceType)))
          }else
          {
            t[1,str_c('GPS_NP1_', ALocation, '_',APeriod)] =NA
          }
          t[1,str_c('GPS_NP2_', ALocation, '_',APeriod)]  = sum(indices, na.rm=TRUE)/nrow(dG1)
          
          #time at locations
          t[1,str_c('GPS_Time1_', ALocation, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE) 
          if(sum(subset(dG1$NextTime,!is.na(dG1$PlaceType)), na.rm=TRUE)>0){
            t[1,str_c('GPS_T1P1_', ALocation, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE)/ sum(subset(dG1$NextTime,!is.na(dG1$PlaceType)), na.rm=TRUE) 
          }else{
            t[1,str_c('GPS_T1P1_', ALocation, '_',APeriod)] = NA
          }
          t[1,str_c('GPS_T1P2_', ALocation, '_',APeriod)] = sum(subset(dG1$NextTime,indices), na.rm=TRUE) / sum(dG1$NextTime, na.rm=TRUE) 
          
        }else{
          t[1,str_c('GPS_Num_', ALocation, '_',APeriod)] = NA
          t[1,str_c('GPS_NP1_', ALocation, '_',APeriod)] = NA
          t[1,str_c('GPS_NP2_', ALocation, '_',APeriod)] = NA
          
          t[1,str_c('GPS_Time1_', ALocation, '_',APeriod)] = NA
          t[1,str_c('GPS_T1P1_', ALocation, '_',APeriod)] = NA
          t[1,str_c('GPS_T1P2_', ALocation, '_',APeriod)] = NA
        }
      }
    }
    fG1 = data.frame(fG1,t,stringsAsFactors=FALSE) #merge in these features into fG1 
    
    
    #Feature extraction done for fG1  Now add to fG
    
    
    #Merge all GPS features for 1 Y into full feature dataframe fG
    if(is.null(fG)){ #if merging features for first Y
      fG = fG1
    }else{
      fG = dfMerge(fG,fG1,AddVars = FALSE)
    }
  }
  
  #fG created for all Y.  Now clean up and add to X
  
  #sort by SubID and UTC
  fG = fG[order(fG$SubID,fG$UTC),]
  
  #merge fG into X
  if(is.null(X)){
    X = fG  #X was null so set to fG
  }else{
    #check that fG and X match
    if(nrow(X)!=nrow(fG)) stop('X and fG do not have same number of rows')
    if(any(fG$SubID !=X$SubID) | any(fG$UTC !=X$UTC)) stop('X and fG have mismatched SubID and/or UTC')
    
    #Remove SubID and UTC from fG b/c already in X
    fG$SubID = NULL
    fG$UTC = NULL
    X = data.frame(X,fG, stringsAsFactors=FALSE)
  } 
  
  return(X)
}


