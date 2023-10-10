#Required packages -------------------------------------------------------
#install.packages('P:/Methods/Software/R/StudySupport/Source/StudySupport.tar.gz', repos=NULL, type= 'source')
library(StudySupport)
library(lmSupport)
library(tidyverse)

library(readxl)  #read_excel
library(writexl) #write_xlsx
library(lubridate)
library(stringr)
library(xml2)   #xml_children, xml_attr
library(RSQLite)
library(janitor)
source(file.path("P:/StudyData/RISK/analysis/shared/scripts_make/fun_gps.R"))


#add_subject_to_database-------------------------------------------------------
add_subject_to_database = function(nSubID, DownloadSurveys, AddAggregateData)
{
  #General setup----------------------------------------------
  #modify for debugging as needed
  IncludeVoiceCalls = TRUE
  IncludeSMS = TRUE
  IncludeGPS = TRUE
  IncludeInterview = TRUE #Risk times, contacts, risk dates
  
  #Libraries, paths,downloads
  path_raw <-  "raw_data"
  path_qualtrics <- "raw_data/qualtrics"
  path_database <- "database"
  
  sSubID = varPadString(nSubID,3)  #Will use 3 character version of ASubID below
  s_subid <- sSubID #for compatability with tidied functions
  
  #set start and end dates for period
  dDates = read_excel(file.path(path_raw, sSubID, str_c(sSubID, '_VisitDates.xlsx')), na= c('NA', ''))
  StartDate = dDates$StartStudy
  EndDate =   dDates$EndStudy
  
  #covert from dttm to date
  StartDate = as_date(StartDate)  
  EndDate =   as_date(EndDate)
  rm(dDates)
  
  #download updated qualtrics files if needed
  if(DownloadSurveys){
    message('Downloading Qualtrics Surveys')
    get_qualtrics_surveys(in_path)
  }
  
  #Begin Aggregate Data Files--------------------------------------------------
  #test if requested and if set to include above
  if(AddAggregateData){
    message('Processing Aggregate Data Files')
    #EMA morning ---------------------------------------------------------------
    message('...Processing Morning EMA')
    
    #Open and tidy file (e.g., rename variables)
    dE1 = read_csv(file.path(path_qualtrics, 'ema_morning.csv'))
    
    #Remove unused variables
    dE1 = select(dE1, -c(ResponseSet, IPAddress, RecipientLastName, RecipientFirstName,
                         RecipientEmail, ExternalDataReference, Status, SendDate, SendTime, LocationLatitude,
                         LocationLongitude, LocationAccuracy))    
    
    #Retain as finished all unfinished records if they report a lapse with specific date/time
    dE1 = mutate(dE1, Finished = replace(Finished, Finished == 0 & !is.na(EMAM_1.1), 1))
    
    #retained and based on participant report
    dE1$Finished[dE1$SubID=='081' & dE1$UTC == 1534680283 ] = 1
    dE1$EMAM_1.1[dE1$SubID=='081' & dE1$UTC == 1534680283 ] = '08-18-2018'
    dE1$EMAM_1.2[dE1$SubID=='081' & dE1$UTC == 1534680283 ] = 19
    dE1$EMAM_1.3[dE1$SubID=='081' & dE1$UTC == 1534680283 ] = '08-18-2018'
    dE1$EMAM_1.4[dE1$SubID=='081' & dE1$UTC == 1534680283 ] = 19
    
    #Remove other unfinished records
    dE1 = filter(dE1, Finished == 1)
    dE1 = select(dE1, -Finished)
    
    dE1 = varRename(dE1, 
                    c("EMAM_1", "EMAM_1.1", "EMAM_1.2", "EMAM_1.3", "EMAM_1.4", "EMAM_1.5", "EMAM_2_1", "EMAM_3_1", "EMAM_4_1", "EMAM_5_1", "EMAM_6_1", "EMAM_7_1", "EMAM_8_1", "EMAM_9_1", "EMAM_10_1"),
                    c("EMA_1", "EMA_1.1", "EMA_1.2", "EMA_1.3", "EMA_1.4", "EMA_1.5", "EMA_2", "EMA_3", "EMA_4", "EMA_5", "EMA_6", "EMA_7", "EMA_8", "EMA_9", "EMA_10"))
    
    #Update UTC with StartDate in unix time if missing or 0 and retain EndDate in unix time
    dE1$UTC[is.na(dE1$UTC)] = as.numeric(as_datetime(dE1$StartDate[is.na(dE1$UTC)], tz='GMT'))
    dE1$UTC[dE1$UTC==0] = as.numeric(as_datetime(dE1$StartDate[dE1$UTC==0], tz='GMT'))
    #dE1 = select(dE1, -StartDate)
    dE1$EndDate = as_datetime(dE1$EndDate, tz='GMT')
    
    #correcting test records that should have been real SubIDs
    dE1$SubID[toupper(dE1$SubID)=='TEST' & dE1$UTC==1512392452] = '031'
    dE1$SubID[toupper(dE1$SubID)=='TEST' & dE1$UTC==1512227465] = '031'
    dE1$SubID[toupper(dE1$SubID)=='TEST' & dE1$UTC==1512047106] = '031'
    dE1$SubID[toupper(dE1$SubID)=='TEST' & dE1$UTC==1511960486] = '031'
    
    #correction other record that has problematic subid
    dE1$SubID[dE1$SubID=='066?SSID=051418134217'] = '066'
    
    #Delete remaining test records
    dE1 = filter(dE1, toupper(SubID) != 'TEST')
    
    # #Remove EMA records with long delay between start and end
    # TD = (dE1$EndDate-dE1$UTC)/60
    # if(any(dE1$EMA_1[TD>15]==2)) warning('EMA lapse reported on record with TD >15 mins')
    # dE1= subset(dE1,TD<=15)
    # rm(TD)
    
    #Format SubID to fixed length with leading 0s
    dE1$SubID = varPadString(dE1$SubID,3,'0')
    
    #fix odd Endate encoding problem
    dE1$EndDate[dE1$SubID == '042' & dE1$UTC == 1525173953] = as_datetime(1525174001)
    dE1$EndDate[dE1$SubID == '085' & dE1$UTC == 1533499971] = as_datetime(1533500001)

    #Reorder Variables for UTC and SubID first
    dE1 = select(dE1, UTC, SubID, everything())
    
    #write data
    write_rds(dE1, file.path(path_database, 'ema_morning.rds'))
    rm(dE1)
    
    #EMA Later ---------------------------------------------------------------
    message('...Processing Later EMA')
    
    #Open and tidy file (remove second header, rename variables)
    dE2 = read_csv(file.path(path_qualtrics, 'ema_later.csv'))
   
    #Remove unused variables 
    dE2 = select(dE2, -c(ResponseSet, IPAddress, RecipientLastName, RecipientFirstName,
                         RecipientEmail, ExternalDataReference, Status, SendDate, SendTime, LocationLatitude,
                         LocationLongitude, LocationAccuracy))


    
    #Retain as finished all unfinished records if they report a lapse with specific date/time
    dE2 = mutate(dE2, Finished = replace(Finished, Finished == 0 & !is.na(EMAL_1.1), 1))
    
    #Remove remaining unfinished records
    dE2 = filter(dE2, Finished==1)
    dE2 = select(dE2, -Finished)
    
    dE2 = varRename(dE2, 
                    c("EMAL_1", "EMAL_1.1", "EMAL_1.2", "EMAL_1.3", "EMAL_1.4", "EMAL_1.5", "EMAL_2_1", "EMAL_3_1", "EMAL_4_1", "EMAL_5_1", "EMAL_6_1", "EMAL_7_1"),
                    c("EMA_1", "EMA_1.1", "EMA_1.2", "EMA_1.3", "EMA_1.4", "EMA_1.5", "EMA_2", "EMA_3", "EMA_4", "EMA_5", "EMA_6", "EMA_7"))
    
    #Update UTC with StartDate in unix time if missing or 0 and retain EndDate in unix time
    dE2$UTC[is.na(dE2$UTC)] = as.numeric(as_datetime(dE2$StartDate[is.na(dE2$UTC)], tz='GMT'))
    dE2$UTC[dE2$UTC==0] = as.numeric(as_datetime(dE2$StartDate[dE2$UTC==0], tz='GMT'))
    #dE2 = select(dE2, -StartDate)
    dE2$EndDate = as_datetime(dE2$EndDate, tz='GMT')
  
    # #Remove EMA records with long delay between start and end
    # TD = (dE2$EndDate-dE2$UTC)/60
    # if(any(dE2$EMA_1[TD>15]==2)) warning('EMA lapse reported on record with TD >15 mins')
    # dE2= subset(dE2,TD<=15)
    # rm(TD)
    
    #convert test records to subID for errors
    dE2$SubID[toupper(dE2$SubID)=='TEST' & dE2$UTC==1512407036] = '031'
    dE2$SubID[toupper(dE2$SubID)=='TEST' & dE2$UTC==1512335063] = '031'
    dE2$SubID[toupper(dE2$SubID)=='TEST' & dE2$UTC==1512326855] = '031'
    dE2$SubID[toupper(dE2$SubID)=='TEST' & dE2$UTC==1512178633] = '031'
    dE2$SubID[toupper(dE2$SubID)=='TEST' & dE2$UTC==1512169637] = '031'
    dE2$SubID[toupper(dE2$SubID)=='TEST' & dE2$UTC==1511984753] = '031'
    
    #Delete testing records
    dE2 = filter(dE2, toupper(dE2$SubID) != 'TEST')
    
    
    #correction other record that has problematic subid
    dE2$SubID[dE2$SubID=='136?SSID=091918134537'] = '136'
    
    #Format SubID to fixed length with leading 0s
    dE2$SubID = varPadString(dE2$SubID,3,'0')
    
    #Reorder Variables for UTC and SubID first
    dE2 = select(dE2, UTC, SubID, everything())
    
    #Write data
    write_rds(dE2, file.path(path_database, 'ema_later.rds'))
    rm(dE2)
    
    #Screening Survey  ---------------------------------------------------------------
    message('...Processing Screening Survey')
    
    #Open and tidy file (remove second header, rename variables)
    dS = read_csv(file.path(path_qualtrics, 'screen.csv'))
    
    #Remove unused variables
    dS = select(dS, -c(ResponseID, ResponseSet, IPAddress, StartDate, EndDate, RecipientLastName, RecipientFirstName,
                         RecipientEmail, ExternalDataReference, Finished, Status, RA, LocationLatitude,
                         LocationLongitude, LocationAccuracy))    
    
    dS = select(dS, -c(DSM5_score, paranoia, psychosis, Psychosis_Final, Paranoia_Final, StudyName, DEM_Inst, DEM2_Inst,
                       AUH_Inst, DSM5_Inst, DSM5_Inst2, YAP_Inst, ASSIST_Inst, SCL90_Inst, IUS_Inst, ASI_Inst, DTS_Inst, 
                       FAD_Inst, MPS_Inst, SB_Inst, SB_Inst2, EB_1, EB_2, EB_3, EB_4, Q140))
    
    #fix data type issues
    dS$DataType[dS$SubID==173] = 1
    
    #Select only Real data
    dS = filter(dS,DataType ==1)
    dS$DataType = NULL
    
    #Variable renaming
    dS = varRename(dS, 
                   c('AUH_8.1_AUH_8_Month',	'AUH_8.2_AUH_8_Day',	'AUH_8.3_AUH_8_Year', "AUH_7.1_1_TEXT",   "AUH_7.1_2_TEXT",   "AUH_7.1_3_TEXT",   "AUH_7.1_4_TEXT"),
                   c('AUH_8_Month',	'AUH_8_Day',	'AUH_8_Year', "AUH_7.1_1_Med1",   "AUH_7.1_2_Med2",   "AUH_7.1_3_Med3",   "AUH_7.1_4_Med4"))
    
    #Format SubID to fixed length with leading 0s
    dS$SubID = varPadString(dS$SubID,3,'0')
    
    #Fill in 0 (not checked) for NA on check all that apply in AUH
    AUHVars = c(str_c('AUH_6_', 1:7))
    for (VarName in AUHVars){
      dS[is.na(dS[VarName]),VarName] = 0	
    }
    #Fill in 1 (NEVER) for NA on WHO ASSIST items for recent periods (because skipped if not used ever)
    ASSISTVars = str_c('ASSIST_',rep(2:7, each=8), str_c('_',rep(1:8,6)))
    for (VarName in ASSISTVars){
      dS[is.na(dS[VarName]),VarName] = 1	
    }
    
    # #Fix issues with Income (DEM_7) reporting
    # dS$DEM_7 = str_replace_all(dS$DEM_7,',','')
    # dS$DEM_7 = as.numeric(dS$DEM_7)
    
    #Reorder Variables for UTC and SubID first
    dS = select(dS, UTC, SubID, everything())
    
    #Write data
    write_rds(dS, file.path(path_database, 'screen.rds'))
    rm(dS)
    
    #Intake  Survey---------------------------------------------------------------
    message('...Processing Intake Survey')
    
    #Open and tidy file (remove second header, rename variables)
    dI = read_csv(file.path(path_qualtrics, 'intake.csv'))
    
    #Remove unused variables
    dI$ResponseID = NULL
    dI$ResponseSet = NULL
    dI$IPAddress = NULL
    dI$StartDate = NULL
    dI$EndDate = NULL
    dI$RecipientLastName = NULL
    dI$RecipientFirstName = NULL
    dI$RecipientEmail = NULL
    dI$ExternalDataReference = NULL
    dI$Finished = NULL
    dI$Status = NULL
    dI$SendDate = NULL
    dI$SendTime = NULL
    dI$LocationLatitude = NULL
    dI$LocationLongitude = NULL
    dI$LocationAccuracy = NULL
    
    dI$StudyName = NULL	
    dI$RA= NULL
    dI$PACS_Inst	= NULL
    dI$AASE_Inst = NULL	
    dI$MAM_Inst = NULL
    dI$DASS21_Inst = NULL
    dI$PSS_Inst = NULL
    dI$QOL_Inst = NULL
    dI$DAS_Inst = NULL
    dI$MSPSS_Inst = NULL
    
    #Select only Real data
    dI = filter(dI, DataType == 1) %>% 
      select(-DataType)
    
    #Format SubID to fixed length with leading 0s
    dI$SubID = varPadString(dI$SubID,3,'0')
    
    #Fill in zeros for check all that apply
    #MAM1.3_#
    CheckAllVars = c(str_c('MAM_1.3_', 1:5))
    for (VarName in CheckAllVars){
      dI[is.na(dI[VarName]),VarName] = 0	
    }
    
    #Reorder Variables for UTC and SubID first
    dI = select(dI, UTC, SubID, everything())
    
    #Write data
    write_rds(dI, file.path(path_database, 'intake.rds'))
    rm(dI)
    
    #Follow-up12 Survey ---------------------------------------------------------------
    message('...Processing Follow-up12 Survey')
    
    #Open and tidy fil e (remove second header, rename variables)
    dF12 = read_csv(file.path(path_qualtrics, 'followup12.csv'))
    
    #Remove unused variables
    dF12$ResponseID = NULL
    dF12$ResponseSet = NULL
    dF12$IPAddress = NULL
    dF12$StartDate = NULL
    dF12$EndDate = NULL
    dF12$RecipientLastName = NULL
    dF12$RecipientFirstName = NULL
    dF12$RecipientEmail = NULL
    dF12$ExternalDataReference = NULL
    dF12$Finished = NULL
    dF12$Status = NULL
    dF12$SendDate = NULL
    dF12$SendTime = NULL
    dF12$LocationLatitude = NULL
    dF12$LocationLongitude = NULL
    dF12$LocationAccuracy = NULL
    
    dF12$StudyName = NULL	
    dF12$RA= NULL
    
    dF12$PACS_Inst	= NULL
    dF12$AASE_Inst = NULL	
    dF12$MAM_Inst = NULL
    dF12$DASS21_Inst = NULL
    dF12$PSS_Inst = NULL
    dF12$QOL_Inst = NULL
    dF12$DAS_Inst = NULL
    dF12$MSPSS_Inst = NULL
    dF12$ASSIST_Inst = NULL
    dF12$RBM_Inst = NULL
    
    #Select only Real data
    dF12 = filter(dF12, DataType ==1)
    dF12$DataType = NULL
    
    #Format SubID to fixed length with leading 0s
    dF12$SubID = varPadString(dF12$SubID,3,'0')
    
    #Fill in zeros for check all that apply
    #MAM1.3_#
    CheckAllVars = c(str_c('MAM_1.3_', 1:5))
    for (VarName in CheckAllVars){
      dF12[is.na(dF12[VarName]),VarName] = 0	
    }

    #clean MAM_13 issue
    dF12$MAM_13[dF12$MAM_13==',0'] = '0'
    dF12$MAM_13 = as.integer(dF12$MAM_13)
    
    # #reclass MAM_15
    # dF12$MAM_15[dF12$MAM_15=='None'] = '0'
    # dF12$MAM_15[dF12$MAM_15=='O'] = '0'
    # dF12$MAM_15 = as.integer(dF12$MAM_15)
    
    
    #Reorder Variables for UTC and SubID first
    FirstVars = c("UTC", "SubID")
    dF12 = dF12[, c(FirstVars, setdiff(names(dF12), FirstVars))]
    
    #Write data
    write_rds(dF12, file.path(path_database, 'followup12.rds'))
    rm(dF12)
    
    
    #Burden Survey ------------------------------------------------
    message('...Processing Burden Survey')
    
    # Open ID battery F1&2 and F3, join and extract relevant columns
    burden <- read_csv(file.path(path_qualtrics,"followup12.csv"), 
                       col_types =cols(.default = col_character())) %>%
                full_join(read_csv(file.path(path_qualtrics,"followup3.csv"), 
                                   col_types = cols(.default = col_character()))) %>% 
      select(UTC,SubID, contains("RBM_"), -RBM_Inst) %>% 
      mutate(SubID = if_else(SubID == "RISK", "081", SubID),  #has to happen here rather then cln_ to allow varPadString()
             SubID = varPadString(SubID,3,'0'))
    
    # Open Beddit burden questions F1&2 & F3, join and extract relevant columns
    beddit <- read_csv(file.path(path_qualtrics,"beddit_burden_survey_F12.csv"), 
                          col_types =cols(.default = col_character())) %>%
      full_join(read_csv(file.path(path_qualtrics,"beddit_burden_survey_F3.csv"), 
                         col_types =cols(.default = col_character()))) %>%
      select(UTC,SubID, contains("RBM_"), -RBM_Inst)%>%
      mutate(SubID = if_else(SubID == "RISK", "081", SubID),  #has to happen here rather then cln_ to allow varPadString()
             SubID = varPadString(SubID,3,'0'))
    
    # Open Empatica burden questions F1&2 and F3, join and extract relevant columns
    empatica <- read_csv(file.path(path_qualtrics,"empatica_burden_survey_F12.csv"), 
                            col_types =cols(.default = col_character())) %>%
      full_join(read_csv(file.path(path_qualtrics,"empatica_burden_survey_F3.csv"), 
                         col_types =cols(.default = col_character()))) %>%
      select(UTC,SubID, contains("RBM_"), -RBM_Inst) %>%
      mutate(SubID = if_else(SubID == "RISK", "081", SubID),  #has to happen here rather then cln_ to allow varPadString()
             SubID = varPadString(SubID,3,'0'))
  
      
    # Create merged dataframe in correct column order. Remove individual dataframes
    all_burden <- full_join(empatica, beddit) %>%
      full_join(burden) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>% 
      write_rds(file.path(path_database, "burden.rds"))
    
    rm(burden, beddit, empatica, all_burden)
    
    #Sleep Schedule Qualtrics Form-------------------------------------------------------
    message('...Processing Sleep Schedule Survey')
    
    #Open and tidy file (remove second header, rename variables)
    dSS = read_csv(file.path(path_qualtrics, 'sleep_schedule.csv'), 
                   col_types =cols(.default = col_character()))
    
    #Remove unused variables
    dSS$ResponseID = NULL
    dSS$ResponseSet = NULL
    dSS$IPAddress = NULL
    dSS$StartDate = NULL
    dSS$EndDate = NULL
    dSS$RecipientLastName = NULL
    dSS$RecipientFirstName = NULL
    dSS$RecipientEmail = NULL
    dSS$ExternalDataReference = NULL
    dSS$Finished = NULL
    dSS$Status = NULL
    dSS$SendDate = NULL
    dSS$SendTime = NULL
    dSS$LocationLatitude = NULL
    dSS$LocationLongitude = NULL
    dSS$LocationAccuracy = NULL
    
    dSS$UTC=NULL     #Not used for UTC.  Instead will use ScheduleStart
    dSS$ScheduleEnd	= NULL
    dSS$Phone= NULL
    dSS$UTC_old = NULL
    
    #Format SubID to fixed length with leading 0s
    dSS$SubID = varPadString(dSS$SubID,3,'0')
    
    #Use schedule start at the UTC for when these days/times start to apply
    dSS$UTC=as.numeric(as_datetime(dSS$ScheduleStart, format='%m/%d/%Y', tz='America/Chicago'))
    dSS$ScheduleStart= NULL
    
    # #Fix old format for subjects 1-3
    # dSS$MoWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
    # dSS$MoBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
    # dSS$TuWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
    # dSS$TuBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
    # dSS$WeWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
    # dSS$WeBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
    # dSS$ThWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
    # dSS$ThBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
    # dSS$FrWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
    # dSS$FrBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
    # dSS$SaWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
    # dSS$SaBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
    # dSS$SuWake[dSS$SubID=='001' & dSS$UTC==1488434400 ] = '6:00'
    # dSS$SuBed[dSS$SubID=='001'  & dSS$UTC==1488434400 ] = '20:00'
    # 
    # 
    # dSS$MoWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '5:30'
    # dSS$MoBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
    # dSS$TuWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '5:30'
    # dSS$TuBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
    # dSS$WeWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '5:30'
    # dSS$WeBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
    # dSS$ThWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '5:30'
    # dSS$ThBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
    # dSS$FrWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '5:30'
    # dSS$FrBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
    # dSS$SaWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '7:30'
    # dSS$SaBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '22:00'
    # dSS$SuWake[dSS$SubID=='002' & dSS$UTC==1490245200 ] = '7:00'
    # dSS$SuBed[dSS$SubID=='002'  & dSS$UTC==1490245200 ] = '20:30'
    # 
    # dSS$MoWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
    # dSS$MoBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
    # dSS$TuWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
    # dSS$TuBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
    # dSS$WeWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
    # dSS$WeBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
    # dSS$ThWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
    # dSS$ThBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
    # dSS$FrWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
    # dSS$FrBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
    # dSS$SaWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
    # dSS$SaBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
    # dSS$SuWake[dSS$SubID=='003' & dSS$UTC==1490158800 ] = '8:00'
    # dSS$SuBed[dSS$SubID=='003'  & dSS$UTC==1490158800 ] = '22:00'
    
    #Reorder Variables for UTC and SubID first
    FirstVars = c("UTC", "SubID")
    dSS = dSS[, c(FirstVars, setdiff(names(dSS), FirstVars))]
    
    #Write data
    write_rds(dSS, file.path(path_database, 'sleep_schedule.rds'))
    rm(dSS)
    
    #Audio Transcript Excel File -----------------------------------------------
    addAudio(path_raw,path_database)
  }
  
  message(str_c('Processing SubID: ', sSubID))  #output SubID
  dir.create(file.path(path_database,sSubID), showWarnings=FALSE) #create subject SubID for output
      
  #GPS -------------------------------------------------------------------------
  if(IncludeGPS){
    message('...Processing GPS')
    
    #JJJ, commented out after data collection b/c no more donwloads
    #get updated FollowMee trackpoints from API if participant active
    #device_id <- api_followmee_device_id(nSubID)
    #if(length(device_id)> 0) get_followmee(nSubID, device_id, path_raw
    
    
    #HM, commented out - for regenerating database data  
    # subid_all <- list.dirs("./raw_data", recursive = FALSE) %>%
    #  str_extract("\\d\\d\\d") %>%
    #  .[!is.na(.)]
    # path_raw <- "raw_data"
    # path_database <- "database"
    
    for (i in 1:length(subid_all)){
      
      subid <- subid_all[i]
      
      # list gpx files
      gpx_files <- list.files(
        path = file.path(path_raw, subid), 
        pattern = '.gpx', 
        full.names = TRUE, 
        recursive = FALSE, 
        include.dirs = FALSE
      )
      
      # list followmee files
      # for testing: subject 82 has both followmee and moves, with multiple .gpx
      followmee_files <- list.files(
        path = file.path(path_raw, subid),
        pattern = '\\d\\d\\d_GPSFollowRaw.rds',
        full.names = TRUE,
        recursive = FALSE,
        include.dirs = FALSE
      )
  
      #store type of gps data source
      only_gpx <- ifelse(length(gpx_files) >= 1 & 
                           length(followmee_files) == 0, 
                         TRUE, FALSE)
      
      only_followmee <- ifelse(length(gpx_files) == 0 & 
                                 length(followmee_files) == 1, 
                               TRUE, FALSE)
      
      both <- ifelse(length(gpx_files) >= 1 & 
                       length(followmee_files) == 1, 
                     TRUE, FALSE)
      
      gps_present <- xor(xor(only_gpx, only_followmee), both)
      
      #check that source is moves, followmee, or both, if not, stopping
      
      if (gps_present == FALSE) stop("GPS data missing or in unexpected form")
      
      d_moves <- NULL
      d_follow <- NULL
      
      #process and save gpx (accomodates multiple)
      if (length(gpx_files) != 0){
        d_moves <- map_df(gpx_files, ~convert_moves(.x)) %>% #import, convert, bind each file
          filter(!is.na(long), !is.na(lat)) %>% #drop obs without lat and long
          distinct() #eliminate fully duplicated rows
      }
      
      #process and save followmee file
      if (length(followmee_files) == 1){
        d_follow <- convert_followmee(followmee_files[1])
      }
      
      subid_numeric <- as.numeric(subid)
      
      #import study start / stop and remove obs outside of study window
      start_end <- read_rds("./analysis/shared/data/ds_visits.rds") %>% 
        select(subid, start_study, end_study) %>% 
        filter(subid == subid_numeric) %>% 
        mutate(
          start_dttm = make_datetime(
            year = year(start_study), 
            month = month(start_study), 
            day = day(start_study), 
            hour = 0, 
            min = 0, 
            sec = 0, 
            tz = 'America/Chicago'),
          end_dttm = make_datetime(
            year = year(end_study), 
            month = month(end_study), 
            day = day(end_study), 
            hour = 23, 
            min = 59, 
            sec = 59, 
            tz = 'America/Chicago'),
        ) %>% 
        filter(!is.na(start_dttm)) %>% 
        filter(!is.na(end_dttm)) %>% 
        select(subid, start_dttm, end_dttm) 
      
      if (nrow(start_end) != 1) stop("Study start and end dates in unexpected form")
      
      message(paste("......Creating database for subject", subid))
      
      #create database rds
      database <- bind_rows(d_moves, d_follow) %>% #accomodates null tibbles
        mutate(start_dttm = start_end$start_dttm,
               end_dttm = start_end$end_dttm) %>% #merge start and end date
        filter(time > start_dttm, #drop obs before start and after end
               time < end_dttm)
      
      study_length <- interval(date(start_end$start_dttm), date(start_end$end_dttm))/days(1)
      
      study_dates <- as_tibble(date(start_end$start_dttm) + days(0:study_length)) %>% 
        rename(date = value)
      
      #store all missing dates (dates with no obs from midnight to midnight)
      all_missing_dates <- database %>%
        arrange(time) %>% 
        mutate(date = date(time))%>%
        group_by(date) %>% 
        summarise(n = n()) %>% 
        full_join(study_dates, by = "date") %>% 
        arrange(date) %>% 
        filter(is.na(n)) %>% 
        select(-n)
      
      message(paste("......Subject", subid, "has", nrow(all_missing_dates), "total missing dates"))
      
      #store missing dates with timestamp
      missing_dates <- database %>% 
        mutate(date = date(time)) %>% 
        full_join(all_missing_dates, by = "date") %>% 
        arrange(date) %>% 
        filter(is.na(time)) %>% 
        mutate(time = ymd(ceiling_date(ymd(date), "day"), tz = 'America/Chicago') - seconds(1)) %>% #impute last sec of day
        select(date, time)

      if (nrow(missing_dates) > 0){ #impute missing days
        database <- database %>% 
          mutate(date = date(time)) %>% 
          full_join(missing_dates, by = c("date", "time")) %>%
          select(-date)
      }
      
      database %>% 
        arrange(time) %>% 
        select(-start_dttm, -end_dttm, -count) %>% 
        select(time, everything()) %>% 
        write_rds(file.path(path_database, subid, str_c(subid,'_gps.rds')))
    }
  
  
  #SMS ------------------------------------------------------------
  if (IncludeSMS){
    message('...Processing SMS')
    addSMS(nSubID, path_raw,path_database,StartDate, EndDate)
  }

  #Voice Call---------------------------------------------------------------------
  if (IncludeVoiceCalls){
    message('...Processing Voice Calls')
    VoiceFiles = list.files(file.path(path_raw, sSubID), pattern = 'Voice', include.dirs = FALSE)
    VoiceFiles = VoiceFiles[VoiceFiles !='RawSMSVoice']  #remove RawSMSVoice
    
    if(length(VoiceFiles)>0){
      for (VoiceFile in VoiceFiles){
        message(str_c('......', VoiceFile))
        
        #Check if carbonite or iTunes file
        if(str_detect(VoiceFile,'.xml')){  #IF ANDROID
          
          #lD = xml_children(read_xml(file.path(path_raw, sSubID, VoiceFile)))
          #switch from read_xml (above) to fix problems with emojiis in voice contact names
          lD = xml_children(read_html(file.path(path_raw, sSubID, VoiceFile)))
 
          #fix for problem with nested list after switching to read_html()
          while (length(lD)==1) {
            lD = xml_children(lD) #loop to find lower level of xml children - SS
          }        
                   
          #trawl the nodes extracting each attribute value
          dD = data.frame(
            number = xml_attr(lD,'number'),
            duration = xml_attr(lD,'duration'),
            date = xml_attr(lD,'date'),
            type = xml_attr(lD,'type'),
            #presentation = xml_attr(lD,'presentation'),
            #readable_date = xml_attr(lD,'readable_date'),
            #contact_name = xml_attr(lD,'contact_name'),
            stringsAsFactors = FALSE
          )
          
          dVT = data.frame(UTC=rep(NA,nrow(dD)), Phone= rep(NA,nrow(dD)), Duration=rep(NA,nrow(dD)), Type=rep(NA,nrow(dD)))
          
          #10 digit or 13 digit unix time stamps
          dD$date = as.numeric(dD$date)
          dVT$UTC = ifelse(trunc(log10(dD$date))+1==10, dD$date, dD$date/1000)
          
          dVT$Phone = as.numeric(dD$number)
          dVT$Type = as.numeric(dD$type)   #WHAT ARE THE VALID VALUES AND THEIR DEFINITIONS?
          dVT$Duration = as.numeric(dD$duration)
          
          rm(lD)
          rm(dD)
          gc()
          }
        else if(str_detect(VoiceFile,'.sql')){#ELSE pre-IOS13
          
          cV = dbConnect(RSQLite::SQLite(), file.path(path_raw, sSubID, VoiceFile))
          
          dD= dbGetQuery( cV,'select * from ZCALLRECORD' )
          
          #Need to update ZAddress as text rather than odd raw format
          dT= dbGetQuery( cV,'select cast(ZADDRESS as text) from ZCALLRECORD' )
          dD$ZADDRESS = dT[,1]
          
          #close the db connection
          dbDisconnect(cV)
          
          dVT = data.frame(UTC=rep(NA,nrow(dD)), Phone= rep(NA,nrow(dD)), Duration=rep(NA,nrow(dD)), Type=rep(NA,nrow(dD)))
          
          #apple has NOT updated this to include 18 digit version??
          dVT$UTC = dD$ZDATE + 978307200  #convert from IOS time sample (s since 1/1/2001) to unix time stamp (s since 1/1/1970)
          dVT$Phone = as.numeric(dD$ZADDRESS)
          dVT$Duration = dD$ZDURATION        
          dVT$Type = dD$ZORIGINATED+1  #1=incoming, 2=outgoing (For now only recording these two types for IOS; see study protocol notes)
        }
        else if(str_detect(VoiceFile,'.csv')){#ELSE IOS13+
          
          dD = read_csv(file.path(path_raw, sSubID, VoiceFile))
          
          #recode incoming/outgoing
          dD = dD %>% mutate_at(vars(`Call type`), ~recode(., 'Incoming' = 1, 'Outgoing' = 2))
          
          dVT = data.frame(UTC=rep(NA,nrow(dD)), Phone= rep(NA,nrow(dD)), Duration=rep(NA,nrow(dD)), Type=rep(NA,nrow(dD)))
          
          dVT$UTC = as.numeric(as_datetime(dD$Date, tz='GMT'))
          dVT$Phone = as.numeric(dD$Number)
          dVT$Duration = dD$Duration        
          dVT$Type = dD$`Call type`  #1=incoming, 2=outgoing (For now only recording these two types for IOS; see study protocol notes)
        }
        
        #Merge
        if(!exists('dV')){
          dV = dVT  #First voice file
        }else{
          dV = bind_rows(dV, dVT) #merge in new file
        }
        rm(dVT)
      }
    }
    else{
      message('......WARNING MESSAGE: Voice file(s) do not exist')
    }
    
    if (exists('dV') && nrow(dV)>0){  #only do more if there are voice calls for this subject
      #Remove duplicates after merges
      dV = subset(dV, !duplicated(dV))
      
      #remove non-numeric characters in phone numbers
      dV$Phone = str_replace_all(dV$Phone, '\\+', '')
      dV$Phone = str_replace_all(dV$Phone, '\\*', '')
      dV$Phone = str_replace_all(dV$Phone, '\\(', '')
      dV$Phone = str_replace_all(dV$Phone, '\\)', '')
      dV$Phone = str_replace_all(dV$Phone, '\\-', '')
      dV$Phone = str_replace_all(dV$Phone, '\\s', '')
      dV$Phone = str_replace_all(dV$Phone, '\\.', '')
      
      dV$Phone = as.numeric(dV$Phone)
      
      #Remove NA phone numbers
      dV = subset(dV, !is.na(Phone))
      
      #Delete numbers <=0 and 611
      dV = subset(dV, Phone>0)
      dV = subset(dV, !(Phone==611))
      
      #Add 608 to 7 digit numbers and remove US country code (1)
      for(i in 1:nrow(dV)){
        if(trunc(log10(dV$Phone[i]))+1==7) dV$Phone[i] = dV$Phone[i] + 6080000000    
        if(varParse(dV$Phone[i],10000000000,10000000000)==1) dV$Phone[i] = dV$Phone[i] %% 10000000000
      }        
      
      #Delete short and long wrong numbers
      i=1
      while(i<=nrow(dV)){
        if (floor(log10(dV$Phone[i]))+1 != 10){
          dV = dV[-i,]
        }
        else{
          i=i+1
        }
      }      
      
      #Reorder Variables for UTC and SubID first
      FirstVars = c("UTC")
      dV = dV[, c(FirstVars, setdiff(names(dV), FirstVars))]
      
      #remove SMS outside study period
      dV$TempDate = as_date(as_datetime(dV$UTC, tz='America/Chicago'))
      dV = filter(dV,TempDate>=StartDate & TempDate<=EndDate)
      dV$TempDate = NULL
      
      #Add Voice_ to all variables but UTC
      #dV = addVarNamePrefix(dV,'Voice_',2)
      
      #add MCF prefixes
      #dV = addMCF(dV,2)
    }
    
    # if(exists('dV') && nrow(dV)==0){ #update empty file variable names to indicate the there were Voice files downloaded
    #   dV = addVarNamePrefix(dV,'Voice_',2)
    # }
    
    if(exists('dV')){
      FileName = str_c(sSubID,'_Voice.rds')
      write_rds(dV, file.path(path_database, sSubID, FileName))
      rm(dV)
    }
  }
  
  
  #Sleep/Beddit Data (PENDING) ------------------------------
  #print('   Processing Sleep/Beddit')
      
    
  
  
  #Interview files -------------------------------
  if (IncludeInterview){
    
    # Visit Dates ------------------------------------
    message('...Processing Visit Dates Report')
    if(file.exists(file.path(path_raw, sSubID, str_c(sSubID,'_VisitDates.xlsx')))){
      #Open file
      dV = read_excel(file.path(path_raw, sSubID, str_c(sSubID,'_VisitDates.xlsx')), na= c('NA', ''))
      
      #turn into dates
      dV$Screen = as_date(dV$Screen) 
      dV$Intake = as_date(dV$Intake)
      dV$StartStudy = as_date(dV$StartStudy)
      dV$Followup1 = as_date(dV$Followup1)
      dV$Followup2 = as_date(dV$Followup2)
      dV$EndStudy = as_date(dV$EndStudy)
      dV$FinalVisit = as_date(dV$FinalVisit)
      
      #Remove all but first row
      dV = slice(dV,1)
      
      #Write data
      write_rds(dV, file.path(path_database,sSubID, str_c(sSubID,'_VisitDates.rds')))
      rm(dV)
      
    }
    else{
      message('......WARNING MESSAGE: Visit Dates file does not exist')
    }
    
    # Locations interview ------------------------------------
    message('...Processing Locations Report')
    if(file.exists(file.path(path_raw, sSubID, str_c(sSubID,'_Locations.xlsx')))){
      addLocationsReport(nSubID, path_raw,path_database)
    }
    else{
      message('......WARNING MESSAGE: Locations Report file does not exist')
    }
    
    # Contacts Interview ----------------------------------------------
    message('...Processing Contacts Report')
    if(file.exists(file.path(path_raw, sSubID, str_c(sSubID,'_Contacts.xlsx')))){
      addContactsReport(nSubID, path_raw, path_database)
    }
    else{
      message('......WARNING MESSAGE: Contacts Report file does not exist')
    }
    
    # Risk Dates Interview -----------------------------------------------
    message('...Processing Risk Dates Report')
    if(file.exists(file.path(path_raw, sSubID, str_c(sSubID,'_Dates.xlsx')))){
      addDatesReport(nSubID, path_raw, path_database)
    }
    else{
      message('......WARNING MESSAGE: Dates Report file does not exist')
    }

    # Risk Times Interview -----------------------------------------------
    message('...Processing Risk Times Report')
    if(file.exists(file.path(path_raw, sSubID, str_c(sSubID,'_Times.xlsx')))){
      addTimesReport(nSubID, path_raw, path_database)
    }
    else{
      message('......WARNING MESSAGE: Times Report file does not exist')
    }  
  } #end if process interview files
 

}


# chkDataIntegrity-------------------------------------
chkDataIntegrity = function(nSubID, Period='All')
{
  RawPath =  'P:/StudyData/RISK/raw_data'
  DataBasePath = 'P:/StudyData/RISK/database'
  
  sSubID = varPadString(nSubID,3)
  
  #set start and end dates for period
  dDates = read_excel(file.path(RawPath, sSubID, str_c(sSubID, '_VisitDates.xlsx')), na= c('NA', ''))
  if (str_to_upper(Period)=='ALL'){
    PeriodStart = dDates$StartStudy
    PeriodEnd =   dDates$EndStudy
  }
  if (str_to_upper(Period)=='FOLLOWUP1'){
    PeriodStart = dDates$StartStudy
    PeriodEnd =   dDates$Followup1
  }
  if (str_to_upper(Period)=='FOLLOWUP2'){
    PeriodStart = dDates$Followup1
    PeriodEnd =   dDates$Followup2
  }
  if (str_to_upper(Period)=='FINALVISIT'){
    PeriodStart = dDates$Followup2
    PeriodEnd =   dDates$EndStudy
  }  
  rm(dDates)

  #covert to Date class in R
  PeriodStart = as_date(PeriodStart)  
  PeriodEnd =   as_date(PeriodEnd)
  
  #correct end date to current date if we havent reached that date yet
  if(PeriodEnd > Sys.Date()) PeriodEnd = Sys.Date()
  nDays = as.numeric(difftime(PeriodEnd, PeriodStart, units='days')) + 1

  #set up file to hold one row of data checks for nSubID
  dC = data.frame(SubID = nSubID,PeriodStart = PeriodStart, PeriodEnd=PeriodEnd, nDays = nDays)

  #Screen
  dS = read_rds(file.path(DataBasePath, 'Screen.rds'))
  dC$nScreen =sum(nSubID == dS$SubID)
  rm(dS)
  
  #Intake
  dI = read_rds(file.path(DataBasePath, 'Intake.rds'))
  dC$nIntake =sum(nSubID == dI$SubID)
  rm(dI)
  
  #Followup12
  dF12 = read_rds(file.path(DataBasePath, 'Followup12.rds'))
  dC$nFollowup12 =sum(nSubID == dF12$SubID)
  rm(dF12)
  
  #Followup3
  # dF3 = read.csv(file.path(DataBasePath, 'Followup3.csv'), header = TRUE, as.is=TRUE)
  # dC$nFollowup3 =sum(nSubID == dF3$SubID)
  # rm(dF3)
  
  #Sleep Schedule
  dSS = read_rds(file.path(DataBasePath, 'SleepSchedule.rds'))
  dC$nSleepSchedule =sum(sSubID == dSS$SubID)
  rm(dSS)
  
  #Audio transcripts
  AudioFiles =  dir(path = file.path(RawPath, sSubID, 'Audio'), 
                    pattern = sSubID, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  nAudios = 0
  for (AAudioFile in AudioFiles){
    TheDate = str_sub(AAudioFile,5,13)
    TheDate = str_c(str_sub(TheDate,1,4), '-', str_sub(TheDate,6,7), '-', str_sub(TheDate,8,9))
    TheDate = as_date(TheDate)
    if(TheDate>=PeriodStart && TheDate <= PeriodEnd)  nAudios = nAudios + 1
  }
  dC$nAudioFiles =nAudios
  dC$pAudioFiles= round((dC$nAudio/nDays) * 100,1)
  
  dA = read_rds(file.path(DataBasePath, 'Audio.rds'))
  dA = filter(dA,SubID==nSubID)
  dA$Date = as_date(as_datetime(dA$UTC,tz="America/Chicago"))
  dA = filter(dA,Date>=PeriodStart & Date<=PeriodEnd)
  dC$mAudioTrans = dC$nAudioFiles -nrow(dA)
  rm(dA)
  
  #EMA
  dE1 = read_rds(file.path(DataBasePath, 'EMAMorning.rds'))
  dE2 = read_rds(file.path(DataBasePath, 'EMALater.rds'))
  dE = bind_rows(dE1,dE2) #merge EMA dataframes for total
 
  #EMA - Morning
  dE1 = filter(dE1,SubID==sSubID)
  dE1$Date = as_date(as_datetime(dE1$UTC,tz="America/Chicago"))
  dE1 = filter(dE1,Date>=PeriodStart & Date<=PeriodEnd)
  
  dC$nEMAMTot =nrow(dE1)
  
  dE1 = filter(dE1,!is.na(dE1$EMA_7))  #checking on complete through 7
  dC$nEMAMComp =nrow(dE1)
  
  tDate = table(dE1$Date)
  tDate[tDate>1] = 1
  dC$nEMAMVal = sum(tDate)
  
  nRequired = nDays * 1
  dC$pEMAMVal= round((dC$nEMAMVal/nRequired) * 100,1)
  rm(dE1)
  
  #EMA - Later
  dE2 = filter(dE2,SubID==sSubID)
  dE2$Date = as_date(as_datetime(dE2$UTC,tz="America/Chicago"))
  dE2 = filter(dE2,Date>=PeriodStart & Date<=PeriodEnd)
  
  
  dC$nEMALTot =nrow(dE2)
  
  dE2 = filter(dE2,!is.na(dE2$EMA_7))  #checking on complete through 7
  dC$nEMALComp =nrow(dE2)
  
  tDate = table(dE2$Date)
  tDate[tDate>3] = 3
  dC$nEMALVal = sum(tDate)
  
  nRequired = nDays * 3
  dC$pEMALVal= round((dC$nEMALVal/nRequired) * 100,1)
  rm(dE2)
  
  
  #EMA-Total
  dE = filter(dE,SubID==sSubID)
  dE$Date = as_date(as_datetime(dE$UTC,tz="America/Chicago"))
  dE = filter(dE,Date>=PeriodStart & Date<=PeriodEnd)
  
  dC$nEMATot =nrow(dE)

  dE = filter(dE,!is.na(dE$EMA_7))  #checking on complete through 7
  dC$nEMAComp =nrow(dE)

  tDate = table(dE$Date)
  tDate[tDate>4] = 4
  #dE = ddply(.data= dE, .variables= c('Date'), .fun= summarise, Cnt = length(EMA_7))
  #dE$Cnt[dE$Cnt>4] = 4  
  dC$nEMAVal = sum(tDate)
  
  nRequired = nDays * 4
  dC$pEMAVal= round((dC$nEMAVal/nRequired) * 100,1)
  rm(dE)
  
  #GPS
  if (file.exists(file.path(DataBasePath, sSubID, str_c(sSubID, '_GPS.rds')))){
    dGPS = read_rds(file.path(DataBasePath, sSubID, str_c(sSubID, '_GPS.rds')))
    dGPS$Date = as_date(as_datetime(dGPS$UTC, tz = 'America/Chicago'))
    dGPS = filter(dGPS,Date>=PeriodStart & Date<=PeriodEnd)
    dC$nGPSPoints = nrow(dGPS)
    
    if(nrow(dGPS)>0){
      # dGPS = ddply(.data= dGPS, .variables= c('Date'), .fun= summarise, Cnt = length(UTC))  #Summarize dataframe
      # dGPS = subset(dGPS, Cnt>9)  #only count days as tracking when at least 10 GPS trackpoints recorded
      tGPS = table(dGPS$Date)
      dC$nGPSDays = sum(tGPS>9)
      dC$pGPSDays = round((dC$nGPSDays/nDays) * 100,1)
    }else{
      dC$nGPSDays = 0
      dC$pGPSDays = 0
    }
    rm(dGPS)
  }else{
    dC$nGPSPoints = NA
    dC$nGPSDays = NA
    dC$pGPSDays = NA
  }
  
  #SMS
  if(file.exists(file.path(DataBasePath, sSubID, str_c(sSubID, '_SMS.rds')))){
    dSMS = read_rds(file.path(DataBasePath, sSubID, str_c(sSubID, '_SMS.rds')))
    dSMS$Date = as_date(as_datetime(dSMS$UTC, tz = 'America/Chicago'))
    dSMS = filter(dSMS,Date>=PeriodStart & Date<=PeriodEnd)
    dC$nSMS = nrow(dSMS)
    rm(dSMS)
  }else{
    dC$nSMS = NA
  }
  
  #Voice
  if(file.exists(file.path(DataBasePath, sSubID, str_c(sSubID, '_Voice.rds')))){
    dV = read_rds(file.path(DataBasePath, sSubID, str_c(sSubID, '_Voice.rds')))
    dV$Date = as_date(as_datetime(dV$UTC, tz = 'America/Chicago'))
    dV = filter(dV,Date>=PeriodStart & Date<=PeriodEnd)
    dC$nVoice = nrow(dV) 
    rm(dV)
  }else{
    dC$nVoice = NA
  }
  
  #Beddit (PENDING)
  
  #Contacts
  if(file.exists(file.path(DataBasePath, sSubID, str_c(sSubID, '_Contacts.rds')))){
    dCon = read_rds(file.path(DataBasePath, sSubID, str_c(sSubID, '_Contacts.rds')))
    dC$nContacts = nrow(dCon) 
    rm(dCon)
  }else{
    dC$nContacts = NA
  } 
  
  #Locations
  if(file.exists(file.path(DataBasePath, sSubID, str_c(sSubID, '_Locations.rds')))){
    dL = read_rds(file.path(DataBasePath, sSubID, str_c(sSubID, '_Locations.rds')))
    dC$nLocations = nrow(dL) 
    rm(dL)
  }else{
    dC$nLocations = NA
  } 
  
  #Dates
  if(file.exists(file.path(DataBasePath, sSubID, str_c(sSubID, '_Dates.rds')))){
    dD = read_rds(file.path(DataBasePath, sSubID, str_c(sSubID, '_Dates.rds')))
    dC$nDates = nrow(dD) 
    rm(dD)
  }else{
    dC$nDates = NA
  }   
  
  #Times
  if(file.exists(file.path(DataBasePath, sSubID, str_c(sSubID, '_Times.rds')))){
    dT = read_rds(file.path(DataBasePath, sSubID, str_c(sSubID, '_Times.rds')))
    dC$nTimes = nrow(dT) 
    rm(dT)
  }else{
    dC$nTimes = NA
  }   
  
  return(dC)
}

#addAudio------------------
#Audio Transcript Excel File
addAudio = function(InPath,path_database)  #, k=100, wvl=50
{
  message('...Processing Audio Transcripts')
  
  #Open file
  dA = read_excel(file.path(InPath, 'Audio.xlsx'), na= c('NA', ''))

  
  #remove leading/trailing ws
  dA$MessageName = str_trim(dA$MessageName)
  
  #Remove extension if exists
  dA$MessageName = str_replace(dA$MessageName, '.m4a', '')
  
  #confirm that all Filename entries are 18 characters exactly after above cleaning
  for (i in (1:length(dA$MessageName))){
    if(nchar(dA$MessageName[i]) != 18){
      stop(sprintf('MessageName entry (%s) in row %d in Audio.xlsx not = 18 characters', dA$MessageName[i], i))
    }
  }
  
  #Get SubID
  dA$SubID= str_sub(dA$MessageName,1,3)
  dA$SubID = as.numeric(dA$SubID)
  
  #Get UTC
  Date = str_sub(dA$MessageName,5)
  dA$UTC = as.numeric(as_datetime(x=Date[1], format='%Y_%m%d_%H%M', tz = "America/Chicago"))
  
  dA$MessageName = NULL  #No longer need FileName variable b/c have SubID and Date
  dA$Transcriber = NULL   #no need for info about who transcribed
  
  # #Process/clean text transcript
  # Text = dA$Text
  # Text = iconv(Text, 'UTF-8', 'ASCII', sub='')
  # Text = removeNumbers(Text)
  # Text = stripWhitespace(Text)
  # Text = tolower(Text)
  # Text = removePunctuation(Text)
  # Text = replace_abbreviation(Text)
  # Text = replace_contraction(Text)
  # Text = replace_symbol(Text)
  # 
  # AllStops = c(stopwords(), 'candace', 'uh', 'um', 'oh', 'inaudible')
  # Text = removeWords(Text, AllStops)
  # 
  # #Text = stemDocument(Text)
  # #Text =  stemCompletion(Text)
  # 
  # #make DT Matrix and add to dA
  # vsText <- VectorSource(Text)
  # vcText <- VCorpus(vsText)
  # dtmText <- DocumentTermMatrix(vcText)
  # mText = as.matrix(dtmText)
  # 
  # StartCol = ncol(dA) + 1
  # dA = cbind(dA,mText) #Add bag of words to dA
  # colnames(dA)[StartCol:ncol(dA)]= str_c('BOW', 1:ncol(mText))
  # 
  # #SVD of DTM and add to dA
  # svdText = svd(mText)
  # #U <- svdText$u   #doc by signular vectors
  # #D <- svdText$d   #singular values
  # #D = D^2 / sum(D^2)  #covert to proportion of variance
  # V = svdText$v   #loadings of words onto vectors
  # V_k = V[,1:k]  #include only first k SVDs
  # SVDs = mText %*% V_k
  # 
  # StartCol = ncol(dA)+1
  # dA = cbind(dA,SVDs) #Add SVD scores to dA
  # names(dA)[StartCol:(ncol(dA))] = str_c('SVD', 1:k)
  # 
  # 
  # #Word2Vec vectors and add to dA
  # wordvec_len = wvl
  # tokens <- space_tokenizer(Text)
  # it = itoken(tokens, progressbar = FALSE)
  # vocab <- create_vocabulary(it)
  # vocab <- prune_vocabulary(vocab, term_count_min = 5L)
  # vectorizer <- vocab_vectorizer(vocab)
  # tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
  # glove = GlobalVectors$new(word_vectors_size = wordvec_len, vocabulary = vocab, x_max = 10)
  # word_vectors_main = glove$fit_transform(tcm, n_iter = 20)
  # word_vectors_context = glove$components
  # word_vectors = word_vectors_main + t(word_vectors_context)
  # words = vocab$term
  # I = 1 : length(Text)
  # X = matrix(NA, nrow = length(Text), ncol = wordvec_len)
  # for (i in I) {
  #   doci = tokens[[i]]
  #   matched = match(doci, words)
  #   matched = na.omit(matched)
  #   if (length(matched) > 1){
  #     doc_vec = colSums(word_vectors[matched, ])/length(matched)
  #     X[i, ] = doc_vec
  #   }
  #   else if(length(matched) == 1){
  #     X[i, ] = word_vectors[matched, ]
  #   }
  #   else{
  #     print(paste('no representation for doc', toString(i)))
  #   }
  # }
  # 
  # StartCol = ncol(dA)+1
  # dA = cbind(dA,X) #Add W2V scores to dA
  # names(dA)[StartCol:(ncol(dA))] = str_c('WV', 1:wvl)
  
  
  #Reorder Variables for UTC and SubID first
  FirstVars = c("UTC", "SubID")
  dA = dA[, c(FirstVars, setdiff(names(dA), FirstVars))]
  
  #Add Audio_ to all variables but UTC and SubID
  #VarNames = setdiff(names(dA), FirstVars)
  #dA = addVarNamePrefix(dA,'Audio_',3)
  
  #Write data
  write_rds(dA, file.path(path_database, 'audio.rds'))
}

#addLocationsReport------------------
addLocationsReport = function(nSubID, InPath,path_database)
{
  sSubID = varPadString(nSubID,3)
  FileName = str_c(sSubID,'_Locations.xlsx')

  dL = read_excel(file.path(InPath, sSubID, str_c(sSubID,'_Locations.xlsx')), na= c('NA', ''))
  
  #rename variables if using old format
  if('DrankHere' %in% names(dL)){
    dL = varRename(dL,c('DrankHere', 'AlcoholHere', 'LocationEmotion','LocationRisk','RiskAvoid'), 
                 c('Drank',     'Alcohol',     'Emotion',        'Risk',        'Avoid'))
  }
  
  #Remove leading/trailing spaces and convert to All Caps
  dL$Type = toupper(str_trim(dL$Type))
  dL$Drank = toupper(str_trim(dL$Drank))
  dL$Alcohol = toupper(str_trim(dL$Alcohol))
  dL$Emotion = toupper(str_trim(dL$Emotion))
  dL$Risk = toupper(str_trim(dL$Risk))
  dL$Avoid = toupper(str_trim(dL$Avoid))
  dL$Vacation = toupper(str_trim(dL$Vacation))
  
  # #Remove empty rows at end of file
  dL = subset(dL,!apply(is.na(dL),1,all))
  
  dL = mutate(dL, UTC = as.numeric(UTC))
  if(any(is.na(dL$UTC))) stop('Detected NAs for UTC in dL')
  
  #if file not empty process furhter
  if(nrow(dL)>0){
    #add Lat/Long if needed
    if(!('FullAddress' %in% names(dL))){
      dL$FullAddress =  str_c(dL$StreetAddress, dL$City, dL$State, sep=', ')
      dL$Lat = NA
      dL$Long = NA
    }
    
    #Add missing full address if needed
    if(any(is.na(dL$FullAddress))) {
      dL$FullAddress =  str_c(dL$StreetAddress, dL$City, dL$State, sep=', ')
    }
    
    #loop to check for missing Lat/Long
    for(i in 1:nrow(dL)){
      if(is.na(dL$Lat[i])){
        dL[i,c('Long', 'Lat')] = geocode(dL$FullAddress[i], output = 'latlon', source = 'google')
        Sys.sleep(2)
        #cludge to fix weird google bug with NAs returned occassionally
        while(is.na(dL$Long[i]))
        {
          dL[i,c('Long', 'Lat')] = geocode(dL$FullAddress[i], output = 'latlon', source = 'google')
          Sys.sleep(2)
        }
      }
    }
    
    #save updated raw excel 
    write_xlsx(data.frame(dL), file.path(InPath, sSubID, FileName), format_headers = FALSE)
  }else{
    message('......WARNING MESSAGE: No data/rows in Locations Report')
  }
  
  #Write data
  write_rds(dL, file.path(path_database,sSubID, str_c(sSubID,'_Locations.rds')))
}

#addContactsReport------------------
addContactsReport = function(nSubID, InPath,path_database)
{
  sSubID = varPadString(nSubID,3)
  FileName = str_c(sSubID,'_Contacts.xlsx')
  dC = read_excel(file.path(InPath, sSubID, FileName), na= c('NA', ''))
  
  #Check for character phone numbers (i.e., entered by mistake with '-')
  if (any(is.character(dC$HomePhone))){
    stop('Character data in HomePhone in Contacts.xlsx')
  }
  if (any(is.character(dC$CellPhone))){
    stop('Character data in CellPhone in Contacts.xlsx')
  }
  if (any(is.character(dC$OtherPhone1))){
    stop('Character data in OtherPhone1 in Contacts.xlsx')
  }
  if (any(is.character(dC$OtherPhone2))){
    stop('Character data in OtherPhone2 in Contacts.xlsx')
  }
  
  #create list of all phone numbers
  AllNums = c(dC$HomePhone, dC$CellPhone, dC$OtherPhone1, dC$OtherPhone2)
  
  #check/remove duplicates
  tC = table(AllNums)
  tDups = tC[tC>1]
  if(length(tDups>0)){
    warning(str_c('Duplicate Phone numbers detected in ', sSubID, '_Contacts.xlsx'))
    Dups = as.numeric(names(tDups))
    
    for (iNum in Dups){
      DupRows = which(dC$HomePhone==iNum | dC$CellPhone==iNum | dC$OtherPhone1==iNum | dC$OtherPhone2==iNum)
      DupRows = DupRows[2:length(DupRows)]  #exclude first entry
      dC = dC[-DupRows,]  #remove remaining rows
    }
  }
  
  #Rename Variables
  dC = varRename(dC,c('ContactDrankPast','DrinkStatus','ContactDrinkFuture','SupportStatus','ContactExperience'), 
                 c('DrankPast','DrinkerStatus','DrinkFuture','Support','Emotion'))
  
  #remove leading/trailing spaces and change to all caps
  dC$Type = toupper(str_trim(dC$Type))
  dC$DrankPast = toupper(str_trim(dC$DrankPast))
  dC$DrinkerStatus = toupper(str_trim(dC$DrinkerStatus))
  dC$DrinkFuture = toupper(str_trim(dC$DrinkFuture))
  dC$Recovery = toupper(str_trim(dC$Recovery))
  dC$Support = toupper(str_trim(dC$Support))
  dC$Emotion = toupper(str_trim(dC$Emotion))
  
  #Fix other common data entry errors
  dC$DrankPast[dC$DrankPast=='ALWAYS/ALMOST ALWAYS'] = 'ALMOST ALWAYS/ALWAYS'
  dC$DrankPast[dC$DrankPast=='N/A'] = 'NA'
  dC$DrankPast[dC$DrankPast=='OCASSIONALLY'] = 'OCCASIONALLY'
  dC$DrankPast[dC$DrankPast=='OCCASSIONALLY'] = 'OCCASIONALLY'
  dC$DrankPast[dC$DrankPast=='OCCATIONALLY'] = 'OCCASIONALLY'
  dC$DrankPast[dC$DrankPast=='OCCCASIONALLY'] = 'OCCASIONALLY'
  dC$DrankPast[dC$DrankPast=='NEVE/ALMOST NEVER'] = 'NEVER/ALMOST NEVER'
  dC$DrankPast[dC$DrankPast=='NEVER/ALWAYS NEVER'] = 'NEVER/ALMOST NEVER'
  dC$DrinkerStatus[dC$DrinkerStatus=='N/A'] = 'NA'
  dC$DrinkFuture[dC$DrinkFuture=='N/A'] = 'NA'
  dC$Recovery[dC$Recovery=='N/A'] = 'NA'
  dC$Support[dC$Support=='N/A'] = 'NA'
  dC$Emotion[dC$Emotion=='N/A'] = 'NA'
  dC$Emotion[dC$Emotion=='UNPLEASANT/MIXED'] = 'MIXED'
  dC$Type[dC$Type=='CO-WORKER/BUSINESS CONTACTS'] = 'CO-WORKER/BUSINESS CONTACT'
  dC$Type[dC$Type=='CO-WORKERS/BUSINESS CONTACTS'] = 'CO-WORKER/BUSINESS CONTACT'
  dC$Type[dC$Type=='FAMILY'] = 'FAMILY-OTHER'
  dC$Type[dC$Type=='FAMILY - OTHER'] = 'FAMILY-OTHER'
  
  #Remove empty rows at end of file
  dC = subset(dC,!apply(is.na(dC),1,all))
  
  dC = mutate(dC, UTC = as.numeric(UTC))
  if(any(is.na(dC$UTC))) stop('Detected NAs for UTC in dC')
  
  #Add Contacts_ to all variables but UTC
  #dC = addVarNamePrefix(dC,'Contacts_',2)
  
  #prepend identifier to each record for multi column features
  #dC = addMCF(dC,2)
  
  #Write data
  write_rds(dC, file.path(path_database,sSubID, str_c(sSubID,'_Contacts.rds')))
}




#addDatesReport------------------
addDatesReport = function(nSubID, InPath,path_database){
  sSubID = varPadString(nSubID,3)
  FileName = str_c(sSubID,'_Dates.xlsx')
  dD = read_excel(file.path(InPath, sSubID, str_c(sSubID,'_Dates.xlsx')), na= c('NA', ''))
  
  #cibnvert to date
  dD$Date = as_date(dD$Date)
  
  #Remove empty rows at end of file
  dD = subset(dD,!apply(is.na(dD),1,all))
  
  dD = mutate(dD, UTC = as.numeric(UTC))
  if(any(is.na(dD$UTC))) stop('Detected NAs for UTC in dD')
  
  #Add Dates_ to all variables but UTC
  #dD = addVarNamePrefix(dD,'Dates_',2)
  
  #prepend identifier to each record for multi column features
  #dD = addMCF(dD,2)
  
  #Write data
  write_rds(dD, file.path(path_database,sSubID, str_c(sSubID,'_Dates.rds')))
}


#addTimesReport------------------
addTimesReport = function(nSubID, InPath,path_database){
  sSubID = varPadString(nSubID,3)
  FileName = str_c(sSubID,'_Times.xlsx')
  dT = read_excel(file.path(InPath, sSubID, str_c(sSubID,'_Times.xlsx')), na= c('NA', ''))
  
  # #Remove empty rows at end of file
  dT = subset(dT,!apply(is.na(dT),1,all))
  
  dT = mutate(dT, UTC = as.numeric(UTC))
  if(any(is.na(dT$UTC))) stop('Detected NAs for UTC in dT')
  
  #Add Times_ to all variables but UTC
  #dT = addVarNamePrefix(dT,'Times_',2)
  
  #prepend identifier to each record for multi column features
  #dT = addMCF(dT,2)
  
  #Write data
  write_rds(dT, file.path(path_database,sSubID, str_c(sSubID,'_Times.rds')))
}

#addSMS------------------
addSMS = function(nSubID, InPath,path_database,StartDate, EndDate){
  sSubID = varPadString(nSubID,3)
  
  SMSFiles = list.files(file.path(InPath, sSubID), pattern = '_SMS_')
  
  if(length(SMSFiles)>0){
    for (SMSFile in SMSFiles){
      message(str_c('......', SMSFile))
      #check if carbonite or iTunes
      if(str_detect(SMSFile,'.xml')){#IF ANDROID/Carbonite
        
        #open the raw SMS file via read_xml and read it into an xml nodeset via xml_children
        #lC = xml_children(read_xml(file.path(InPath, sSubID, SMSFile)))
        #changed to read_html from read_xml(above line) to strip emojii
        lD = xml_children(read_html(file.path(InPath, sSubID, SMSFile)))  
        
        #fix for problem with nested list after switching to read_html()
        while (length(lD)==1) {
          lD = xml_children(lD) #loop to find lower level of xml children - SS
        }
        
        #trawl the nodes extracting each attribute value
        dD = data.frame(
          #protocol = xml_attr(lD,'protocol'),
          address = xml_attr(lD,'address'),
          date = xml_attr(lD,'date'),
          type = xml_attr(lD,'type'),
          #subject = xml_attr(lD,'subject'),
          body = xml_attr(lD,'body'),
          #toa = xml_attr(lD,'toa'),
          #sc_toa = xml_attr(lD,'sc_toa'),
          #service_center = xml_attr(lD,'service_center'),
          #read = xml_attr(lD,'read'),
          #status = xml_attr(lD,'status'),
          #locked = xml_attr(lD,'locked'),
          #date_sent = xml_attr(lD,'date_sent'),
          #readable_date = xml_attr(lD,'readable_date'),
          #contact_name = xml_attr(lD,'contact_name'),
          stringsAsFactors = FALSE
        )
        
        dSMST = data.frame(UTC=rep(NA,nrow(dD)), Phone= rep(NA,nrow(dD)), Text=rep(NA,nrow(dD)), Type=rep(NA,nrow(dD)))
        
        #time stamps are either 10 digit (s) or 13 digit (ms) unix time stamps
        dD$date = as.numeric(dD$date)
        dSMST$UTC = ifelse(trunc(log10(dD$date))+1==10, dD$date, dD$date/1000)
        
        dSMST$Phone = suppressWarnings(as.numeric(dD$address))  #warnings for emails as phone number
        dSMST$Type = as.numeric(dD$type)   
        dSMST$Text = dD$body
        
        rm(dD)
        rm(lD)
        gc()
        }
      else{#ELSE IOS 
        
        cSMS = dbConnect(RSQLite::SQLite(), file.path(InPath,sSubID,SMSFile))
        dH=dbGetQuery( cSMS,'select * from handle' )
        dD=dbGetQuery( cSMS,'select * from message' )
        
        #close the db connection
        dbDisconnect(cSMS)
        
        #replace handle_id with phone number from handle ID dataframe
        dD = dD[!is.na(dD$handle_id),]  #select out any NA.  Very rare problem
        if(nrow(dD)>0){
          for (i in 1:nrow(dD)){
            if (dD$handle_id[i]=='0'){  #some handle IDs are zero for unknown reason
              dD$handle_id[i] =NA
            } 
            else{
              dD$handle_id[i] = dH$id[which(dH$ROWID==dD$handle_id[i])]
            }
          }
        }
        dSMST = data.frame(UTC=rep(NA,nrow(dD)), Phone= rep(NA,nrow(dD)), Text=rep(NA,nrow(dD)), Type=rep(NA,nrow(dD)))
        
        #convert date stamp to unix time UTC
        #for 9 digit apple stamps:  dD$date + 978307200
        #for 18 digit apple stamps:  dD$date/1000000000 + 978307200
        dSMST$UTC = ifelse(trunc(log10(dD$date))+1 == 9, dD$date + 978307200,dD$date/1000000000 + 978307200)   #convert from IOS time sample (secs since 1/1/2001) to unix time stamp (secs since 1/1/1970)
        
        dSMST$Phone = dD$handle_id
        dSMST$Type = (dD$is_from_me + 1)  #1 is incoming, 2 is outgoing;  draft is not defined for IOS at this point
        dSMST$Text = dD$text
        
        dSMST$Text[dSMST$Text=='?'] = ''
        
        rm(dD)
        rm(dH)
      }
      
      #Merge
      if(!exists('dSMS')){
        dSMS = dSMST  #First SMS file
      }else{
        dSMS = dfMerge(dSMS, dSMST, AddVars=FALSE) #merge in new file
      }
      rm(dSMST)   
    }
  }
  else{
    message('......WARNING MESSAGE: SMS file(s) do not exist')
  }
  
  if (exists('dSMS') && nrow(dSMS)>0){  #only do more if there are SMS for this subject
    #Remove duplicates after merges
    #exclude Text from duplicates
    dTmp = dSMS[,c('UTC', 'Phone', 'Type')]
    dSMS = subset(dSMS, !duplicated(dTmp))
    rm(dTmp)
    
    #removed leading + and other non-numeric characters
    if(!is.numeric(dSMS$Phone)){
      dSMS$Phone = str_replace_all(dSMS$Phone, '\\+', '')
      dSMS$Phone = str_replace_all(dSMS$Phone, '\\*', '')
      dSMS$Phone = str_replace_all(dSMS$Phone, '\\(', '')
      dSMS$Phone = str_replace_all(dSMS$Phone, '\\)', '')
      dSMS$Phone = str_replace_all(dSMS$Phone, '\\-', '')
      dSMS$Phone = str_replace_all(dSMS$Phone, '\\s', '')
      dSMS$Phone = str_replace_all(dSMS$Phone, '\\.', '')
      
      #Convert Phone to numeric
      dSMS$Phone = as.numeric(dSMS$Phone)
    }
    
    #Add 608 to 7 digit numbers and remove US country code
    for(i in 1:nrow(dSMS)){
      if(!is.na(dSMS$Phone[i]) && trunc(log10( dSMS$Phone[i]))+1==7) dSMS$Phone[i] = dSMS$Phone[i] + 6080000000
      if(!is.na(dSMS$Phone[i]) && varParse(dSMS$Phone[i],10000000000,10000000000)==1) dSMS$Phone[i] = dSMS$Phone[i]%%10000000000
    }
    
    #remove leading/trailing ws
    dSMS$Text = str_trim(dSMS$Text)
    
    #remove \r, \n
    dSMS$Text = str_replace_all(dSMS$Text, '\r', '')
    dSMS$Text = str_replace_all(dSMS$Text, '\n', '')
    
    #remove "
    dSMS$Text = str_replace_all(dSMS$Text, '"', '')
    
    #remove SMS outside study period
    dSMS$TempDate = as_date(as_datetime(dSMS$UTC, tz='America/Chicago'))
    dSMS = filter(dSMS,TempDate>=StartDate & TempDate<=EndDate)
    dSMS$TempDate = NULL
    
    #Reorder Variables for UTC first
    FirstVars = c("UTC")
    dSMS = dSMS[, c(FirstVars, setdiff(names(dSMS), FirstVars))]
  }
  
  if(exists('dSMS')){
    FileName = str_c(sSubID,'_SMS.rds')
    write_rds(dSMS, file.path(path_database, sSubID, FileName))
  }
}

#removed since we now use a standalone script - SS 2020/06/10
# #get_qualtrics_surveys---------------------
# #Downloads to raw data folder all RISK qualtrics batteries as csv files and renames as needed
# 
# get_qualtrics_surveys = function(raw_path)
# {
#   
#   #Morning EMA
#   message("...Downloading Morning EMA")
#   dEMAM = apiGetSurvey(SurveyID="SV_bBGKAzNQeyXuN6Z", UseLabels = FALSE)
#   
#   write_csv(dEMAM, file.path(raw_path, "EMAMorning.csv"))
#   
#   #Later EMA
#   message("...Downloading Later EMA")
#   dEMAL = apiGetSurvey(SurveyID="SV_aViOZNT66b7wff7", UseLabels = FALSE)
#   write_csv(dEMAL, file.path(raw_path, "EMALater.csv"))
#   
#   #Screen
#   message("...Downloading Screening Battery")
#   id = apiGetSurvey(SurveyID="SV_4GfoMwQRJXoTJQ1", UseLabels = FALSE)
#   write_csv(id, file.path(raw_path, "Screen.csv"))
#   
#   #Intake
#   message("...Downloading Intake Battery")
#   sr = apiGetSurvey(SurveyID="SV_b49P4TTAAlVMjUF", UseLabels = FALSE)
#   write_csv(sr, file.path(raw_path, "Intake.csv"))
#   
#   #Follow-up12
#   message("...Downloading Follow-up Battery")
#   dF12 = apiGetSurvey(SurveyID="SV_3a8KivBmHug8f65", UseLabels = FALSE)
#   write_csv(dF12, file.path(raw_path, "Followup12.csv"))
#   
#   #Follow-up3
#   message("...Downloading Final Visit Battery")
#   dF3 = apiGetSurvey(SurveyID="SV_6rn6hZggROy1Mvr", UseLabels = FALSE)
#   write_csv(dF3, file.path(raw_path, "Followup3.csv"))
#   
#   #Sleep Schedule
#   message("...Downloading Sleep Schedule Battery")
#   dSS = apiGetSurvey(SurveyID="SV_exqjLqg1OohSqvX", UseLabels = FALSE)
#   write_csv(dSS, file.path(raw_path, "SleepSchedule.csv"))
# }
