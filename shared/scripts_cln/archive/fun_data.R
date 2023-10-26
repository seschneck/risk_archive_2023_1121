library(tidyverse)
library(lmSupport)
library(lubridate)
#library(stringr)
library(timeDate)  #FIX:  Do we need this versus lubridate??
#library(ggmap)  #geocode, revgeocode
library(geosphere)  #distGeo
#library(tm)  #text mining
#library(text2vec)
#library(iterators)
#library(doParallel)
library(janitor)

#make_data()---------------------
#This script makes the final dataframes used to calculate Y and X.
#Takes as input the raw, cleaned data (csv files) in the /database folder
#Makes and saves dataframes in the /analysis/shared/data folder
make_data = function(Type = "ALL", in_path, out_path)
{

  #List of SubIDS for data in folders
  SubIDs = list.dirs(path=in_path, full.names = FALSE, recursive = FALSE) #gets folder names from RawData folder
  SubIDs = suppressWarnings(as.numeric(SubIDs)) #makes SubID character strings numeric, turns text into NA
  SubIDs = subset(SubIDs,!is.na(SubIDs)) #removes NAs
  
  message("Processing Aggregate Files")
  
  #EMA ---------
  if(toupper(Type) =="ALL" || toupper(Type) =="EMA" ){
    message("...Processing EMA")
    dM = read_rds(file.path(in_path,"ema_morning.rds"))
    dM$Type=1L #indicate it is morning EMA
    dL = read_rds(file.path(in_path,"ema_later.rds"))
    dL$Type=2L #indicate it is later EMA
    dL$EMA_8 = NA
    dL$EMA_9 = NA
    dL$EMA_10 = NA
    
    dE = bind_rows(dM,dL)
    
    #using as.POSIXct instead of as_datetime intentionally b/c of problem with class
    dE$SubID = as.integer(dE$SubID)
    dE$EMA_1.1 = as.POSIXct(str_c(dE$EMA_1.1, " ", dE$EMA_1.2, ":00:00"), format="%m-%d-%Y %H:%M:%S",  tz="America/Chicago")
    dE$EMA_1.2 = NULL
    dE$EMA_1.3 =  as.POSIXct(str_c(dE$EMA_1.3, " ", dE$EMA_1.4, ":00:00"), format="%m-%d-%Y %H:%M:%S", tz="America/Chicago")
    dE$EMA_1.4 = NULL
    
    #Exclude participants without raw data folder and sort
    dE <- filter(dE,SubID %in% SubIDs) %>% 
      arrange(SubID, UTC) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id)
    
    write_rds(dE, file.path(out_path,"ds_ema.rds"))
    rm(dE)
    rm(dL)
    rm(dM)
  }
  
  #Morning Audio dataframe ----------
  #NEED TO MOVE FEATURE ENGINEERING FROM HERE TO fun_xy
  if(toupper(Type) =="ALL" || toupper(Type) =="AUDIO" ){
    message("...processing Morning Audio")
    # #CONSTANTS TO CONSIDER
    # k=100
    # wvl=50
    
    dA = read_rds(file.path(in_path,"audio.rds"))
    dA$SubID = as.integer(dA$SubID)
    
    # #Process/clean text transcript
    # Text = dA$Text
    # Text = iconv(Text, "UTF-8", "ASCII", sub="")
    # Text = removeNumbers(Text)
    # Text = stripWhitespace(Text)
    # Text = tolower(Text)
    # Text = removePunctuation(Text)
    # Text = replace_abbreviation(Text)
    # Text = replace_contraction(Text)
    # Text = replace_symbol(Text)
    # 
    # AllStops = c(stopwords(), "candace", "uh", "um", "oh", "inaudible")
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
    # colnames(dA)[StartCol:ncol(dA)]= str_c("BOW", 1:ncol(mText))
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
    # names(dA)[StartCol:(ncol(dA))] = str_c("SVD", 1:k)
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
    #     print(paste("no representation for doc", toString(i)))
    #   }
    # }
    # 
    # StartCol = ncol(dA)+1
    # dA = cbind(dA,X) #Add W2V scores to dA
    # names(dA)[StartCol:(ncol(dA))] = str_c("WV", 1:wvl)
    
    write_rds(dA, file.path(out_path,"ds_audio.rds"))
    rm(dA)
  }
  
  #Survey dataframes for 1X ID and 3X SR signals------------
  if(toupper(Type) =="ALL" || toupper(Type) =="SURVEY" ){
    message("...Processing surveys")
    
    #Screen
    id <- read_rds(file.path(in_path,"screen.rds")) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>% 
      mutate(subid = as.integer(subid)) %>% 
      filter(subid %in% SubIDs)
    

    #SR1 will use ASSIST 2-3 from screen as well as intake vars later.  But need to rescale to 1 month
    #Remaing ASSIST stay as id variables
    #not keeping UTC b/c will use from Intake
    vars_assist_sr_intake = c(str_c("assist_2_", 1:8), str_c("assist_3_", 1:8))
    assist_sr_intake <- select(id, one_of("subid", vars_assist_sr_intake)) %>% 
      mutate_at(vars_assist_sr_intake, ~recode(.,`1`=1, `2`=2, `3`=2, `4`=3, `5`=4))

    id %>% 
      select(-one_of(vars_assist_sr_intake)) %>% 
      select(utc, subid, everything()) %>% 
      arrange(subid, utc) %>% 
      write_rds(file.path(out_path,"ds_id.rds")) %>% 
      glimpse()
    
    rm(id)
    
    #Intake (& Screen)
    dSR1 <- read_rds(file.path(in_path,"intake.rds")) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>% 
      mutate(subid = as.integer(subid)) %>% 
      filter(subid %in% SubIDs) %>% 
      full_join(assist_sr_intake, by ="subid")
    
    #Follow-up12
    dSR23 <- read_rds(file.path(in_path,"followup12.rds")) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>% 
      mutate(subid = as.integer(subid)) %>% 
      filter(subid %in% SubIDs)

    dSR1 %>% 
      bind_rows(dSR23) %>% 
      select(-contains("rbm_")) %>% 
      select(utc, subid, everything()) %>% 
      arrange(subid, utc) %>% 
      write_rds(file.path(out_path,"ds_sr.rds")) %>% 
      glimpse()
    
    rm(assist_sr_intake)
    rm(dSR1)
    rm(dSR23)
  }
  
  message("Processing Subject Specific Files")
  #Loop for subject specific files
  dVi = NULL  #Visit dates
  dD = NULL   #Risk dates report
  dL = NULL   #Locations report
  dG = NULL   #GPS
  dC = NULL   #Contacts
  dV = NULL   #Voice calls
  dS = NULL   #SMS 
  
  for (ASubID in SubIDs)
  {
    message("...SubID: ", ASubID)
    
    #Visits dataframe ----------
    if(toupper(Type) =="ALL" || toupper(Type) =="VISITS" ){
      message("......Visits")
      aVi = read_rds(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_VisitDates.rds")))
      aVi$SubID = ASubID
      
      if(is.null(dVi)){
        dVi = aVi
      }else{
        dVi = bind_rows(dVi,aVi)
      }
      rm(aVi)
    }
    
    #Dates dataframe ----------
    if(toupper(Type) =="ALL" || toupper(Type) =="DATES" ){
      message("......Dates")
      if(file.exists(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_Dates.rds")))){
        aD = read_rds(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_Dates.rds")))
        if(nrow(aD)>0){
          aD$SubID = ASubID
          
          if(is.null(dD)){
            dD = aD
          }else{
            dD = bind_rows(dD,aD)
          }
        }
        rm(aD)
      }
    }
    
    if(toupper(Type) =="ALL" || toupper(Type) =="GPS" ){
      #Locations dataframe ----------
      message("......Locations")
      
      if(file.exists(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_Locations.rds")))){
        aL = read_rds(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_Locations.rds")))
        if(nrow(aL)>0){
          aL$SubID = ASubID
          
          if(is.null(dL)){
            dL = aL
          }else{
            dL = bind_rows(dL,aL)
          }
        }
      }else{
        aL = NULL  #for later checking when processing GPS
      }
      
      #GPS dataframe------------------
      message("......GPS")
      if(!is.null(aL) && file.exists(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_GPS.rds")))){
        aG = read_rds(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_GPS.rds")))
        
        if(nrow(aG)>0){
          aG$SubID = ASubID
          aG$PlaceType = NA
          aG$Drank = NA
          aG$Alcohol = NA
          aG$Emotion=NA
          aG$Risk = NA
          aG$Avoid = NA
          
          #merge in location context to place records
          if(nrow(aL)>0){
            for(iG in 1:nrow(aG)){
              if(aG$Type[iG]=="PLACE"){
                DistMatch = Inf
                iMatch = 0
                for(iL in 1:nrow(aL)){
                  Dist = distGeo(c(aG$Long[iG],aG$Lat[iG]), c(aL$Long[iL], aL$Lat[iL]))
                  if(Dist<DistMatch){
                    DistMatch = Dist
                    iMatch = iL
                  }
                }
                if(DistMatch <=60){
                  aG$PlaceType[iG] = aL$Type[iMatch]
                  aG$Drank[iG] = aL$Drank[iMatch]
                  aG$Alcohol[iG] = aL$Alcohol[iMatch]
                  aG$Emotion[iG] = aL$Emotion[iMatch]
                  aG$Risk[iG] = aL$Risk[iMatch]
                  aG$Avoid[iG] = aL$Avoid[iMatch]
                }
              }
            }
          }
          
          if(is.null(dG)){
            dG = aG
          }else{
            dG = bind_rows(dG,aG)
          }
        }
        rm(aG)
      }
      rm(aL)
    }
    
    if(toupper(Type) =="ALL" || toupper(Type) =="PHONE" ){
      #Contacts dataframe ----------
      message("......Contacts")
      if(file.exists(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_Contacts.rds")))){
        aC = read_rds(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_Contacts.rds")))
        if(nrow(aC)>0){
          aC$SubID = ASubID
          
          if(is.null(dC)){
            dC = aC
          }else{
            dC = bind_rows(dC,aC)
          }
        }
      }
      
      #Voice call dataframe------------------
      message("......Voice")
      if(file.exists(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_Voice.rds")))){
        aV = read_rds(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_Voice.rds")))
        
        if(nrow(aV)>0){
          aV$SubID = ASubID
          aV$ContactType = NA
          aV$DrankPast = NA
          aV$DrinkerStatus = NA
          aV$DrinkFuture = NA
          aV$Recovery = NA
          aV$Support=NA
          aV$Emotion = NA
          
          #merge in contacts context to voice calls
          if(nrow(aC)>0){
            for(iV in 1:nrow(aV)){
              for(iC in 1:nrow(aC)){
                # Match = any(aV$Voice_Phone[iV]==aC[iC,c("CellPhone","HomePhone",
                #                      "OtherPhone1","OtherPhone1")]) 
                Match = any(aV$Phone[iV] %in% c(aC[iC,"CellPhone"],aC[iC,"HomePhone"],
                                                aC[iC,"OtherPhone1"],aC[iC,"OtherPhone2"]))
                #if(is.na(Match)) Match = FALSE
                if(Match){
                  aV$ContactType[iV] = aC$Type[iC]
                  aV$DrankPast[iV] = aC$DrankPast[iC]
                  aV$DrinkerStatus[iV] = aC$DrinkerStatus[iC]
                  aV$DrinkFuture[iV] = aC$DrinkFuture[iC]
                  aV$Recovery[iV] = aC$Recovery[iC]
                  aV$Support[iV]=aC$Support[iC]
                  aV$Emotion[iV] = aC$Emotion[iC]
                }
              }
            }
          }
          
          if(is.null(dV)){
            dV = aV
          }else{
            dV = bind_rows(dV,aV)
          }
        }
        rm(aV)
      }
      
      
      #SMS dataframe------------------
      message("......SMS")
      if(file.exists(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_SMS.rds")))){
        aS = read_rds(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_SMS.rds")))
        
        if(nrow(aS)>0){
          aS$SubID = ASubID
          aS$ContactType = NA
          aS$DrankPast = NA
          aS$DrinkerStatus = NA
          aS$DrinkFuture = NA
          aS$Recovery = NA
          aS$Support=NA
          aS$Emotion = NA
          
          #merge in contacts context to voice calls
          if(nrow(aC)>0){
            for(iS in 1:nrow(aS)){
              for(iC in 1:nrow(aC)){
                # Match = any(aS$Voice_Phone[iS]==aC[iC,c("CellPhone","HomePhone",
                #                      "OtherPhone1","OtherPhone1")]) 
                Match = any(aS$Phone[iS] %in% c(aC[iC,"CellPhone"],aC[iC,"HomePhone"],
                                                aC[iC,"OtherPhone1"],aC[iC,"OtherPhone2"]))
                #if(is.na(Match)) Match = FALSE
                if(Match){
                  aS$ContactType[iS] = aC$Type[iC]
                  aS$DrankPast[iS] = aC$DrankPast[iC]
                  aS$DrinkerStatus[iS] = aC$DrinkerStatus[iC]
                  aS$DrinkFuture[iS] = aC$DrinkFuture[iC]
                  aS$Recovery[iS] = aC$Recovery[iC]
                  aS$Support[iS]=aC$Support[iC]
                  aS$Emotion[iS] = aC$Emotion[iC]
                }
              }
            }
          }
          
          if(is.null(dS)){
            dS = aS
          }else{
            dS = bind_rows(dS,aS)
          }
        }
        rm(aC)
        rm(aS)
      }
    }
  }#end For ASubID loop
  
  #Save subject specific files created in loop above ----------
  
  #save dVi
  if(!is.null(dVi)){
    dVi <- select(dVi, SubID, everything()) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>% 
      write_rds(file.path(out_path,"ds_visits.rds"))
    rm(dVi)
  }
  
  #save dD
  if(!is.null(dD)){
    dD <- select(dD, SubID, everything()) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>% 
      write_rds(file.path(out_path,"ds_dates_report.rds"))
    rm(dD)
  }
  
  #save dL
  if(!is.null(dL)){
    
    dL <- select(dL, SubID, everything()) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>% 
      write_rds(file.path(out_path,"ds_locations_report.rds"))
    rm(dL)
  }
  
  #save dG
  if(!is.null(dG)){

    dG <- select(dG, SubID, everything()) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>% 
      write_rds(file.path(out_path,"ds_gps.rds"))    
    rm(dG)  
  }
  
  #save dC
  if(!is.null(dC)){
    dC <- select(dC, -c(MonthlyVisit, StreetAddress, City, State)) %>% 
      select(SubID, UTC, everything()) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>%
      write_rds(file.path(out_path,"ds_contacts_report.rds"))
    rm(dC)
  }
  
  #save dV
  if(!is.null(dV)){
    dV = select(dV, SubID, UTC, everything()) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>%
      write_rds(file.path(out_path,"ds_voice.rds"))
    rm(dV)
  }
  
  #save dS
  if(!is.null(dS)){
    dS = select(dS, SubID, UTC, everything()) %>% 
      clean_names(case = "snake") %>% 
      rename(subid = sub_id) %>%
      write_rds(file.path(out_path,"ds_sms.rds"))
    rm(dS)
  }
}


#make_lapses-------------
#Make lapses dataframe with lapse labels and N with count of labels by SubID
make_lapses = function(dE,Period="Day", in_path)
{
  
  #Need to use EMA1_5 to screen out days after any red report
  #need to include MAM_22 and screen out any days including and after a report of No
  #need to screen out subjects who get data screened before first follow-up
  
  
  #make long sequence of days from study start to study end
  Dates=seq(as_datetime("2017-01-01 00:00:00", tz="America/Chicago"),as_datetime("2020-12-12 00:00:00", tz="America/Chicago"),by="days")
  Dates[hour(Dates)==1] = Dates[hour(Dates)==1] - hours(1) #correct for daylight savings
  Dates = as.numeric(Dates) #convert to unix time
  
  if(Period=="Day"){
    WinWidth = 60*60*24  #seconds in a day
  }else{
    WinWidth = 60*60*24*7  #seconds in a week
  }
  
  dY = data.frame(NULL) #initial empty dY
  
  #dE = dE[,c("UTC", "SubID", "EMA_1", "EMA_1.1", "EMA_1.5", "EMA_8","EMA_9","EMA_10", "Type")] #Sarah added in last 4 vars to look at expectations with lapses
  dE = dE[,c("UTC", "SubID", "EMA_1", "EMA_1.1", "EMA_1.5")]
  dE$EMA_1.1 = as.numeric(dE$EMA_1.1) #convert lapse times to unix time
  
  SubIDs = unique(dE$SubID)
  for(ASubID in SubIDs){
    dSub = filter(dE,SubID==ASubID)
    
    #Open visit dates to get start and end dates
    dVisitDates = read_rds(file.path(in_path,varPadString(ASubID,3), str_c(varPadString(ASubID,3), "_VisitDates.rds")))
    
    StudyStartDate = force_tz(as_datetime(dVisitDates$StartStudy), "America/Chicago") #default for as_datetime is UTC,  Need to force America/Chicago
    StudyStartDate = as.numeric(StudyStartDate)
    StudyEndDate = force_tz(as_datetime(dVisitDates$EndStudy), "America/Chicago") 
    StudyEndDate = as.numeric(StudyEndDate)
    
    
    #Subset dates down to relevant for this Sub
    #Start on day two b/c no features before that
    #end on second to last day for day period and 7 days before for week so full period at end
    SubDates = Dates[Dates>StudyStartDate & Dates<=(StudyEndDate-WinWidth)]
    
    #Check that there are dates for this subject
    if(length(SubDates>0)){
      dSubY = data.frame(SubID = ASubID, UTC=SubDates, Lapse=0) #make subjects dY with Lapse=0
      
      # if(Period=="Day"){
      #   dSubY$FutureIntent = NA  #only included for day level recording of lapses
      #   dSubY$LikelyRisky = NA
      #   dSubY$LikelyStress= NA
      #   dSubY$LikelyAlc = NA
      # }
      
      #Add non NA predictions of next week
      #LikelyRisky = dSub[!is.na(dSub$EMA_8), c("UTC","EMA_8")]
      #LikelyStress = dSub[!is.na(dSub$EMA_9), c("UTC","EMA_9")]
      #LikelyAlc = dSub[!is.na(dSub$EMA_10), c("UTC","EMA_10")]
      
      #Remove duplicates
      #LikelyRisky = LikelyRisky[duplicated(as.Date(as.POSIXct(LikelyRisky$UTC,origin = "1970-01-01"), tz="America/Chicago"), fromLast=TRUE)==FALSE,]
      #LikelyRisky = LikelyRisky[duplicated(as.Date(as.POSIXct(LikelyRisky$UTC,origin = "1970-01-01"), tz="America/Chicago"), fromLast=TRUE)==FALSE,]
      #LikelyRisky = LikelyRisky[duplicated(as.Date(as.POSIXct(LikelyRisky$UTC,origin = "1970-01-01"), tz="America/Chicago"), fromLast=TRUE)==FALSE,]
      
      # for (i in SubDates){
      #   if (any(LikelyStress$UTC>=i & i+WinWidth>LikelyStress$UTC)){
      #     dSubY$LikelyRisky[dSubY$UTC==i] = LikelyRisky$EMA_8[LikelyRisky$UTC>=i & i+WinWidth>LikelyRisky$UTC]
      #     dSubY$LikelyStress[dSubY$UTC==i] = LikelyStress$EMA_9[LikelyStress$EMA_9[LikelyStress$UTC>=i & i+WinWidth>LikelyStress$UTC]]
      #     dSubY$LikelyAlc[dSubY$UTC==i] = LikelyAlc$EMA_10[LikelyAlc$EMA_10[LikelyAlc$UTC>=i & i+WinWidth>LikelyAlc$UTC]]
      #   }else{
      #     dSubY$LikelyRisky[dSubY$UTC==i] = NA
      #     dSubY$LikelyStress[dSubY$UTC==i] = NA
      #     dSubY$LikelyAlc[dSubY$UTC==i] = NA
      #   }
      # 
      # }#warnings???
      
      #Update lapses to 1 for lapse events
      Lapses = dSub$EMA_1.1[!is.na(dSub$EMA_1.1)]
      # FutureIntent = dSub$EMA_1.5[!is.na(dSub$EMA_1.1)]#get future intent too
      # #When more than one lapse on same day, remove earlier events
      # #when using as.Date wrapped around as.POSIXct, need to specify tz for as.Date!! - SJS
      # LastEvents = duplicated(as.Date(as.POSIXct(Lapses,origin = "1970-01-01"), tz="America/Chicago"), fromLast=TRUE)==FALSE
      # Lapses = Lapses[LastEvents]
      # FutureIntent = FutureIntent[LastEvents]
      
      
      if(length(Lapses)>0){
        for(i in 1:length(Lapses))
        {
          dSubY$Lapse[dSubY$UTC<=Lapses[i] & dSubY$UTC+WinWidth>Lapses[i]] = 1
          #if(Period=="Day") dSubY$FutureIntent[dSubY$UTC<=Lapses[i] & dSubY$UTC+WinWidth>Lapses[i]] = FutureIntent[i]
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
      warning(str_c("No valid dates for SubID: ", ASubID))
    }
  }
  
  dY = dY[order(dY$SubID,dY$UTC),]
  
  return(dY)
}

#score_scales-----------------------------------------
#Adds scale scores to ID and SR files
score_scales = function(in_path)
{
  #ID---------------
  id =  read_rds(file.path(in_path, "ds_id.rds"))
  
  #DEM
  id$scale_dem_age <- id$dem_1
  id$scale_dem_sex <- recode(id$dem_2, `1`="male", `2`="female")
  id$scale_dem_race <- recode(id$dem_3, `1`="americanindian", `2`="asian", `3`="pacificislander",
                              `4`="black", `5`="white", `6`="other")
  id$scale_dem_hispanic <- recode(id$dem_4, `1`="no", `2`="yes", `3`="yes",
                              `4`="yes", `5`="yes")
  id$scale_dem_educ <- recode(id$dem_5, `1`="lessthanhs", `2`="hs", `3`="somecollege",
                              `4`="twoyearcollege", `5`="college", `6`="advanced")
  id$scale_dem_employ_type <- recode(id$dem_3, `1`="employed", `2`="laidoffleave", `3`="unemployed",
                              `4`="retired", `5`="disabled", `6`="homemaker", `7`="student", `8`="other")
  id$scale_dem_employed <- recode(id$dem_6_1, `1`="parttime", `2`="fulltime", .missing = "notemployed")
  id$scale_dem_income <- id$dem_7
  id$scale_dem_marital <- recode(id$dem_8, `1`="married", `2`="widowed", `3`="divorced",
                                  `4`="separated", `5`="nevermarried")
  
  #DEM2 (treating as ID not SR)
  id$scale_dem2_liveparents <- id$dem2_2
  id$scale_dem2_deadparents <- id$dem2_4
  id$scale_dem2_livekids <- id$dem2_6
  id$scale_dem2_deadkids <- id$dem2_8
  
  #AUH
  id$scale_auh_age_first <- id$auh_1
  id$scale_auh_age_regular <- id$auh_2
  id$scale_auh_age_problem <- id$auh_3
  id$scale_auh_age_firstquit <- id$auh_4
  id$scale_auh_times_quit <- id$auh_5
  
  id$scale_auh_program_longres <- recode(id$auh_6_1, `0`="no", `1`="yes")
  id$scale_auh_program_shortres <- recode(id$auh_6_2, `0`="no", `1`="yes")
  id$scale_auh_program_outpatient <- recode(id$auh_6_3, `0`="no", `1`="yes")
  id$scale_auh_program_individual <- recode(id$auh_6_4, `0`="no", `1`="yes")
  id$scale_auh_program_group <- recode(id$auh_6_5, `0`="no", `1`="yes")
  id$scale_auh_program_selfhelp <- recode(id$auh_6_6, `0`="no", `1`="yes")
  id$scale_auh_program_other <- recode(id$auh_6_7, `0`="no", `1`="yes")
  id$scale_auh_program_total <- varScore(id, Forward = str_c("auh_6_", 1:7), Range = c(0,1))
  
  id$scale_auh_alcmeds <- recode(id$auh_7, `1`="no", `2`="yes")
  id$scale_auh_freqdrink_sixmonths <- id$auh_9
  id$scale_auh_freqbinge_sixmonths <- id$auh_10
  id$scale_auh_quantdrink_sixmonths <- id$auh_11
  id$scale_auh_freqdrink_heaviest <- id$auh_12
  id$scale_auh_freqbinge_heaviest <- id$auh_13
  id$scale_auh_quantdrink_heaviest <- id$auh_14
  
  #DSM
  id$scale_dsm5_tot = varScore(id, Forward= str_c("dsm5_", 1:11), Range = c(1,2)) - 11
  
  #YAP
  yap_life = id[,str_c("yap_", 1:27)] 
  yap_year = yap_life 
  yap_life$yap_1  = varRecode(yap_life$yap_1, 1:10, c(0,1,1,1,1,1,1,1,1,1))
  yap_life$yap_2  = varRecode(yap_life$yap_2, 1:10, c(0,1,1,1,1,1,1,1,1,1))
  yap_life$yap_3  = varRecode(yap_life$yap_3, 1:10, c(0,1,1,1,1,1,1,1,1,1))
  yap_life$yap_4  = varRecode(yap_life$yap_4, 1:10, c(0,1,1,1,1,1,1,1,1,1))
  yap_life$yap_5  = varRecode(yap_life$yap_5, 1:10, c(0,1,1,1,1,1,1,1,1,1))
  yap_life$yap_6  = varRecode(yap_life$yap_6, 1:10, c(0,1,1,1,1,1,1,1,1,1))
  yap_life$yap_7  = varRecode(yap_life$yap_7, 1:10, c(0,1,1,1,1,1,1,1,1,1))
  yap_life$yap_8  = varRecode(yap_life$yap_8, 1:10, c(0,1,1,1,1,1,1,1,1,1))
  yap_life$yap_9  = varRecode(yap_life$yap_9, 1:5, c(0,1,1,1,1))
  yap_life$yap_10  = varRecode(yap_life$yap_10, 1:5, c(0,1,1,1,1))
  yap_life$yap_11  = varRecode(yap_life$yap_11, 1:5, c(0,1,1,1,1))
  yap_life$yap_12  = varRecode(yap_life$yap_12, 1:5, c(0,1,1,1,1))
  yap_life$yap_13  = varRecode(yap_life$yap_13, 1:5, c(0,1,1,1,1))
  yap_life$yap_14  = varRecode(yap_life$yap_14, 1:5, c(0,1,1,1,1))
  yap_life$yap_15  = varRecode(yap_life$yap_15, 1:5, c(0,1,1,1,1))
  yap_life$yap_16  = varRecode(yap_life$yap_16, 1:5, c(0,1,1,1,1))
  yap_life$yap_17  = varRecode(yap_life$yap_17, 1:5, c(0,1,1,1,1))
  yap_life$yap_18  = varRecode(yap_life$yap_18, 1:5, c(0,1,1,1,1))
  yap_life$yap_19  = varRecode(yap_life$yap_19, 1:5, c(0,1,1,1,1))
  yap_life$yap_20  = varRecode(yap_life$yap_20, 1:5, c(0,1,1,1,1))
  yap_life$yap_21  = varRecode(yap_life$yap_21, 1:3, c(0,1,1))
  yap_life$yap_22  = varRecode(yap_life$yap_22, 1:3, c(0,1,1))
  yap_life$yap_23  = varRecode(yap_life$yap_23, 1:3, c(0,1,1))
  yap_life$yap_24  = varRecode(yap_life$yap_24, 1:3, c(0,1,1))
  yap_life$yap_25  = varRecode(yap_life$yap_25, 1:3, c(0,1,1))
  yap_life$yap_26  = varRecode(yap_life$yap_26, 1:3, c(0,1,1))
  yap_life$yap_27  = varRecode(yap_life$yap_27, 1:3, c(0,1,1))
  id$scale_yap_life = varScore(yap_life, Forward= str_c("yap_", 1:27), Range = c(0,1))
  rm(yap_life)
  
  yap_year$yap_1  = varRecode(yap_year$yap_1, 1:10, c(0,0,1,1,1,1,1,1,1,1))
  yap_year$yap_2  = varRecode(yap_year$yap_2, 1:10, c(0,0,1,1,1,1,1,1,1,1))
  yap_year$yap_3  = varRecode(yap_year$yap_3, 1:10, c(0,0,1,1,1,1,1,1,1,1))
  yap_year$yap_4  = varRecode(yap_year$yap_4, 1:10, c(0,0,1,1,1,1,1,1,1,1))
  yap_year$yap_5  = varRecode(yap_year$yap_5, 1:10, c(0,0,1,1,1,1,1,1,1,1))
  yap_year$yap_6  = varRecode(yap_year$yap_6, 1:10, c(0,0,1,1,1,1,1,1,1,1))
  yap_year$yap_7  = varRecode(yap_year$yap_7, 1:10, c(0,0,1,1,1,1,1,1,1,1))
  yap_year$yap_8  = varRecode(yap_year$yap_8, 1:10, c(0,0,1,1,1,1,1,1,1,1))
  yap_year$yap_9  = varRecode(yap_year$yap_9, 1:5, c(0,0,1,1,1))
  yap_year$yap_10  = varRecode(yap_year$yap_10, 1:5, c(0,0,1,1,1))
  yap_year$yap_11  = varRecode(yap_year$yap_11, 1:5, c(0,0,1,1,1))
  yap_year$yap_12  = varRecode(yap_year$yap_12, 1:5, c(0,0,1,1,1))
  yap_year$yap_13  = varRecode(yap_year$yap_13, 1:5, c(0,0,1,1,1))
  yap_year$yap_14  = varRecode(yap_year$yap_14, 1:5, c(0,0,1,1,1))
  yap_year$yap_15  = varRecode(yap_year$yap_15, 1:5, c(0,0,1,1,1))
  yap_year$yap_16  = varRecode(yap_year$yap_16, 1:5, c(0,0,1,1,1))
  yap_year$yap_17  = varRecode(yap_year$yap_17, 1:5, c(0,0,1,1,1))
  yap_year$yap_18  = varRecode(yap_year$yap_18, 1:5, c(0,0,1,1,1))
  yap_year$yap_19  = varRecode(yap_year$yap_19, 1:5, c(0,0,1,1,1))
  yap_year$yap_20  = varRecode(yap_year$yap_20, 1:5, c(0,0,1,1,1))
  yap_year$yap_21  = varRecode(yap_year$yap_21, 1:3, c(0,0,1))
  yap_year$yap_22  = varRecode(yap_year$yap_22, 1:3, c(0,0,1))
  yap_year$yap_23  = varRecode(yap_year$yap_23, 1:3, c(0,0,1))
  yap_year$yap_24  = varRecode(yap_year$yap_24, 1:3, c(0,0,1))
  yap_year$yap_25  = varRecode(yap_year$yap_25, 1:3, c(0,0,1))
  yap_year$yap_26  = varRecode(yap_year$yap_26, 1:3, c(0,0,1))
  yap_year$yap_27  = varRecode(yap_year$yap_27, 1:3, c(0,0,1))
  id$scale_yap_year = varScore(yap_year, Forward= str_c("yap_", 1:27), Range = c(0,1))
  rm(yap_year)
  
  #WHO
  #These are the WHO-Assist questions that were completed ONLY at baseline (screen)
  #questions 2-3 are part of SR
  id$assist_1_all = varScore(id, Forward = str_c("assist_1_", 1:8), Range = c(1,2) ) - 8
  
  assist = id[,str_c("assist_4_", 1:8)]
  assist$assist_4_1 = varRecode(assist$assist_4_1,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_4_2 = varRecode(assist$assist_4_2,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_4_3 = varRecode(assist$assist_4_3,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_4_4 = varRecode(assist$assist_4_4,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_4_5 = varRecode(assist$assist_4_5,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_4_6 = varRecode(assist$assist_4_6,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_4_7 = varRecode(assist$assist_4_7,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_4_8 = varRecode(assist$assist_4_8,c(1,2,3,4,5),c(0,1.5,3,15,90))
  id$assist_4_all = varScore(assist, Forward=str_c("assist_4_", 1:8), Range = c(0,90))
  rm(assist)
  
  assist = id[,str_c("assist_5_", 1:8)]
  assist$assist_5_1 = varRecode(assist$assist_5_1,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_5_2 = varRecode(assist$assist_5_2,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_5_3 = varRecode(assist$assist_5_3,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_5_4 = varRecode(assist$assist_5_4,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_5_5 = varRecode(assist$assist_5_5,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_5_6 = varRecode(assist$assist_5_6,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_5_7 = varRecode(assist$assist_5_7,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_5_8 = varRecode(assist$assist_5_8,c(1,2,3,4,5),c(0,1.5,3,15,90))
  id$scale_assist_5_all = varScore(assist, Forward=str_c("assist_5_", 1:8), Range = c(0,90) )
  rm(assist)
  
  assist = id[,str_c("assist_6_", 1:8)]
  assist$assist_6_1 = varRecode(assist$assist_6_1,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_6_2 = varRecode(assist$assist_6_2,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_6_3 = varRecode(assist$assist_6_3,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_6_4 = varRecode(assist$assist_6_4,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_6_5 = varRecode(assist$assist_6_5,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_6_6 = varRecode(assist$assist_6_6,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_6_7 = varRecode(assist$assist_6_7,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_6_8 = varRecode(assist$assist_6_8,c(1,2,3,4,5),c(0,1.5,3,15,90))
  id$scale_assist_6_all = varScore(assist, Forward=str_c("assist_6_", 1:8), Range = c(0,90) )
  rm(assist)
  
  assist = id[,str_c("assist_7_", 1:8)]
  assist$assist_7_1 = varRecode(assist$assist_7_1,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_7_2 = varRecode(assist$assist_7_2,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_7_3 = varRecode(assist$assist_7_3,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_7_4 = varRecode(assist$assist_7_4,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_7_5 = varRecode(assist$assist_7_5,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_7_6 = varRecode(assist$assist_7_6,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_7_7 = varRecode(assist$assist_7_7,c(1,2,3,4,5),c(0,1.5,3,15,90))
  assist$assist_7_8 = varRecode(assist$assist_7_8,c(1,2,3,4,5),c(0,1.5,3,15,90))
  id$scale_assist_7_all = varScore(assist, Forward=str_c("assist_7_", 1:8), Range = c(0,90) )
  rm(assist)
  
  id$scale_assist_8_inject <- recode(id$assist_8, `2` = 3, `3` = 2)  #recoded to order no, yes but not past 3 months, yes and past 3 months
  
  #SCL-90
  id$scale_scl90_tot = varScore(id, Forward= str_c("scl90_", 1:90), Range = c(1,5) ) - 90
  id$scale_scl90_som = varScore(id, Forward= c("scl90_42", "scl90_52", "scl90_58", "scl90_56", "scl90_12", "scl90_49", "scl90_27", "scl90_48", "scl90_4", "scl90_53", "scl90_1", "scl90_40"), Range = c(1,5) ) - 12
  id$scale_scl90_oc = varScore(id, Forward= c("scl90_10", "scl90_28", "scl90_3", "scl90_38", "scl90_45", "scl90_46", "scl90_51", "scl90_55", "scl90_65", "scl90_9"), Range = c(1,5) ) - 10
  id$scale_scl90_sens = varScore(id, Forward= c("scl90_21", "scl90_34", "scl90_36", "scl90_37", "scl90_41", "scl90_6", "scl90_61", "scl90_69", "scl90_73"), Range = c(1,5) ) - 9
  id$scale_scl90_dep = varScore(id, Forward= c("scl90_14", "scl90_15", "scl90_20", "scl90_22", "scl90_26", "scl90_29", "scl90_30", "scl90_31", "scl90_32", "scl90_5", "scl90_54", "scl90_71", "scl90_79"), Range = c(1,5) ) - 13
  id$scale_scl90_anx = varScore(id, Forward= c("scl90_17", "scl90_2", "scl90_23", "scl90_33", "scl90_39", "scl90_57", "scl90_72", "scl90_78", "scl90_80", "scl90_86"), Range = c(1,5) ) - 10
  id$scale_scl90_ang = varScore(id, Forward= c("scl90_11", "scl90_24", "scl90_63", "scl90_67", "scl90_74", "scl90_81"), Range = c(1,5) ) - 6
  id$scale_scl90_pho = varScore(id, Forward= c("scl90_50", "scl90_13", "scl90_25", "scl90_47", "scl90_70", "scl90_75", "scl90_82"), Range = c(1,5) ) - 7
  id$scale_scl90_par = varScore(id, Forward= c("scl90_18", "scl90_43", "scl90_68", "scl90_76", "scl90_8", "scl90_83"), Range = c(1,5) ) - 6
  id$scale_scl90_psy = varScore(id, Forward= c("scl90_62", "scl90_16", "scl90_35", "scl90_7", "scl90_77", "scl90_84", "scl90_85", "scl90_87", "scl90_88", "scl90_90"), Range = c(1,5) ) - 10
  
  #IUS
  id$scale_ius_tot = varScore(id, Forward= str_c("ius_", 1:27), Range = c(1,5) )
  
  #ASI
  id$scale_asi3_pc = varScore(id, Forward= c("asi3_4", "asi3_12", "asi3_8", "asi3_7", "asi3_15", "asi3_3"), Range = c(1,5) ) - 6
  id$scale_asi3_cc = varScore(id, Forward= c("asi3_14", "asi3_18", "asi3_10", "asi3_16", "asi3_2", "asi3_5"), Range = c(1,5) ) - 6
  id$scale_asi3_sc = varScore(id, Forward= c("asi3_9", "asi3_6", "asi3_11", "asi3_13", "asi3_17", "asi3_1"), Range = c(1,5) ) - 6
  id$scale_asi3_tot = varScore(id, Forward= str_c("asi3_", 1:18), Range = c(1,5) ) - 18
  
  #DTS
  id$scale_dts_toler = varScore(id, Forward= c(), Reverse = c("dts_1", "dts_3", "dts_5"), Range = c(1,5)  ) / 3
  id$scale_dts_absorb = varScore(id, Forward= c(), Reverse = c("dts_2", "dts_4", "dts_15"), Range = c(1,5)  ) / 3
  id$scale_dts_appraise = varScore(id, Forward = c("dts_6"), Reverse = c("dts_7", "dts_9", "dts_10", "dts_11", "dts_12"), Range = c(1,5)  ) / 6
  id$scale_dts_regulate = varScore(id, Forward= c(), Reverse = c("dts_8", "dts_13", "dts_14"), Range = c(1,5)  ) / 3
  id$scale_dts_tot = (id$scale_dts_toler + id$scale_dts_absorb + id$scale_dts_appraise + id$scale_dts_regulate ) / 4
  
  #FAD
  id$scale_fad_prob = varScore(id, Forward= c("fad_2", "fad_12", "fad_24", "fad_38", "fad_50", "fad_60"), Range = c(1,4) )
  id$scale_fad_comm = varScore(id, Forward= c("fad_3", "fad_18", "fad_29", "fad_43", "fad_59"), Reverse= c("fad_14", "fad_22", "fad_35", "fad_52"), Range = c(1,4) )
  id$scale_fad_role = varScore(id, Forward= c("fad_10", "fad_30", "fad_40"), Reverse= c("fad_4", "fad_8", "fad_15", "fad_23", "fad_34", "fad_45", "fad_53", "fad_58"), Range = c(1,4) )
  id$scale_fad_resp = varScore(id, Forward= c("fad_49", "fad_57"), Reverse= c("fad_9", "fad_19", "fad_28", "fad_39"), Range = c(1,4) )
  id$scale_fad_inv = varScore(id, Forward= c(), Reverse= c("fad_5", "fad_13", "fad_25", "fad_33", "fad_37", "fad_42", "fad_54"), Range = c(1,4) )
  id$scale_fad_beh = varScore(id, Forward= c("fad_20", "fad_32", "fad_55"), Reverse= c("fad_7", "fad_17", "fad_27", "fad_44", "fad_47", "fad_48"), Range = c(1,4) )
  id$scale_fad_gen = varScore(id, Forward= c("fad_6", "fad_16", "fad_26", "fad_36", "fad_46", "fad_56"), Reverse= c("fad_1", "fad_11", "fad_21", "fad_31", "fad_41", "fad_51"), Range = c(1,4) )
  
  #MPS
  id$scale_mps_wb = varScore(id, Forward = c("mps_1", "mps_26", "mps_38", "mps_50", "mps_62",  "mps_74", "mps_85", "mps_97", "mps_109","mps_121", "mps_133","mps_144"), Range = c(1,2) ) - 12
  id$scale_mps_sp = varScore(id, Forward = c("mps_2", "mps_15", "mps_39", "mps_51", "mps_75",  "mps_87", "mps_110"), Reverse = c("mps_63", "mps_98", "mps_122", "mps_134","mps_145"), Range = c(1,2) ) - 12
  id$scale_mps_ac = varScore(id, Forward = c("mps_3", "mps_16", "mps_27", "mps_52", "mps_76",  "mps_88", "mps_111", "mps_123","mps_135","mps_146"), Reverse = c("mps_64", "mps_99"), Range = c(1,2) ) - 12
  id$scale_mps_sc = varScore(id, Forward = c("mps_5", "mps_40", "mps_77", "mps_112"), Reverse = c("mps_17", "mps_28", "mps_65", "mps_89", "mps_100","mps_124", "mps_136","mps_148" ), Range = c(1,2) ) - 12
  id$scale_mps_sr = varScore(id, Forward = c("mps_6", "mps_18", "mps_29", "mps_41", "mps_53",  "mps_78", "mps_90", "mps_101", "mps_113","mps_125", "mps_137","mps_149"), Range = c(1,2) ) - 12
  id$scale_mps_al = varScore(id, Forward = c("mps_7", "mps_19", "mps_30", "mps_42", "mps_54", "mps_66", "mps_91", "mps_102","mps_114", "mps_126", "mps_138","mps_150"), Range = c(1,2) ) - 12
  id$scale_mps_ag = varScore(id, Forward = c("mps_8", "mps_20", "mps_31", "mps_43", "mps_55",  "mps_67", "mps_103", "mps_115","mps_127","mps_139", "mps_151"), Reverse = c("mps_79"), Range = c(1,2) ) - 12
  id$scale_mps_ct = varScore(id, Forward = c("mps_9", "mps_44", "mps_56", "mps_68", "mps_92",  "mps_116","mps_128", "mps_140"), Reverse = c("mps_21", "mps_33", "mps_80", "mps_152" ), Range = c(1,2) ) - 12
  id$scale_mps_ha = varScore(id, Forward = c("mps_57", "mps_141"), Reverse = c("mps_11", "mps_22", "mps_34", "mps_46", "mps_69", "mps_81", "mps_93", "mps_105", "mps_129",  "mps_153"), Range = c(1,2) ) - 12
  id$scale_mps_td = varScore(id, Forward = c("mps_12", "mps_23", "mps_35", "mps_58", "mps_82", "mps_94", "mps_106","mps_142","mps_154"), Reverse = c("mps_47", "mps_70", "mps_118"), Range = c(1,2) ) - 12
  id$scale_mps_ab = varScore(id, Forward = c("mps_13", "mps_24", "mps_36", "mps_48", "mps_59",  "mps_71", "mps_83", "mps_95", "mps_107","mps_119",  "mps_130","mps_155"), Range = c(1,2) ) - 12
  id$scale_mps_uv = varScore(id, Forward = c("mps_25", "mps_49", "mps_72", "mps_96", "mps_120", "mps_143", "mps_147"), Reverse= c( "mps_4", "mps_14", "mps_37", "mps_61", "mps_84",  "mps_108","mps_131"), Range = c(1,2) ) - 12
  id$scale_mps_pag = round((id$scale_mps_wb * 1.529) + (id$scale_mps_sp * 1.294) + (id$scale_mps_ac * 3.211) + (id$scale_mps_sc * -0.317) + (id$scale_mps_sr * -0.112) + (id$scale_mps_al * -0.085) + (id$scale_mps_ag * 0.063) + (id$scale_mps_ct * 0.154) + (id$scale_mps_ha * -0.186) + (id$scale_mps_td * 0.02) + 18.448)
  id$scale_mps_pco = round((id$scale_mps_wb * 1.582) + (id$scale_mps_sp * 1.387) + (id$scale_mps_ac * -0.51) + (id$scale_mps_sc * 3.411) + (id$scale_mps_sr * 0.048) + (id$scale_mps_al * 0.017) + (id$scale_mps_ag * 0.059) + (id$scale_mps_ct * -0.068) + (id$scale_mps_ha * 0.205) + (id$scale_mps_td * 0.097) + 16.804)
  id$scale_mps_nag = round((id$scale_mps_wb * 0.042) + (id$scale_mps_sp * 0.111) + (id$scale_mps_ac * -0.036) + (id$scale_mps_sc * -0.07) + (id$scale_mps_sr * 1.721) + (id$scale_mps_al * -0.885) + (id$scale_mps_ag * 5.26) + (id$scale_mps_ct * 0.106) + (id$scale_mps_ha * 0.13) + (id$scale_mps_td * 0.057) + 22.739)
  id$scale_mps_nal = round((id$scale_mps_wb * -0.043) + (id$scale_mps_sp * -0.072) + (id$scale_mps_ac * 0.059) + (id$scale_mps_sc * 0.206) + (id$scale_mps_sr * 1.389) + (id$scale_mps_al * 5.398) + (id$scale_mps_ag * -0.695) + (id$scale_mps_ct * -0.114) + (id$scale_mps_ha * -0.025) + (id$scale_mps_td * 0.089) + 20.341)
  id$scale_mps_pem = round((id$scale_mps_wb * 1.933) + (id$scale_mps_sp * 1.669) + (id$scale_mps_ac * 1.671) + (id$scale_mps_sc * 1.950) + (id$scale_mps_sr * 0.085) + (id$scale_mps_al * 0.292) + (id$scale_mps_ag * 0.130) + (id$scale_mps_ct * 0.048) + (id$scale_scale_mps_ha * 0.015) + (id$scale_mps_td * 0.070) + 13.712 )
  id$scale_mps_nem = round((id$scale_mps_wb * 0.127) + (id$scale_mps_sp * 0.150) + (id$scale_mps_ac * 0.038) + (id$scale_mps_sc * 0.279) + (id$scale_mps_sr * 1.904) + (id$scale_mps_al * 3.061) + (id$scale_mps_ag * 2.551) + (id$scale_mps_ct * 0.045) + (id$scale_mps_ha * 0.126) + (id$scale_mps_td * 0.147) + 6.270 )
  id$scale_mps_con = round((id$scale_mps_wb * -0.085) + (id$scale_mps_sp * -0.052) + (id$scale_mps_ac * 0.241) + (id$scale_mps_sc * -0.068) + (id$scale_mps_sr * 0.046) + (id$scale_mps_al * -0.302) + (id$scale_mps_ag * 0.296) + (id$scale_mps_ct * 2.717) + (id$scale_mps_ha * 2.579) + (id$scale_mps_td * 2.199) + 20.742 )
  
  
  write_rds(id, file.path(in_path, "ds_id.rds"))
  rm(id)  
  

  #SR --------------------------
  sr = read_rds(file.path(in_path, "ds_sr.rds"))
  
  #Penn Alcohol Craving Scale (PACS)
  sr$scale_pacs_tot = varScore(sr, Forward= c("pacs_1", "pacs_2", "pacs_3", "pacs_4", "pacs_5"), Range = c(1,7) ) - 5
  
  #Alcohol Abstinence Confidence (AASE)
  sr$scale_aase_na = varScore(sr, Forward= c("aase_18", "aase_16", "aase_3", "aase_14", "aase_6"), Range = c(1,5) )-5
  sr$scale_aase_soc = varScore(sr, Forward= c("aase_15", "aase_20", "aase_4", "aase_17", "aase_8"), Range = c(1,5) )-5
  sr$scale_aase_phys = varScore(sr, Forward= c("aase_2", "aase_12", "aase_5", "aase_13", "aase_9"), Range = c(1,5) )-5
  sr$scale_aase_crav = varScore(sr, Forward=c("aase_1", "aase_7", "aase_11", "aase_10", "aase_19"), Range = c(1,5) )-5
  
  #MAM
  sr$scale_mam_housing <- recode(sr$mam_1, `1`="independent", `2`="homelessshelter", `3`="unsheltered",`4`="other")
  #currently ignoring follow-ups to mam_1
  
  sr$scale_mam_psychhosp <- recode(sr$mam_2, `1`="no", `2`="yes")
  sr$scale_mam_arrest <- recode(sr$mam_3, `1`="no", `2`="yes")
  sr$scale_mam_jail <- recode(sr$mam_4, `1`="no", `2`="yes")
  sr$scale_mam_violentcrime <- recode(sr$mam_5, `1`="no", `2`="yes")
  sr$scale_mam_nonviolentcrime <- recode(sr$mam_6, `1`="no", `2`="yes")
  sr$scale_mam_aodtxt <- recode(sr$mam_7, `1`="no", `2`="yes")
  sr$scale_mam_numindcounsel <- sr$mam_8
  sr$scale_mam_numgrpcounsel <- sr$mam_9
  sr$scale_mam_numselfhelp <- sr$mam_10
  sr$scale_mam_othercounsel <- recode(sr$mam_11, `1`="no", `2`="yes")
  sr$scale_mam_numothercounse <- sr$mam_12
  sr$scale_mam_numdayssupport <- sr$mam_13
  sr$scale_mam_numdaysnotsupport <- sr$mam_14
  sr$scale_mam_numdaysworkschoolvol <- sr$mam_15
  sr$scale_mam_religonassupport <- sr$mam_16
  sr$scale_mam_alcmeduse <- recode(sr$mam_11, `1`="no", `2`="yes")
  
  
  #WHO Assist 2-3 (items that are repeated monthly)
  assist = sr[,str_c("assist_2_", 1:8)]
  assist$assist_2_1 = varRecode(assist$assist_2_1,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_2_2 = varRecode(assist$assist_2_2,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_2_3 = varRecode(assist$assist_2_3,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_2_4 = varRecode(assist$assist_2_4,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_2_5 = varRecode(assist$assist_2_5,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_2_6 = varRecode(assist$assist_2_6,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_2_7 = varRecode(assist$assist_2_7,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_2_8 = varRecode(assist$assist_2_8,c(1,2,3,4),c(0,1.5,5,30))
  sr$scale_assist_2_all = varScore(assist, Forward=str_c("assist_2_", 1:8), Range = c(0,30))
  rm(assist)
  
  assist = sr[,str_c("assist_3_", 1:8)]
  assist$assist_3_1 = varRecode(assist$assist_3_1,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_3_2 = varRecode(assist$assist_3_2,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_3_3 = varRecode(assist$assist_3_3,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_3_4 = varRecode(assist$assist_3_4,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_3_5 = varRecode(assist$assist_3_5,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_3_6 = varRecode(assist$assist_3_6,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_3_7 = varRecode(assist$assist_3_7,c(1,2,3,4),c(0,1.5,5,30))
  assist$assist_3_8 = varRecode(assist$assist_3_8,c(1,2,3,4),c(0,1.5,5,30))
  sr$scale_assist_AllUrge = varScore(assist, Forward=str_c("assist_3_", 1:8), Range = c(0,30))
  rm(assist)
  
  #Depression, anxiety, stress scales (DASS)
  sr$scale_dass21_anx = (varScore(sr, Forward= c("dass21_2", "dass21_4", "dass21_7", "dass21_9", "dass21_15", "dass21_19", "dass21_20"), Range = c(1,4) ) - 7) * 2
  sr$scale_dass21_dep = (varScore(sr, Forward= c("dass21_3", "dass21_5", "dass21_10", "dass21_13", "dass21_16", "dass21_17", "dass21_21"), Range = c(1,4) ) - 7) * 2
  sr$scale_dass21_str = (varScore(sr, Forward= c("dass21_1", "dass21_6", "dass21_8", "dass21_11", "dass21_12", "dass21_14", "dass21_18"), Range = c(1,4) ) - 7) *2
  sr$scale_dass21_tot = (varScore(sr, Forward= str_c("dass21_", 1:21), Range = c(1,4) ) + 21) * 2
  
  #Perceived stress scale (PSS)
  sr$scale_pss_tot = varScore(sr, Forward= c("pss_1", "pss_2", "pss_3", "pss_6", "pss_9", "pss_10"), Reverse= c("pss_4", "pss_5", "pss_7", "pss_8"), Range = c(1,5) ) - 10
  
  #Quality of Life (QOL)
  sr$scale_qol_tot = varScore(sr, Forward= str_c("qol_", 1:17), Range = c(1,5) )
  
  #DAS
  #handled reverse score here b/c range of items differ
  das <- select(sr, contains("das_")) %>% 
    mutate_at(str_c("das_", c(16,17,20:22)), ~recode(., `1`=6, `2`=5, `3`=4, `4`=3, `5`=2, `6`=1)) %>% 
    mutate_at(str_c("das_", c(29,30)), ~recode(., `1`=2, `2`=1)) 
  
  sr$scale_das_con = varScore(das, Forward= c("das_1", "das_2", "das_3", "das_5", "das_7", "das_8", "das_9", "das_10", "das_11", "das_12", "das_13", "das_14", "das_15"), Range = c(1,6) )
  sr$scale_das_sat = varScore(das, Forward= c("das_16", "das_17", "das_18", "das_19", "das_20", "das_21", "das_22", "das_23", "das_31", "das_32"), Range = c(1,7) )
  sr$scale_das_coh = varScore(das, Forward= c("das_24", "das_25", "das_26", "das_27", "das_28"), Range = c(1,6) )
  sr$scale_das_aff = varScore(das, Forward= c("das_4", "das_6", "das_29", "das_30"), Range = c(1,6))
  sr$scale_das_tot = varScore(das, Forward= str_c("das_", c(1:32)), Range = c(1,7))
  rm(das)
  
  #Perceived support (MSPSS)
  sr$scale_mspss_fam = varScore(sr, Forward= c("mspss_3", "mspss_4", "mspss_8", "mspss_11"), Range = c(1,7) )
  sr$scale_mspss_fri = varScore(sr, Forward= c("mspss_6", "mspss_7", "mspss_9", "mspss_12"), Range = c(1,7) )
  sr$scale_mspss_so = varScore(sr, Forward= c("mspss_1", "mspss_2", "mspss_5", "mspss_10"), Range = c(1,7) )
 
  write_rds(sr, file.path(in_path, "ds_sr.rds"))
  rm(sr)  
}

