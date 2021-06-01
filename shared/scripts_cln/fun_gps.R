#Functions for manipulation of GPS data

# Required packages -------------------------------------------------------
library(plotKML)    #readGPX
library(stringr)    #str_c
library(geosphere)  #distGeo
library(leaflet)    #leaflet
library(tidyverse)  #bind_rows(), tibble()
library(jsonlite)   #fromJSON()
library(httr)       #GET (for API access to FollowMee)
library(purrr)      #flatten() and other list work
library(lubridate)  #as_datetime
library(janitor)

# install.packages('P:/Methods/Software/R/Packages/Source/ggmap.zip', repos=NULL, type= 'source') #2.7.9 installer
library(ggmap)  #geocode, revgeocode, register_google (which requires 2.7.9)
register_google(key = 'AIzaSyBBwIG1tC4QZi5QrTnhXXLktIPD8NlVVI0') #API key to override query limit

# Function: gps_load_moves()------------------------------------------------------
gps_load_moves <-  function(file_name) {
  
  d <- readGPX(file_name,
               metadata = FALSE, bounds = FALSE, 
               waypoints = TRUE, tracks = TRUE, routes = FALSE) %>%
    .$tracks %>% 
    purrr::flatten(.) %>% # as a list of dfs
    enframe(.) %>% # as a nested df
    unnest(keep_empty = TRUE) %>% # as one tibble
    rename(long = lon) #renaming for coherence
  

  if (any(is.na(d$time) & !(is.na(d$lat) & is.na(d$long)))) stop("Missing value detected for time on non-missing day") 
  
  # drop initial missing obs and impute time for missing obs and 
  # fill in the last second of the next day;  Retain time zone from previous observation
  d <- d %>% 
    fill(time) %>% 
    mutate(time = if_else(is.na(long), 
                          str_c(lubridate::mdy(name), 
                                "T23:59:59.000-", 
                                str_extract(time, "\\d\\d:\\d\\d$")),
                          time)) %>% 
    select(-name)

  return(d)  
}


# Function: gps_plot())------------------------------------------------------

gps_plot = function(d, gap='adaptive') 
{
  #Places dataframe
  d_p <- filter(d, type =='place')
  
  #make popuplabel
  index <- ifelse(rep('index' %in% colnames(d_p),nrow(d_p)),d_p$index,
                  rownames(d_p))
  
  d_p$pop_up_label = str_c(
    d_p$next_time, ' mins on ', 
    weekdays(d_p$time), ' ',
    format(d_p$time, format= '%b-%d-%Y %I:%M:%S %p '), 
    ' at ', d_p$lat, ' ', d_p$lon, 
    ' (',index , ')'
  )
  
  #Moving (tracks) dataframe
  
  d_m <- filter(d, type !='place', type != "off")
  
  m <-  leaflet() %>% 
    addTiles(group='Color') %>% # Add default OpenStreetMap map tiles
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group='B&W') #B&W OSM tiles
  
  #Put tracks on first  
  types <- c('WALKING', 'RUNNING', 'CYCLING', 'TRANSPORT', 'AIRPLANE', 'NULL')
  i=1
  while(i<nrow(dM)) #need at least two points left for a new track
  {
    d_t <- tibble(lat = double(),long = double())  #make new empty track
    type = d_m$type[i]
    j=1 #add first trackpoint in current empty track
    d_t[j,'lat'] = d_m$lat[i]
    d_t[j,'long'] = d_m$long[i]
    
    #Set max allowable sampling Gap (in minutes) based on move Type c('walking', 'running', 'cycling', 'transport', 'airplane')
    gap = switch(which(type == types), 1.25,1,1,.75,2, 0)  #0 is for null type
    
    while(
      i<nrow(d_m) && 
      !is.na(d_n$next_time[i]) && 
      d_m$next_time[i] <= gap && 
      d_m$type[i+1] == type
    )
    {
      i <- i+1
      j <- j+1
      d_t[j,'lat'] <- d_m$lat[i]
      d_t[j,'long'] <- d_m$long[i]
    }
    
    #display this track if it has at least 2 points
    if(nrow(d_t)>1)
    {
      color <- switch(which(type == types), 'purple', 'blue', 'cyan', 'yellow', 'orange', 'red')
      
      m <- addPolylines(m, data = d_t, 
                        lng = ~long, lat = ~lat, 
                        color = color, weight = 2, 
                        opacity = 1, group = 'tracks')
    }
    
    i = i+1  #advance to next trackpoint (first in next track)
  }
  
  #Add places
  m <- addCircleMarkers(m, data = d_p, 
                        lng = ~long, lat = ~lat, 
                        radius = 1.5, color = 'red', opacity = 1, 
                        popup = ~pop_up_label, group = 'places') %>% 
    addLayersControl(baseGroups = c('color', 'B&W'),
                     overlayGroups = c('places', 'tracks'), 
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    addMeasure(position = "bottomleft", 
               primaryLengthUnit = "meters", 
               primaryAreaUnit = "sqmeters") %>% 
    addLegend("bottomright", 
              colors = c('purple', 'blue', 'cyan', 'yellow', 'orange', 'red'),
              labels = c('walking', 'runnng', 'cycling', 
                         'transport', 'airplane', 'place'),
              title = 'Legend',opacity = 1, layerId = 'legend')
  return(m)
}


# Function: gpsNearestReference(dTarget, dReference)------------------------------------------
#Calculates the distance in meters for each location in dTarget
#to the closest location in dReference
#Adds NearestReference to dTarget

#JJC:  Dont tidy yet

gpsNearestReference = function(dTarget, dReference, MaxDistanceForMatch=0) 
{
  dTarget$NearestReference = as.double(NA)
  
  for(i in 1:nrow(dTarget))
  {
    if(!is.na(dTarget$Type[i]) && toupper(dTarget$Type[i])=='PLACE')
    {
      for(j in 1:nrow(dReference))
      {
        Distance = round(distGeo(dTarget[i,c('Long', 'Lat')], dReference[j,c('Long', 'Lat')]),1)
        if (is.na(dTarget$NearestReference[i]) || Distance < dTarget$NearestReference[i]) dTarget$NearestReference[i] = Distance
      }
    }
  }

  return(dTarget)
}

#gpsCountMatchs = function(d,Radius)----------------------------

#JJC:  Dont tidy yet


gpsCountMatchs = function(d,Radius)
{
  d$NumMatchs = NA
  d$NumMatchs[toupper(d$Type)=='PLACE'] = 0
  
  Indices = which(toupper(d$Type) =='PLACE')
  for (i in Indices)
  {
    for (j in Indices)
    {
      Distance = round(distGeo(d[i,c('Long', 'Lat')], d[j,c('Long', 'Lat')]),1)
      if(Distance<=Radius) d$NumMatchs[i] = d$NumMatchs[i] + 1
    }
  }
  return(d)
}

#Function: gpsExtractPlace()------------------------------------------------------

#JJC:  Dont tidy yet

gpsExtractPlace = function(d,Radius,Lat=NULL, Lon=NULL, Address = NULL)
{
  d = d[toupper(d$Type)=='PLACE',]
  if(!is.null(Address)) #Get Lat/Lon if address provided
  {
    LonLat = geocode(Address, output = 'latlon', source = 'google')
    Lon = LonLat[[1]]
    Lat = LonLat[[2]]
  }
  d$Distance=NA
  i=1
  while(i<=nrow(d))
  {
    d$Distance[i] = distGeo(c(Lon,Lat), d[i,c('Lon', 'Lat')])
    if(d$Distance[i]<=Radius)
    {
      i=i+1
    }
    else
    {
      d=d[-i,]
    }
  }
  return(d)
}

#Function: api_followmee_gps----------------------------------------

api_followmee_gps <- function(device_id,from,to){
  url <- 'http://www.followmee.com'
  path <- '/api/tracks.aspx'
  
  api_key <- '12cf03011635a89978f641cf8b54441c'
  user_name <- 'ARCRISK1'
  output_type <- 'json'
  fn = 'daterangefordevice'
  
  #sample data
  #from = '2018-07-10'
  #to = '2018-07-11'
  #device_id = 11964044
  
  query <-  str_c('?key=', api_key,
                  '&username=', user_name,
                  '&output=', output_type,
                  '&function=', fn,
                  '&from=',from,
                  '&to=',to,
                  '&deviceid=',device_id)
  
  get_url <- str_c(url, path, query)
  
  j_gps <-  GET(url = get_url)
  
  if(http_type(j_gps) != "application/json") {
    stop('GPS not formated at JSON')
  }
  
  d_gps <- fromJSON(content(j_gps, "text"), simplifyVector = TRUE) %>% 
    .$Data
  return(d_gps)
}

#Function:  api_followmee_devices--------------------------------------------

api_followmee_devices <- function(){
  url  <-  'http://www.followmee.com'
  path <- '/api/info.aspx'
  
  api_key <- '12cf03011635a89978f641cf8b54441c'
  user_name <- 'ARCRISK1'
  output_type <- 'json'
  fn <- 'devicelist'
  
  query <- str_c('?key=', api_key,
                '&username=', user_name,
                '&output=',output_type,
                '&function=', fn)
  get_url <- str_c(url, path, query)
  
  j_devices <- GET(url=get_url)
  
  if(http_type(j_devices) != "application/json") {
    stop('Devices not formated at JSON')
  }
  
  d_devices <-  fromJSON(content(j_devices, "text"), simplifyVector = TRUE) %>% 
    .$Data
  
  return(d_devices)
}

#Function:  api_followmee_device_id--------------------------------------

api_followmee_device_id <- function(n_subid){
  #convert nSubID to string if needed
  if(is.numeric(n_subid)){
    s_subid <- varPadString(n_subid,3)
  }else{
    s_subid <- n_subid
  }
  d_devices <- api_followmee_devices()
  device_id <- d_devices$device_id[d_devices$device_name==s_subid]
  return(device_id)
}


#Function: convert_moves ---------------------

convert_moves <- function(file_name){
  
  message('......Loading: ', file_name)
  
  d_moves <- gps_load_moves(file_name) %>% 
    mutate(raw_orig_time = time,
      time = as_datetime(time, tz = 'America/Chicago')) %>% 
    arrange(time) %>% #sort on time
    mutate(
      data_type = 'gps',
      speed_kmh =NA,
      altitude_meters = NA,
      direction = NA,
      accuracy = NA,
      app_source = 'moves',
      count = NA
    ) %>% 
    select(lat, long, time, accuracy, sgmnt_type, trckpnt_type, 
           app_source, data_type, speed_kmh, altitude_meters,
           direction, count, raw_orig_time)
  
  return(d_moves)
}



#getFollowMee------------------------------------------

# tidying but not expecting to run this script
# instead using convert followmee with the saved raw data
# and calculating known places in make_gps

#This script updates a subject's raw FollowMee datafile to include new data up to the previous day
get_followmee <- function(n_subid,device_id, root_path){
  message('......Downloading FollowMee trackpoints for active participant via API')
  
  #CONSTANTS TO CONSIDER
  dis_tol <- 65   #make distance before considered new place trackpoint
  min_place_time <- 3  #must be at trackpoint for more than this time to be starting point for place
  
  subid <- varPadString(n_subid,3)
  
  raw_path <- file.path(root_path, subid)
  raw_file_name <- str_c(subid,'_GPSFollowRaw.rds')
  proc_file_name <- str_c(subid,'_GPSFollowProcessed.rds')
  d_dates <- read_excel(file.path(raw_path, str_c(subid, '_VisitDates.xlsx')), na= c('NA', '')) 
  
  #Open current files (if exists) and determine First New Day of Data
  if(file.exists(file.path(raw_path,raw_file_name))){
    d_r = read_rds(file.path(raw_path,raw_file_name))
    last_day = as_datetime(d_r$date[nrow(d_r)]) #get last day (read in as UTC by default b/c cant recognoze tz stamp)
    last_day = with_tz(last_day, 'America/Chicago')  #convert to America/Chicago
    last_day = as_date(last_day) #convert to just date
    first_new_day = last_day + days(1)#begin with next day after last recorded
    
    d_p = read_rds(file.path(raw_path,proc_file_name))
  }else{
    d_r <- NULL
    d_p <- NULL
    first_new_day <- d_dates$start_study #begin with study start day
  }
  
  #Determine last day to record
  last_new_day <- Sys.Date()
  if(last_new_day >= d_dates$end_study){
    last_new_day <- d_dates$end_studyEndStudy  #set to study end date for last recording
  }else{
    last_new_day <- Sys.Date() - 1  #end with yesterday so that its a complete day
  }
  
  message(str_c('......First Date for New GPS Data: ', first_new_day))
  message(str_c('......Last Date for New GPS Data:  ', last_new_day))
  
  #If there are new data to download, get it
  if(first_new_day <= last_new_day){
    
    start <- first_new_day
    while(start <= last_new_day){
      end <- start + days(6)  #get up to seven 7 days of data
      if(end > last_new_day) end <- last_new_day #dont collect beyond last new day b.c it will be incomplete
      
      #message('start: ', Start)
      #message('end: ', End)
      
      d_new_raw = api_followmee_gps(device_id, start, end)
      d_new_proc = d_new_raw
      
      if(!is.null(d_new_proc)){   #if new points exist in current period
        #Remove network points
        d_new_proc <- filter(d_new_proc, type != "Network") %>% 

        #Rename Lat/Lon,Type, and add empty Type
          rename(
            lat = Latitude,
            long = Longitude,
            data_type = Type,
            time = Date,
            speed_kmh = `Speed(km/h)`,
            altitude_meters = `Altitude(m)`
          ) %>% 
          clean_names("snake") %>% 
          mutate( #Add/Delete variables to match existing followmee and Moves
            type = "movement",
            app_source = "followmee",
            next_time = NA,
            next_dist = NA,
            count = NA
          ) %>% 
          mutate( #Process date variables
            raw_orig_time = time, 
            time = with_tz(as_datetime(time), 'America/Chicago') 
          )
        
        #merge new files with current files if curent files exists
        if(!is.null(d_r)){
          d_r = bind_rows(d_r,d_new_raw)
          d_p = bind_rows(d_p,d_new_proc)
        }else{
          d_r = d_new_raw
          d_p = d_new_proc
        }
      }else{
        message(str_c('......WARNING MESSAGE: No trackpoints available between: ', start, ' - ', end))
      }
      
      start = start+days(7)
    }
    
    #Calc Next Time
    d_p$next_time[1:(nrow(d_p)-1)] = round(difftime(d_p$time[2:(nrow(d_p))], d_p$time[1:(nrow(d_p)-1)], units='mins'),1)
    
    #reorder columns
    d_p <- select(d_p,
                  time, lat, long, next_time, next_dist,
                  app_source, data_type, speed_kmh, altitude_meters,
                  direction, count, raw_orig_time)
    
    ##HM STOPPED HERE TO RETURN ONCE PIPELINE IS SET UP
    
    #determine Places vs. Movement points
    iRow = 1
    while(iRow<nrow(d_p)){  #only check to second to last row b/c no nexttime for last row
      #message(str_c('Checking iRow: ', iRow))
      if(d_p$next_time[iRow]>min_place_time){  #found a starting place
        d_p$type[iRow] = 'place'
        
        #determine distance to entry before and after if they exist
        if(iRow>1) {
          BDist = distGeo(c(d_p$Long[iRow],d_p$Lat[iRow]),c(d_p$Long[iRow-1],d_p$Lat[iRow-1]))
        }else{
          BDist = Inf
        }
        if(iRow<nrow(d_p)) {
          ADist = distGeo(c(d_p$Long[iRow],d_p$Lat[iRow]),c(d_p$Long[iRow+1],d_p$Lat[iRow+1]))
        }else{
          ADist = Inf
        }
        
        Update = FALSE
        #if BDist is closer and within DistTol update iRow entry with weighted mean including entry before
        if(BDist<ADist && BDist <=DistTol){
          CombinePoints = matrix(cbind(d_p$Long[(iRow-1):iRow],d_p$Lat[(iRow-1):iRow]), nrow=2,ncol=2)
          NewPoints = geomean(xy=CombinePoints,w=d_p$Count[(iRow-1):iRow])
          d_p$Long[iRow]=NewPoints[1,1]
          d_p$Lat[iRow]=NewPoints[1,2]
          d_p$Count[iRow]=d_p$Count[iRow]+1
          d_p$Time[iRow] = d_p$Time[iRow-1]
          d_p = d_p[-(iRow-1),] #remove previous entry
          iRow = iRow-1 #account for removal
          Update=TRUE
        }
        
        #if ADist is closer and within DistTol update iRow entry with weighted mean including entry after
        if(ADist<=BDist && ADist <=DistTol){
          CombinePoints = matrix(cbind(d_p$Long[iRow:(iRow+1)],d_p$Lat[iRow:(iRow+1)]), nrow=2,ncol=2)
          NewPoints = geomean(xy=CombinePoints,w=d_p$Count[iRow:(iRow+1)])
          d_p$Long[iRow]=NewPoints[1,1]
          d_p$Lat[iRow]=NewPoints[1,2]
          d_p$Count[iRow]=d_p$Count[iRow]+1
          d_p = d_p[-(iRow+1),] #remove next entry
          Update=TRUE
        }
        if(!Update) iRow = iRow+1
        
      }else{
        iRow =  iRow+1
      }
    }
    
    #Recalc NextDist/Next Time
    d_p$NextTime = NA
    d_p$NextTime[1:(nrow(d_p)-1)] = round(difftime(d_p$Time[2:(nrow(d_p))], d_p$Time[1:(nrow(d_p)-1)], units='mins'),1)
    d_p$NextDist = NA
    d_p$NextDist[1:(nrow(d_p)-1)] = round(distGeo(d_p[2:(nrow(d_p)),c('Long', 'Lat')], d_p[1:(nrow(d_p)-1),c('Long', 'Lat')]),0)
    
    #Save files
    saveRDS(d_r, file.path(RawPath,RawFileName))
    saveRDS(d_p, file.path(RawPath,ProcFileName))
    
  }else{
    message('......WARNING MESSAGE: No new GPS data available')
  }
}  


# Function: convert_followmee() -----------------------------------------------------
# imports followmee data and reformats it to match moves data

convert_followmee <- function(file_name){
  
  #multiple GPX exist for 52 58 59 63 64 65 66 76 77 78 79 80 81 82
  message('......Loading: ', file_name)
  
  d_followmee <- read_rds(file_name) %>% 
    clean_names("snake") %>% 
    mutate(
      raw_orig_time = date,
      time = as_datetime(date, tz = 'America/Chicago')
      ) %>%  # convert time to dttm
    arrange(time) %>% 
    mutate(
      lat = latitude,
      long = longitude,
      data_type = str_to_lower(type),
      speed_kmh = speed_km_h,
      altitude_meters = altitude_m,
      app_source = 'followmee',
      count = NA,
      sgmnt_type = NA,
      trckpnt_type = NA
    ) %>% 
    select(lat, long, time, accuracy, sgmnt_type, trckpnt_type, 
           app_source, data_type, speed_kmh, altitude_meters,
           direction, count, raw_orig_time)
  
  return(d_followmee)
  
}
