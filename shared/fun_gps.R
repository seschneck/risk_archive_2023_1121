# WE NEED TO MOVE THESE SCRIPTS TO fun_gps.R in toolboxes/lab_support
# THIS WILL BE TRUE FOR CURRENT AND FOLLOWMEE FUNCTIONS



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