#This script makes features for the GPS paper analysis

# set up ------------------------------------------------------------------
library(tidyverse)
library(purrr)
library(janitor)
library(lubridate)
library(data.table)

gps <- read_rds("./analysis/shared/data/ds_gps.rds") %>%  
  as.data.table()


#to do: add a proxy for sleep time/ nightly rest

# wrangling ---------------------------------------------------------------

gps <- gps %>% 
  group_by(subid) %>% 
  filter(!(is.na(lat) & is.na(long))) %>% #drop missing days
  mutate( #calculate next time
    next_time = difftime(lead(time_central), time_central, units = "mins")) %>% 
  ungroup()

# make predictor df------------------------------------------------------

#store visits
visits <- read_rds("./analysis/shared/data/ds_visits.rds") %>% 
  mutate(subid = as.character(str_pad(subid, width = 3, side = "left", pad = "0"))) %>% 
  glimpse()

#function to create prediction structure
create_x_structure <- function(focal_subid){
 
  start_study <- visits %>% 
    filter(subid == focal_subid) %>% 
    pull(start_study)
  
  end_study <- visits %>% 
    filter(subid == focal_subid) %>% 
    pull(end_study)
  
  study_length <- as.numeric( #calculate days on study
    difftime(end_study, start_study, units = "days"))
  
  #create lapse periods (for prediction)
  lapse_periods <- c(1:study_length) %>% #for each day on study
    days() + #define the lapse prediction period (i.e., the following day)  
    as_datetime(start_study, tz = 'America/Chicago') + #add to start date, tz
    hours(18) # adjust such that time is noon-to-noon CDT
  #ISSUE: make sure that we are accounting for DST correctly
  # might need to force the tz
  # make sure that if a participant's lapse window correctly accounts
  # for shifts during the study (e.g., on study around oct 31)
  # double check that for GPS we handle DST shifts correctly
  
  #create a tibble with lapse periods and different prediction windows 
  gps_x_wide <- tibble(subid = focal_subid, focal_lapse_period = lapse_periods) %>% 
    mutate( #creating intervals corresponding to levels of 
      int_start_1day = focal_lapse_period - days(1),
      int_start_3day = focal_lapse_period - days(3),
      int_start_week = focal_lapse_period - days(7),
      int_1day = interval(int_start_1day, focal_lapse_period),
      # ISSUE: add an interval type for since study start, including first day
      #could add lags (e.g., 1 lagged day (i.e., 2 days before target lapse))
      int_3day = interval(int_start_3day, focal_lapse_period),
      int_week = interval(int_start_week, focal_lapse_period)) %>% 
    select(subid, focal_lapse_period, int_1day, int_3day, int_week)
  
  # make long
  # note: gather() and melt() do not work with time intervals
  temp_1day <- gps_x_wide %>% 
    select(subid, focal_lapse_period, int_1day) %>% 
    mutate(interval_type = "1day",
           interval = int_1day) %>% 
    select(subid, focal_lapse_period, interval_type, interval)
  
  temp_3day <- gps_x_wide %>% 
    select(subid, focal_lapse_period, int_3day) %>% 
    mutate(interval_type = "3day",
           interval = int_3day) %>% 
    select(subid, focal_lapse_period, interval_type, interval)
  
  temp_week <- gps_x_wide %>% 
    select(subid, focal_lapse_period, int_week) %>% 
    mutate(interval_type = "week",
           interval = int_week) %>% 
    select(subid, focal_lapse_period, interval_type, interval)
  
  gps_x <- bind_rows(temp_1day, temp_3day) %>% bind_rows(temp_week)
  
  return(gps_x)
  
}

#create x structure for all participants
gps_x <- map_df(unique(gps$subid),~create_x_structure(.x)) %>% 
  glimpse()

#  prep factors for feature engineering -----------------------------------

gps <- gps %>% 
  mutate( #turn levels of known_place_type into binary factors
    # ISSUE: replace this code using class2ind() or move to recipes
    place_AAmeeting = as.factor(
      case_when(known_place_type == "AA/RECOVERY MEETING" ~ "YES",
                known_place_type != "AA/RECOVERY MEETING" ~
                  "NO",
                TRUE ~ known_place_type)), #else (i.e., NA), remain as is
    place_BAR = as.factor(
      case_when(known_place_type == "BAR" ~ "YES",
                known_place_type != "BAR" ~ "NO",
                TRUE ~ known_place_type)),
    place_CHURCH = as.factor(
      case_when(known_place_type == "CHURCH" ~ "YES",
                known_place_type != "CHURCH" ~ "NO",
                TRUE ~ known_place_type)),
    place_CAFE = as.factor(
      case_when(known_place_type == "COFFEE SHOP/CAFE" ~ "YES",
                known_place_type != "COFFEE SHOP/CAFE" ~ "NO",
                TRUE ~ known_place_type)),
    place_ERRANDS = as.factor(
      case_when(known_place_type == "ERRANDS" ~ "YES",
                known_place_type != "ERRANDS" ~ "NO",
                TRUE ~ known_place_type)),
    place_GYM = as.factor(
      case_when(known_place_type == "GYM/FITNESS CENTER" ~ "YES",
                known_place_type != "GYM/FITNESS CENTER" ~ "NO",
                TRUE ~ known_place_type)),
    place_HEALTHCARE = as.factor(
      case_when(known_place_type == "HEALTH CARE" ~ "YES",
                known_place_type != "HEALTH CARE" ~ "NO",
                TRUE ~ known_place_type)),
    place_HOME = as.factor(
      case_when(known_place_type == "HOME" ~ "YES",
                known_place_type != "HOME" ~ "NO",
                TRUE ~ known_place_type)),
    place_FAMILY = as.factor(
      case_when(known_place_type == "HOME OF FAMILY MEMBER" ~ "YES",
                known_place_type != "HOME OF FAMILY MEMBER" ~ "NO",
                TRUE ~ known_place_type)),
    place_FRIEND = as.factor(
      case_when(known_place_type == "HOME OF FRIEND" ~ "YES",
                known_place_type != "HOME OF FRIEND" ~ "NO",
                TRUE ~ known_place_type)),
    place_LIQUOR = as.factor(
      case_when(known_place_type == "LIQUOR STORE" ~ "YES",
                known_place_type != "LIQUOR STORE" ~ "NO",
                TRUE ~ known_place_type)),
    place_OTHER = as.factor(
      case_when(known_place_type == "OTHER" ~ "YES",
                known_place_type != "OTHER" ~ "NO",
                TRUE ~ known_place_type)),
    place_PARK = as.factor(
      case_when(known_place_type == "PARK" ~ "YES",
                known_place_type != "PARK" ~ "NO",
                TRUE ~ known_place_type)),
    place_RESTAURANT = as.factor(
      case_when(known_place_type == "RESTAURANT" ~ "YES",
                known_place_type != "RESTAURANT" ~ "NO",
                TRUE ~ known_place_type)),
    place_SCHOOL = as.factor(
      case_when(known_place_type == "SCHOOL" ~ "YES",
                known_place_type != "SCHOOL" ~ "NO",
                TRUE ~ known_place_type)),
    place_VOLUNTEER = as.factor(
      case_when(known_place_type == "VOLUNTEER" ~ "YES",
                known_place_type != "VOLUNTEER" ~ "NO",
                TRUE ~ known_place_type)),
    place_WORK = as.factor(
      case_when(known_place_type == "WORK" ~ "YES",
                known_place_type != "WORK" ~ "NO",
                TRUE ~ known_place_type)),
    place_unpleasant = as.factor(
      case_when(emotion == "UNPLEASANT" ~ "YES",
                emotion == "MIXED" ~ "YES",
                emotion == "NEUTRAL" ~ "NO",
                emotion == "PLEASANT" ~ "NO",
                TRUE ~ emotion)),
    place_pleasant = as.factor(
      case_when(emotion == "PLEASANT" ~ "YES",
                emotion == "MIXED" ~ "YES",
                emotion == "NEUTRAL" ~ "NO",
                emotion == "UNPLEASANT" ~ "NO",
                TRUE ~ emotion)),
    place_mixed = as.factor(
      case_when(emotion == "MIXED" ~ "YES",
                emotion != "MIXED" ~ "NO",
                TRUE ~ emotion)),
    place_highrisk = as.factor(
      case_when(risk == "HIGH" ~ "YES",
                risk != "HIGH" ~ "NO",
                TRUE ~ risk)),
    place_minmedrisk = as.factor(
      case_when(risk == "HIGH" ~ "YES",
                risk == "MEDIUM" ~ "YES",
                risk == "LOW" ~ "NO",
                risk == "NO" ~ "NO",
                TRUE ~ risk)),
    place_minlowrisk = as.factor(
      case_when(risk == "HIGH" ~ "YES",
                risk == "MEDIUM" ~ "YES",
                risk == "LOW" ~ "YES",
                risk == "NO" ~ "NO",
                TRUE ~ risk))) %>% 
  clean_names("snake") %>% 
  rename(
    place_drank = drank,
    place_avoid = avoid,
    place_alcohol = alcohol,
    place_vacation = known_vacation
  )

#ISSUE: add in variables for socially connected places 

# make features -----------------------------------------------------------

# PLAN: the gps_x dataset will serve as input to a function that 
# first selects gps observations for each participant and each window
# then calculates times and counts (e.g., across in windows of different lengths)
# for each type of place
# (We can use this system to subset other kinds of observations or create other 
# kinds of variables (like day of the week, etc.))
# 
# So, in this system, a function will 
# use each row of gps_x as a string of arguments
# we will identify the outcome (lapse) with focal_lapse_period

# function that pulls total time at each place type
# for the specified subject, lapse period, and interval
get_place_times <- function(index){
  
  args_vector <- slice(gps_x, index)
  
  focal_subid <- args_vector[1]
  focal_lapse_period <- args_vector[2]
  interval_type <- args_vector[3]
  interval <- args_vector[4]
  
  gps %>% 
    filter(subid == focal_subid) %>% 
    filter(time_central %within% interval) %>% #ISSUE: filter on actual time
    summarise(
      across(starts_with("place"), 
             ~sum(ifelse(.x == "YES", next_time, 0), 
                  na.rm = TRUE), #carefully consider how NAs are handled
             .names = "time_{interval_type}_{col}")) %>% 
    mutate(
      subid = focal_subid,
      interval_type = interval_type,
      focal_lapse_period = focal_lapse_period,
      interval = interval) %>%
    select(subid, focal_lapse_period, interval_type, interval, everything())
}

# using the function
gps_features <- map_df(1:nrow(gps_x), get_place_times) 


# STOPPED HERE
# implement the function for every row
# try: nest_by, then purr functions to create individual lists

# ISSUE: for counts, first chunk observations into tracks (using the code written previously),
# then compute counts

# MORE IDEAS:
# Add a predictor that proxies non-vacation-related travel. Consider a binary 
# variable for a certain distance from home 