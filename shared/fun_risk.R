library(tidyverse)
library(lubridate)
library(vroom)

create_lapse_labels <- function() {
  
  
}

get_study_dates <- function(filename_visits, filename_emam, filename_emal) {
  # Returns a tibble with study start and end dates as dttms in central time
  # Also indicates who completed through followup_1 and time of last completed ema
  # Inputs: 
  #   filename/path for processed visit_dates, and morning and later ema in processed data
  
  visits <- vroom(filename_visits)  %>% 
    rename(study_start = start_study, study_end = end_study) %>% 
    mutate(followup_complete = !is.na(followup_1),
           study_start = as_datetime(study_start),
           study_start = force_tz(study_start, tz = "America/Chicago"),
           study_end = as_datetime(study_end),
           study_end = force_tz(study_end, tz = "America/Chicago"),
           subid = as.numeric(subid)) %>% 
    select(subid, study_start, study_end, followup_complete)

  emam <- vroom(filename_emam) %>% 
    rename_with(~ str_replace(.x, "emam_", "ema_"))
  
  emal <- vroom(filename_emal)%>% 
    rename_with(~ str_replace(.x, "emal_", "ema_"))
  
  ema <- bind_rows(emam, emal) %>% 
    filter(finished == 1) %>% 
    mutate(subid = as.numeric(subid)) %>% 
    select(subid, ema_end = start_date) %>% 
    mutate(ema_end = with_tz(ema_end, tzone = "America/Chicago")) %>% # for easier checking
    right_join((visits %>% select(subid, study_end)), by = "subid") %>% 
    filter(ema_end <= study_end + days(2)) %>%   # only consider emas within two days as valid
    group_by(subid) %>% 
    arrange(desc(ema_end)) %>% 
    slice(1)
      
    visits <- visits %>% 
      left_join((ema %>% select(subid, ema_end)), by = "subid")

  return(visits)
}

get_lapse_hours <- function(subid, study_start, study_end, ema_end) {
  # Returns a tibble for one subject with columns for subid (numeric) and 
  #   lapse_hour (dttm).  
  #   First hour is midnight on study day 2
  #   Last hour is earlier of the hour of the final EMA or 11 pm on the last day
  #   of the study
  # Inputs:
  #   all date or dttm in America/Chicago
  
  hour_start <- study_start %>% 
    as_datetime() %>% # forces as UTC
    force_tz(tz = "America/Chicago") %>% # set to America/Chicago with same hour
    `+`(days(1)) 
  
  # calculate hour_end in three steps
  study_end <- study_end %>% 
    as_datetime() %>% # forces as UTC
    force_tz(tz = "America/Chicago") %>%   # set to America/Chicago with same hour
    `+`(hours(23))  # 11 pm on study_end date
  
  ema_end <- ema_end %>% 
    floor_date(unit = "hours")
  
  hour_end <- min(study_end, ema_end)
  
  lapse_hours <- tibble(hour = seq(hour_start, hour_end, by = "hour"), subid) %>% 
    relocate(subid)
  
  return(lapse_hours)
}