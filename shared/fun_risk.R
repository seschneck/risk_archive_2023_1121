library(tidyverse)
library(lubridate)
library(vroom)
library(foreach)


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

merge_lapses <- function(lapses) {
# merges lapses that are overlapping
# used with df containing all participants
    
  lapses <- lapses %>% 
    arrange(subid, lapse_start) %>% 
    mutate(lapse_cnt = 1)
  
  i <- 2
  while(i <= nrow(lapses)) {
    
    if(lapses$subid[[i]] == lapses$subid[[i-1]] && 
       !lapses$exclude[[i]] && !lapses$exclude[[i-1]] &&
       !is.na(lapses$lapse_start[[i]]) && !is.na(lapses$lapse_end[[i-1]]) &&
       lapses$lapse_start[[i]] <= lapses$lapse_end[[i-1]]) {
      
      lapses$lapse_end[[i-1]] <- lapses$lapse_end[[i]]
      lapses$lapse_end_time[[i-1]] <- lapses$lapse_end_time[[i]]
      lapses$lapse_end_date[[i-1]] <- lapses$lapse_end_date[[i]]
      
      lapses$lapse_cnt[[i-1]] = lapses$lapse_cnt[[i-1]] + 1
      
      lapses <- lapses %>% 
        slice(-i)
      
    } else {
      i <- i + 1
    }
    
  }
  
  lapses <- lapses %>% 
    mutate(duration = difftime(lapse_end, lapse_start, units = "hours"))
  
  return(lapses)
}


get_study_hours <- function(subid, study_start, study_end, ema_end) {
  # Returns a tibble for one subject with columns for subid (numeric) and 
  #   lapse_hour (dttm).  
  #   First hour is midnight on study day 2
  #   Last hour is earlier of the hour preceding the final EMA or 11 pm on the last day
  #   of the study
  # Inputs:
  #   all dttm in America/Chicago
  
  hour_start <- study_start + days(1)
  
  # calculate hour_end in three steps
  study_end <- study_end + (hours(23))  # 11 pm on study_end date
  
  ema_end <-  floor_date(ema_end, unit = "hours") - hours(1)
  
  hour_end <- min(study_end, ema_end)
  
  study_hours <- tibble(hour = seq(hour_start, hour_end, by = "hour"), subid) %>% 
    relocate(subid)
  
  return(study_hours)
}

get_lapse_labels <- function(lapses, dates) {
  
  subids <- unique(dates$subid)
  
  labels <- dates %>% 
    select(subid, study_start, study_end, ema_end) %>% 
    pmap_dfr(~get_study_hours(..1, ..2, ..3, ..4)) %>% 
    mutate(lapse = FALSE,
           no_lapse = TRUE)
  
  
  # first handle lapses
  valid_lapses <- lapses %>% 
    filter(!exclude)
  
  no_match <- 0
  for (i in 1:nrow(valid_lapses)) {
    lapse_subid <- valid_lapses$subid[[i]]
    lapse_hour <- valid_lapses$lapse_start[[i]]
    
    row_index <- which(labels$subid == lapse_subid & labels$hour == lapse_hour)
    
    if (length(row_index) == 1) {
      labels$lapse[row_index] <- TRUE
    } 
    
    # some extra checks
    # this should not happen
    if (length(row_index) > 1) {
      stop("get_lapse_labels() multiple match; SubID: ", lapse_subid, " Lapse hour: ", lapse_hour, " i: ", i)
    }
    
    # this can happen if we have a lapse report outside of the study_hours
    if (length(row_index) == 0) {
      warnings("get_lapse_labels() no match; SubID: ", lapse_subid, " Lapse hour: ", lapse_hour, " i: ", i)
      no_match <- no_match + 1
    }
  }
  
  # final check for lapses
  if (! (nrow(valid_lapses) == (sum(labels$lapse) + no_match))) {
    stop("No all lapses accounted")
  }
  
  
  # now handle no_lapse exclusions
  exclusions <-  lapses %>% 
    filter(!exclude)
  
} 