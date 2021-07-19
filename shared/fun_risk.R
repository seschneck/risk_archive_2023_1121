create_lapse_labels <- function() {
  
  
}

get_study_dates <- function(visits) {
  # Returns a tibble with useful info about study dates for all participants.
  #   All date and dttm variables are in America/Chicago
  # Inputs: 
  #   visits is the visit dates file

  
  
  visits <- visits %>% 
    rename(study_start = start_study, study_end = end_study) %>% 
    mutate(followup_complete = !is.na(followup_1),
           study_start = as_datetime(study_start),
           study_start = force_tz(study_start, tz = "America/Chicago"),
           study_end = as_datetime(study_end),
           study_end = force_tz(study_end, tz = "America/Chicago")) %>% 
    select(subid, study_start, study_end, followup_complete)

  
  return(dates)
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