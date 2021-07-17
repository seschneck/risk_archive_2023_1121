create_lapse_labels <- function() {
  
  
}

get_study_dates <- function(ema, visits) {
  # Returns a tibble with useful info about study dates for all participants.
  #   All date and dttm variables are in America/Chicago
  # Inputs: 
  #   ema is tibble with all ema responses (morning and later)
  #   visits is the visit dates file

  
  # confirm numeric subids in ema and visits tibbles for later joining
  if(!is.numeric(ema$subid)) {
    ema <- ema %>% 
      mutate(subid = as.numeric(subid))
  }
  if(!is.numeric(visits$subid)) {
    visits <- visits %>% 
      mutate(subid = as.numeric(subid))
  }
  
  ema <- ema %>% 
    rename(ema_start = start_date, ema_end = end_date) %>% 
    group_by(subid) %>% 
    arrange(ema_start) %>% 
    slice(c(1, n())) %>% 
    mutate(ema_start = min(ema_start),
           ema_end = max(ema_end),
           ema_start = with_tz(ema_start, tz = "America/Chicago"),
           ema_end = with_tz(ema_end, tz = "America/Chicago")) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(subid, ema_start, ema_end)
  
  visits <- visits %>% 
    mutate(followup_complete = !is.na(followup_1)) %>% 
    rename(study_start = start_study, study_end = end_study)
  
  dates <- visits %>% 
    left_join(ema, by = "subid") %>% 
    relocate(subid, study_start, study_end, ema_start, ema_end)
  
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