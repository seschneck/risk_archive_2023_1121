library(readxl)
library(tidyverse)  # local functions so fine to load all tidyverse packages
library(purrr)

merge_excel_files <- function(file_suffix, path_root) {
  # Merges excel files that were created by the interview. For example,
  # Locations, Contacts, Dates, etc.
  # Returns one df with all subjects in the same file.
  
  # file_suffix: suffix on file name that indicates data type; e.g., Location
  # path_root: root folder for the subject folders containing the excel files
  
  import_w_subid <- function(xlsxfile){
    read_excel(xlsxfile) %>% 
      mutate(subid = str_extract(xlsxfile, "\\d\\d\\d"))
  }
  
  list.files(file.path(path_root), 
             recursive = TRUE,
             pattern = paste0("\\d\\d\\d_", file_suffix, ".xlsx"),
             full.names = TRUE) %>% 
    as.list() %>% 
    set_names(str_extract(., "\\d\\d\\d")) %>% 
    map_df(~import_w_subid(.x) %>% 
             clean_names() %>% 
             mutate(across(everything(), as.character)))
  
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


get_label_windows <- function(subid, study_start, study_end, ema_end, 
                              buffer_start = 0,
                              buffer_end = 0,
                              window_dur = 3600) {
  # Returns a tibble for one subject with columns for subid (numeric) and 
  #   dttm_label (dttm).  

  # Inputs:
  #   study_start, study_end, ema_end = dttm vars that should be in America/Chicago 
  #   buffer_start = adds buffer time in seconds between start_time and first lapses used 
  #     First hour for lapses is midnight on study day 1 by default (0)
  #   buffer_end = adds buffer time in seconds between end_time and last lapses used
  #     Last hour is min of final EMA or 11 pm on the last day of study by default (0)
  #   window_dur = duration of lapse window in seconds. Defaults to 1 hour
  
  start_time <- study_start + seconds(buffer_start)
  
  # calculate end_time in 4 steps
  #   1. get study end
  study_end <- study_end + (hours(23))  # 11 pm on study_end date
 
  #   2. get ema end
  ema_end <-  floor_date(ema_end, unit = "hours") - hours(1)
  
  #   3. calculate end time as earliest end time (study end or ema end)
  end_time <- min(study_end, ema_end)
  
  #   4. subtract end_buffer
  end_time <- end_time - seconds(end_buffer)
  
  
  label_windows <- tibble(dttm_label = seq(start_time, end_time, by = window_dur), subid) %>% 
    relocate(subid)
  
  return(label_windows)
}



# old function before we started using get_label_windows
get_study_hours <- function(subid, study_start, study_end, ema_end, buffer_hours = 0) {
  # Returns a tibble for one subject with columns for subid (numeric) and 
  #   dttm_label (dttm).  
  #   buffer_hours adds buffer time between study start and first lapses used 
  #   First hour for lapses is midnight on study day 1 by default (buffer_hours = 0)
  #   Last hour is earlier of the hour preceding the final EMA or 11 pm on the last day
  #   of the study
  # Inputs:
  #   all dttm in America/Chicago
  
  hour_start <- study_start + hours(buffer_hours)
  
  # calculate hour_end in three steps
  study_end <- study_end + (hours(23))  # 11 pm on study_end date
  
  ema_end <-  floor_date(ema_end, unit = "hours") - hours(1)
  
  hour_end <- min(study_end, ema_end)
  
  study_hours <- tibble(dttm_label = seq(hour_start, hour_end, by = "hour"), subid) %>% 
    relocate(subid)
  
  return(study_hours)
}


# FIX: add lapse_hour_window parameter to specify lapse window (hour should be 1 
# or in increments of 24)
# FIX: add lapse_hour_window_start parameter to specify when lapse window starts
# get_study_hours starts at midnight by default for day 1 - change buffer_hours parameter to
# match lapse_hour_window should start (i.e., 6 for 6 am)?
# hour end in get_study_hours is still going to be at 11 pm or one hour before last ema
get_lapse_labels <- function(lapses, dates, lapse_hour_window, lapse_hour_window_start = 0) {
  
  subids <- unique(dates$subid)
  
  labels <- dates %>% 
    select(subid, study_start, study_end, ema_end) %>% 
    # pass start time to get_study_hours?
    # mutate(buffer_hours = lapse_hour_window_start) %>% 
    pmap_dfr(~get_study_hours(..1, ..2, ..3, ..4)) %>% # add ..5 if using buffer_hours
    mutate(lapse = FALSE,
           no_lapse = TRUE)
  
  # FIX: if lapse_hour_window is > 1 check if there are any lapses in lapse_hour_window
  # Set window to start at buffer_hours
  # FIX: How to handle buffer around lapses? Currently as 3 hours for hour level lapses
  
  
  # Step 1: handle valid lapses
  valid_lapses <- lapses %>% 
    filter(!exclude)
  
  no_match <- 0
  for (i in 1:nrow(valid_lapses)) {
    lapse_subid <- valid_lapses$subid[[i]]
    lapse_hour <- valid_lapses$lapse_start[[i]]
    
    row_index <- which(labels$subid == lapse_subid & labels$dttm_label == lapse_hour)
    
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
      no_match <- no_match + 1
    }
  }
  
  # final check for lapses
  if (! (nrow(valid_lapses) == (sum(labels$lapse) + no_match))) {
    stop("Not all lapses accounted")
  }
  
  # Step 2: Handle no_lapse exclusions for valid lapse periods +- 24 hours.  
  
  # First set an end time for valid lapses that have a missing end time (there are a couple)
  # We will be cautious and set longest valid lapse duration (24 hours)
  valid_lapses <- valid_lapses %>% 
    mutate(lapse_end = if_else(is.na(lapse_end),
                               lapse_start + hours(24),
                               lapse_end))
  
  for (i in 1:nrow(valid_lapses)) {
    
    # exclude lapse +- 24 hours on each side
    lapse_hours_exclude <- seq(valid_lapses$lapse_start[[i]] - hours(24), 
                               valid_lapses$lapse_end[[i]] + hours(24), 
                               by = "hours")
    
    for (lapse_hour in lapse_hours_exclude) {
      row_index <- which(labels$subid == valid_lapses$subid[[i]] & labels$dttm_label == lapse_hour)
      
      if (length(row_index) == 1) {
        labels$no_lapse[row_index] <- FALSE
      } 
    }
  }
  
  # Step 3: Handle no_lapse exclusions for excluded periods with date but no times
  # Exclude full date +- 24 hours
  exclusions <- lapses %>% 
    filter(exclude & is.na(lapse_start) & is.na(lapse_end))
  
  for (i in 1:nrow(exclusions)) {
    lapse_start <- as_datetime(exclusions$lapse_start_date[[i]], 
                               format = "%m-%d-%Y", tz = "America/Chicago")
    lapse_end <- as_datetime(exclusions$lapse_end_date[[i]], 
                             format = "%m-%d-%Y", tz = "America/Chicago")
    
    # exclude lapse +- 3 hours on each side
    lapse_hours_exclude <- seq(lapse_start - hours(24), 
                               lapse_end + hours(48), # end of day + 24 hours
                               by = "hours")
    
    for (lapse_hour in lapse_hours_exclude) {
      row_index <- which(labels$subid == exclusions$subid[[i]] & labels$dttm_label == lapse_hour)
      
      if (length(row_index) == 1) {
        labels$no_lapse[row_index] <- FALSE
      } 
    }
  }
  
  # Step 4: Handle no_lapse exclusions for very long duration lapses (> 24 hours)
  # Exclude full period +- 24 hours
  exclusions <- lapses %>% 
    filter(exclude & duration > 24)
  
  for (i in 1:nrow(exclusions)) {
    
    # exclude lapse +- 24 hours on each side
    lapse_hours_exclude <- seq(exclusions$lapse_start[[i]] - hours(24), 
                               exclusions$lapse_end[[i]] + hours(24), 
                               by = "hours")
    
    for (lapse_hour in lapse_hours_exclude) {
      row_index <- which(labels$subid == exclusions$subid[[i]] & labels$dttm_label == lapse_hour)
      
      if (length(row_index) == 1) {
        labels$no_lapse[row_index] <- FALSE
      } 
    }
  }
  
  # Step 5: Handle no_lapse exclusions for negative duration lapses
  # Flip start and end time and then exclude full period +- 24 hours
  exclusions <- lapses %>% 
    filter(exclude & duration <= 24)
  
  for (i in 1:nrow(exclusions)) {
    
    # exclude lapse +- 3 hours on each side
    lapse_hours_exclude <- seq(exclusions$lapse_end[[i]] - hours(24), 
                               exclusions$lapse_start[[i]] + hours(24), 
                               by = "hours")
    
    for (lapse_hour in lapse_hours_exclude) {
      row_index <- which(labels$subid == exclusions$subid[[i]] & labels$dttm_label == lapse_hour)
      
      if (length(row_index) == 1) {
        labels$no_lapse[row_index] <- FALSE
      } 
    }
  }
  
  return(labels)
  
} 


sample_labels <- function(valid_labels, p_lapse, seed) {
  
  set.seed(seed)
  
  lapses <- valid_labels %>% 
    filter(lapse) %>% 
    mutate(label = "lapse") %>% 
    select(subid, dttm_label, label)
  
  n_no_lapse <- nrow(lapses) * ((1 / p_lapse) - 1)
  
  no_lapses <- valid_labels %>% 
    filter(no_lapse) %>% 
    mutate(label = "no_lapse") %>% 
    select(subid, dttm_label, label) %>% 
    sample_n(n_no_lapse)
  
  labels <- lapses %>% 
    bind_rows(no_lapses)
  
}
