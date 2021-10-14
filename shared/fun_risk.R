library(tidyverse)
library(lubridate)
library(vroom)
library(foreach)


get_study_dates <- function(filename_visits, filename_emam, filename_emal) {
  # Returns a tibble with study start and end dates as dttms in central time
  # Also indicates who completed through followup_1 and time of last completed ema
  # Inputs: 
  #   filename/path for processed visit_dates, and morning and later ema in processed data
  
  visits <- vroom(filename_visits, col_types = vroom::cols())  %>% 
    rename(study_start = start_study, study_end = end_study) %>% 
    mutate(followup_complete = !is.na(followup_1),
           study_start = as_datetime(study_start),
           study_start = force_tz(study_start, tz = "America/Chicago"),
           study_end = as_datetime(study_end),
           study_end = force_tz(study_end, tz = "America/Chicago"),
           subid = as.numeric(subid)) %>% 
    select(subid, study_start, study_end, followup_complete)

  emam <- vroom(filename_emam, col_types = vroom::cols()) %>% 
    rename_with(~ str_replace(.x, "emam_", "ema_"))
  
  emal <- vroom(filename_emal, col_types = vroom::cols())%>% 
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

get_lapse_labels <- function(lapses, dates) {
  
  subids <- unique(dates$subid)
  
  labels <- dates %>% 
    select(subid, study_start, study_end, ema_end) %>% 
    pmap_dfr(~get_study_hours(..1, ..2, ..3, ..4)) %>% 
    mutate(lapse = FALSE,
           no_lapse = TRUE)
  
  
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
    stop("No all lapses accounted")
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

 
get_feature_period <- function(the_subid, the_dttm_label, data, lead_hours, period_duration_hours) {
  
  # This function filters data rows based on a lapse label (subid and hour) passed in
  # Pass in subid and hour from lapse label - use map2_dfr to iterate through each 
  # row of lapse tibble
  # Pass in data tibble that will be filtered on by labels (MUST INCLUDE dttm_obs VARIABLE)
  # Set lead_hours parameter to number of hours out from lapse you wish to predict 
  # Set period duration hours to set the duration over which you wish to use data from  
  
  data <- data %>% 
    filter(subid == the_subid) %>% 
    # filter on period duration hours
    mutate(period_start_dttm = the_dttm_label - duration(period_duration_hours, "hours")) %>% 
    filter(dttm_obs >= period_start_dttm) %>% 
    # filter on lead hours
    mutate(diff_hours = as.numeric(difftime(the_dttm_label, dttm_obs, units = "hours"))) %>%  
    filter(diff_hours >= lead_hours) %>% 
    select(-c(period_start_dttm, diff_hours))
  
  data
}



make_features <- function (the_subid, the_dttm_label, data, lead_hours, period_duration_hours, 
                           study_start, data_type, col_list, fun_list){
  
  # This function takes a list of columns and list of functions to use for feature engineering 
  # It maps over a lapse label row taking in subid and hour
  # It calls the get_feature_period function and passes in the subid, hour, lead_hours 
  # and period_duration_hours parameters to narrow down the data to the appropriate 
  # rows for a lapse observation
  # It returns a single row of features (data_feat) for each lapse label 
  # data type is used for naming features
  
  # If using this function multiple times you should join returned feature set with 
  # previous feature set on subid and dttm_label
  
  # when defining statistical functions you should use an if else statement to run safely 
  # mean = function(.x) {if (length(.x) > 0) round(mean(.x, na.rm = TRUE), 2)
  #                        else  as.numeric(NA)}
  
  # filter down data
  data <- get_feature_period(the_subid, the_dttm_label, data, lead_hours, period_duration_hours)
  relative_hours <- get_relative_hours(the_subid, the_dttm_label, study_start, period_duration_hours)
  
  # create features
  data_feat <- data %>% 
    # all_of is for strict selection - error thrown if any variables are missing
    summarise(across(all_of(col_list), fun_list, .names = "{data_type}_{.fn}_{.col}")) %>% 
    mutate(subid = the_subid,
           dttm_label = the_dttm_label,
           relative_hours = relative_hours) %>% 
    relocate(subid, dttm_label, relative_hours)
  
  data_feat
  
}

get_relative_hours <- function(the_subid, the_dttm_label, study_start, period_duration_hours) {
  
  # pass in tibble of subids and study_start date - dates should be in central time
  # pass in the period_duration_hours 
  # pass in subid and label

  study_start_date <- subset(study_start, subid == the_subid)$start_study
  relative_hours <- as.numeric(difftime(the_dttm_label, study_start_date, units = "hours"))
  if (relative_hours >= period_duration_hours) return(period_duration_hours)
   else return(relative_hours)
}

