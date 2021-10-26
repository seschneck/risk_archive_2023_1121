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

 
get_x_period <- function(the_subid, the_dttm_label, x_all, lead, period_duration) {
  
  # This function filters data rows based on a lapse label (subid and hour) passed in
  # Pass in subid and hour from lapse label - use map2_dfr to iterate through each 
  # row of lapse tibble
  # Pass in data tibble that will be filtered on by labels (MUST INCLUDE dttm_obs VARIABLE)
  # Set lead_hours parameter to number of hours out from lapse you wish to predict 
  # Set period duration hours to set the duration over which you wish to use data from  
  
  x_all %>% 
    filter(subid == the_subid) %>% 
    
    # filter on period duration hours
    mutate(period_start_dttm = the_dttm_label - duration(period_duration, "hours")) %>% 
    filter(dttm_obs >= period_start_dttm) %>% 
    
    # filter on lead hours
    mutate(diff_hours = as.numeric(difftime(the_dttm_label, dttm_obs, units = "hours"))) %>%  
    filter(diff_hours >= lead) %>% 
    select(-c(period_start_dttm, diff_hours))
}




correct_period_duration <- function(the_subid, the_dttm_label, data_start, period_duration) {
  
# set period_duration <- Inf to ignore and just get period duration based on data_start

  data_start <- data_start %>% 
    filter(subid == the_subid) %>% 
    pull(data_start)
  
  
  
  data_start_hours <- as.numeric(difftime(the_dttm_label, data_start, units = "hours"))


  period_duration <- if_else(data_start_hours < period_duration, 
                             data_start_hours,
                             period_duration)
  
  return(period_duration)
}



score_ratecount_value <- function(the_subid, the_dttm_label, x_all, period_durations, 
                                  lead, data_start, col_name, col_values, data_type_col_name = NA, 
                                  data_type_values = NA, context_col_name = NA, context_values = NA) {
  # Counts the number of matches for col_name == value and converts to a rate
  # based on the period_duration.  Returns a raw rate (ratecount) and two relative
  # rates (dratecount for diff between rate in period and rate across all data; 
  # pratecount for percent change between rate in period and rate across all data)
  
  # the_subid: single integer subid
  # the_dttm_label: single dttm for label onset
  # x_all:  raw data for all subids and communications
  # period_durations: vector of 1+ integer period_durations in hours
  # lead: the lead time for prediction in hours (a single integer)
  # data_start: a df with data_start = min(study_start, comm_start) for all subids
  # col_name: column name for raw data for feature as string
  # col_values: a vector of 1+ values to count within the col_name.  Can be string or numeric
  # data_type_col_name: name of column name to filter on for data log type values (used in meta study)
  # data_type_values: a vector of 1+ meta data log types to filter on (sms, or voice, if empty uses all logs)
  # context: col_name of context feature
  # context_values: a vector of 1+ context values to filter on

  
  
# Loop over context values
features <- foreach (context_value = context_values, .combine = "cbind") %do% {
  
  # Filter data if context_value provided
  if (!is.na(context_value)) {
    x_c <- x_all %>%
      filter(.data[[context_col_name]] == context_value) 
  } else x_c <- x_all # renaming to avoid rewriting over x_all for next loop
  
  
  # define function: this will change for other feature score functions
  ratecount <- function (.x, value, duration) {
    the_count <- if (length(.x) > 0) sum(.x == value, na.rm = TRUE)
      else 0
    return(the_count / duration)
  }
  
  base_duration <- correct_period_duration(the_subid, the_dttm_label, 
                                         data_start, Inf)  # use Inf to ignore period_duration

  
  # loop over data types
  foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
    
    if (!is.na(data_type_value)) {
      x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
    } else x <- x_c # renaming to avoid rewriting over x_c for next loop
      
    # loop over period durations and column values
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
      
      x_period <- get_x_period(the_subid, the_dttm_label, x, lead, period_duration)
      
      true_period_duration <- correct_period_duration(the_subid, the_dttm_label, 
                                                      data_start, period_duration)
    
      
    foreach(col_value = col_values, .combine = "cbind") %do% { 
          
          baseline <- x %>%
            filter(subid == the_subid) %>%
            summarise("base" := ratecount(.data[[col_name]], col_value, base_duration)) %>%
            pull(base)

          raw_rate <- x_period %>%
            summarise("raw" := ratecount(.data[[col_name]], col_value, true_period_duration)) %>%
            pull(raw)
 
          
          rates <- 
            tibble(
              "{data_type_value}.p{period_duration}.l{lead}.rratecount.{col_name}.{col_value}.{context_col_name}.{context_value}" := raw_rate,
              "{data_type_value}.p{period_duration}.l{lead}.dratecount.{col_name}.{col_value}.{context_col_name}.{context_value}" := raw_rate - baseline,
              "{data_type_value}.p{period_duration}.l{lead}.pratecount.{col_name}.{col_value}.{context_col_name}.{context_value}" := 
                if_else(baseline == 0, 0, (raw_rate - baseline) / baseline)) %>% 
            rename_with(~str_remove_all(.x, ".NA")) %>% 
            rename_with(~str_remove(.x, "^NA."))
        
    }
  }
  }
}
  
  features <- features %>%
    mutate(subid = the_subid,
           dttm_label = the_dttm_label) %>%
    relocate(subid, dttm_label)
  
  return(features)
}



score_propcount_value <- function(the_subid, the_dttm_label, x_all, 
                                  period_durations, lead, col_name, col_values, 
                                  data_type_col_name = NA, data_type_values = NA, 
                                  context_col_name = NA, context_values = NA) {
  # Counts the number of matches for col_name == value and converts to a rate
  # based on the period_duration.  Returns a raw rate (ratecount) and two relative
  # rates (dratecount for diff between rate in period and rate across all data; 
  # pratecount for percent change between rate in period and rate across all data)
  
  # the_subid: single integer subid
  # the_dttm_label: single dttm for label onset
  # x_all:  raw data for all subids and communications
  # period_durations: vector of 1+ integer period_durations in hours
  # lead: the lead time for prediction in hours (a single integer)
  # col_name: column name for raw data for feature as string
  # col_values: a vector of 1+ values to count within the col_name.  Can be string or numeric
  # data_type_col_name: name of column name to filter on for data log type values (used in meta study)
  # data_type_values: a vector of 1+ meta data log types to filter on (sms, or voice, if empty uses all logs)
  # context_col_name: col_name of context feature
  # context_values: a vector of 1+ context values to filter on
  
  
  
  # Loop over context values
  features <- foreach (context_value = context_values, .combine = "cbind") %do% {
    
    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all # rename to avoid rewriting over x_all for next loop
    
    
    # this will change for other feature score functions
    propcount <- function (.x, value, n_rows) {
      if (length(.x) > 0) {
        the_count <- sum(.x == value, na.rm = TRUE)
        return(the_count/n_rows)
      } else return(0)
    }
    
    
    # loop over data types
    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c # rename to avoid rewriting over x for next loop
      
      # loop over period durations and column values
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
        
        x_period <- get_x_period(the_subid, the_dttm_label, x, lead, period_duration)

        
        
        foreach(col_value = col_values, .combine = "cbind") %do% { 

          
          prop <- x_period %>%
            summarise("prop" := propcount(.data[[col_name]], col_value, nrow(x_period))) %>%
            pull(prop)
          
          
          rates <- 
            tibble(
              "{data_type_value}.p{period_duration}.l{lead}.proptotal.{col_name}.{col_value}.{context_col_name}.{context_value}" := prop) %>% 
            rename_with(~str_remove_all(.x, ".NA")) %>% 
            rename_with(~str_remove(.x, "^NA."))
        }
      }
    }
  }
  
  features <- features %>%
    mutate(subid = the_subid,
           dttm_label = the_dttm_label) %>%
    relocate(subid, dttm_label)
  
  return(features)
}


score_ratecontinuous_value <- function(the_subid, the_dttm_label, x_all, 
                                  period_durations, lead, data_start, 
                                  col_name, data_type_col_name = NA, data_type_values = NA, 
                                  context_col_name = NA, context_values = NA) {
  # Sums the value for col_name (i.e., duration) and converts to a rate
  # based on the period_duration.  Returns a raw rate (ratecount) and two relative
  # rates (dratecount for diff between rate in period and rate across all data; 
  # pratecount for percent change between rate in period and rate across all data)
  
  # the_subid: single integer subid
  # the_dttm_label: single dttm for label onset
  # x_all:  raw data for all subids and communications
  # period_durations: vector of 1+ integer period_durations in hours
  # lead: the lead time for prediction in hours (a single integer)
  # data_start: a df with data_start = min(study_start, comm_start) for all subids
  # col_name: column name for raw data for feature as string - should be continuous var
  # data_type_col_name: name of column name to filter on for data log type values (used in meta study)
  # data_type_values: a vector of 1+ meta data log types to filter on (sms, or voice, if empty uses all logs)
  # context_col_name: col_name of context feature
  # context_values: a vector of 1+ context values to filter on
  
  
  
  # Loop over context values
  features <- foreach (context_value = context_values, .combine = "cbind") %do% {
    
    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all
    
    
    # this will change for other feature score functions
    # FIX: confirm should handle no observations as NA (0 is interpreted differently 
    # with continuous var like duration)
    
    ratecontinuous <- function (.x, duration) {
      if (length(.x) > 0) {
        the_sum <- sum(.x, na.rm = TRUE)
        return(the_sum / duration)
      } else return(NA_integer_ )   
    }
    
    meancontinuous <- function (.x) {
      if (length(.x) > 0) {
        the_mean <- mean(.x, na.rm = TRUE) 
        return(the_mean)
      } else return(NA_integer_)
    }
    
    base_duration <- correct_period_duration(the_subid, the_dttm_label, 
                                             data_start, Inf)  # use Inf to ignore period_duration
    
    
    # loop over data types
    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c
      
      # loop over period durations and column values
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
        
        x_period <- get_x_period(the_subid, the_dttm_label, x, lead, period_duration)
        
        true_period_duration <- correct_period_duration(the_subid, the_dttm_label, 
                                                        data_start, period_duration)
          
          baseline <- x %>%
            filter(subid == the_subid) %>%
            summarise("base" := ratecontinuous(.data[[col_name]], base_duration)) %>%
            pull(base)
          
          raw_rate <- x_period %>%
            summarise("raw" := ratecontinuous(.data[[col_name]], true_period_duration)) %>%
            pull(raw)
          
          baseline_mean <- x %>% 
            filter(subid == the_subid) %>% 
            summarise("base" := meancontinuous(.data[[col_name]])) %>% 
            pull(base)
          
          raw_mean <- x_period %>%
            summarise("raw" := meancontinuous(.data[[col_name]])) %>%
            pull(raw)
          
          
          
          rates <- 
            tibble(
              "{data_type_value}.p{period_duration}.l{lead}.rratesum_{col_name}.{context_col_name}.{context_value}" := raw_rate,
              "{data_type_value}.p{period_duration}.l{lead}.dratesum_{col_name}.{context_col_name}.{context_value}" := raw_rate - baseline,
              "{data_type_value}.p{period_duration}.l{lead}.pratesum_{col_name}.{context_col_name}.{context_value}" := (raw_rate - baseline) / baseline,
              "{data_type_value}.p{period_duration}.l{lead}.rmean_{col_name}.{context_col_name}.{context_value}" := raw_mean,
              "{data_type_value}.p{period_duration}.l{lead}.dmean_{col_name}.{context_col_name}.{context_value}" := raw_mean - baseline_mean,
              "{data_type_value}.p{period_duration}.l{lead}.pmean_{col_name}.{context_col_name}.{context_value}" := (raw_mean - baseline_mean) / baseline_mean) %>% 
            rename_with(~str_remove_all(.x, ".NA")) %>% 
            rename_with(~str_remove(.x, "^NA."))
        }
    }
  }
  
  features <- features %>%
    mutate(subid = the_subid,
           dttm_label = the_dttm_label) %>%
    relocate(subid, dttm_label)
  
  return(features)
}


