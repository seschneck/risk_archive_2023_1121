suppressWarnings(suppressPackageStartupMessages({
require(lubridate)
require(vroom)
require(dplyr)
require(stringr)
require(foreach)
}))

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



get_x_period <- function(the_subid, the_dttm_label, x_all, lead, period_duration) {
  
  # This function filters data rows based on a lapse label (subid and hour) passed in
  # Pass in subid and hour from lapse label - use map2_dfr to iterate through each 
  # row of lapse tibble
  # Pass in data tibble that will be filtered on by labels (MUST INCLUDE dttm_obs VARIABLE)
  # Set lead_hours parameter to number of hours out from lapse you wish to predict 
  # Set period duration hours to get the duration over which you wish to use data from  
  # set period_duration to Inf if you want all the way back to the first observation
  
  x_all %>% 
    filter(subid == the_subid) %>% 
    
    # filter on period_duration hours and lead hours
    # if period_duration == Inf, set period_start_dttm to date long ago! (kludge)
    mutate(period_start_dttm = if_else(is.infinite(period_duration),
                                       the_dttm_label - duration(24 * 365 * 5, "hours"), # 5 years before label
                                       the_dttm_label - duration(period_duration + lead, "hours")), # lead + duration before label
           period_end_dttm = the_dttm_label - duration(lead, "hours")) %>% # lead before label
    filter(dttm_obs >= period_start_dttm,
           dttm_obs < period_end_dttm) %>% # less than to make sure no overlap between features and label period
    select(-c(period_start_dttm, period_end_dttm))
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
                                  data_type_values = NA, context_col_name = NA, context_values = NA,
                                  passive = FALSE) {
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
  # passive: is a variable to distinguish variables that use no context and to append passive
  # onto those variable names for filtering down feature sets in recipes (set to TRUE if passive)
  

  
  # define ratecount function
  ratecount <- function (.x, value, duration) {
    the_count <- if (length(.x) > 0) {
      sum(.x == value, na.rm = TRUE)
    } else 0
    
    return(the_count / duration)
  }

  # nested foreach - col_value within period_duration within data_type_value within context_value
  
  features <- foreach (context_value = context_values, .combine = "cbind") %do% {
    
    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all # renaming to avoid rewriting over x_all for next loop
    
    
    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c # renaming to avoid rewriting over x_c for next loop
      

      
      
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
        
        true_period_duration <- correct_period_duration(the_subid, the_dttm_label, 
                                                        data_start, period_duration)
        
        base_duration <- correct_period_duration(the_subid, the_dttm_label, 
                                                 data_start, Inf)  # use Inf to ignore period_duration
        
        foreach(col_value = col_values, .combine = "cbind") %do% { 

          # baseline moved to here for this scoring function to limit to col_value   CHECK WITH KENDRA
          baseline <- x %>%
            get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = Inf) %>% # Inf gives all data back to first obs
            summarise("base" := ratecount(.data[[col_name]], col_value, base_duration)) %>%
            pull(base)
          
          raw <- x %>%
            get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = period_duration) %>% 
            summarise("raw" := ratecount(.data[[col_name]], col_value, true_period_duration)) %>%
            pull(raw)
          
          passive_label <- if_else(passive, "passive", "NA")
          
          rates <- 
            tibble(
              "{data_type_value}.p{period_duration}.l{lead}.rratecount.{col_name}.{col_value}.{context_col_name}.{context_value}.{passive_label}" := raw,
              "{data_type_value}.p{period_duration}.l{lead}.dratecount.{col_name}.{col_value}.{context_col_name}.{context_value}.{passive_label}" := raw - baseline) %>% 
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
                                  context_col_name = NA, context_values = NA,
                                  passive = FALSE) {
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
  # passive: is a variable to distinguish variables that use no context and to append passive
  # onto those variable names for filtering down feature sets in recipes (set to TRUE if passive)
  
  
  # define propcount function
  propcount <- function (.x, value, n_rows) {
    if (length(.x) > 0) {
      the_count <- sum(.x == value, na.rm = TRUE)
      return(the_count / n_rows)
    } else return(NA_real_) # NA because if they have no rows we cannot deduce a proportion - different than 0
  }
  
  
  # nested foreach - col_value within period_duration within data_type_value within context_value
  
  features <- foreach (context_value = context_values, .combine = "cbind") %do% {
    
    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all # rename to avoid rewriting over x_all for next loop
    
    
    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c # rename to avoid rewriting over x for next loop
      
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
        
        foreach(col_value = col_values, .combine = "cbind") %do% { 

          # baseline moved to here for this scoring function to limit to col_value  
          baseline <- x %>%
            get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = Inf) %>% # Inf gives all data back to first obs
            summarise("base" := propcount(.data[[col_name]], col_value, nrow(.))) %>%   
            pull(base)
          
          raw <- x %>%
            get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = period_duration) %>% 
            summarise("raw" := propcount(.data[[col_name]], col_value, nrow(.))) %>% 
            pull(raw)
          
          passive_label <- if_else(passive, "passive", "NA")
          
          rates <- 
            tibble(
              "{data_type_value}.p{period_duration}.l{lead}.rpropcount.{col_name}.{col_value}.{context_col_name}.{context_value}.{passive_label}" := raw,
              "{data_type_value}.p{period_duration}.l{lead}.dpropcount.{col_name}.{col_value}.{context_col_name}.{context_value}.{passive_label}" := raw - baseline) %>%   
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

score_propdatetime <- function(the_subid, the_dttm_label, x_all, period_durations, lead, 
                               dttm_col_name, dttm_window, dttm_description, 
                               data_type_col_name = NA, data_type_values = NA, 
                               context_col_name = NA, context_values = NA, passive = FALSE) {
  # Counts the number of matches for col_name == value and converts to a rate
  # based on the period_duration.  Returns a raw rate (ratecount) and two relative
  # rates (dratecount for diff between rate in period and rate across all data; 
  # pratecount for percent change between rate in period and rate across all data)
  
  # the_subid: single integer subid
  # the_dttm_label: single dttm for label onset
  # x_all:  raw data for all subids and communications
  # period_durations: vector of 1+ integer period_durations in hours
  # lead: the lead time for prediction in hours (a single integer)
  # dttm_col_name: column name for raw data for feature as string
  # dttm_window: a boolean filter statement to filter dttm_col_name on 
  # dttm_description: short description used in variable naming
  # data_type_col_name: name of column name to filter on for data log type values (used in meta study)
  # data_type_values: a vector of 1+ meta data log types to filter on (sms, or voice, if empty uses all logs)
  # context_col_name: col_name of context feature
  # context_values: a vector of 1+ context values to filter on
  # passive: is a variable to distinguish variables that use no context and to append passive
  # onto those variable names for filtering down feature sets in recipes (set to TRUE if passive)
  
  
  # define propdatetime function
  propdatetime <- function (.x, dttm_col_name, dttm_window, n_rows) {
    if (nrow(.x) > 0) {
      the_count <- .x %>% 
        filter(!!dttm_window) %>% 
        nrow()
      return(the_count / n_rows)
    } else return(NA_real_) # NA because if they have no rows we cannot deduce a proportion - different than 0
  }
  
  
  # nested foreach - period_duration within data_type_value within context_value
  
  features <- foreach (context_value = context_values, .combine = "cbind") %do% {
    
    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all # rename to avoid rewriting over x_all for next loop
    

    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c # rename to avoid rewriting over x for next loop
      
      # baseline is constant across period durations so get outside of lower loop
      baseline <- x %>% 
        get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = Inf) %>% # Inf gives all data back to first obs
        summarise("base" := propdatetime(., dttm_col_name, dttm_window, nrow(.))) %>%   
        pull(base)
      
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
        
        raw <- x %>%
          get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = period_duration) %>% 
          summarise("raw" := propdatetime(., dttm_col_name, dttm_window, nrow(.))) %>%   
          pull(raw)
        
        passive_label <- if_else(passive, "passive", "NA")
        
        rates <- 
          tibble(
            "{data_type_value}.p{period_duration}.l{lead}.rpropdatetime.{dttm_col_name}.{dttm_description}.{context_col_name}.{context_value}.{passive_label}" := raw,
            "{data_type_value}.p{period_duration}.l{lead}.dpropdatetime.{dttm_col_name}.{dttm_description}.{context_col_name}.{context_value}.{passive_label}" := raw - baseline) %>%  
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

score_ratesum <- function(the_subid, the_dttm_label, x_all, 
                          period_durations, lead, data_start, 
                          col_name, data_type_col_name = NA, data_type_values = NA, 
                          context_col_name = NA, context_values = NA, passive = FALSE) {
  # Sums the value for col_name (i.e., duration) and converts to a rate
  # based on the period_duration.  Returns a raw rate and two relative
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
  # passive: is a variable to distinguish variables that use no context and to append passive
  # onto those variable names for filtering down feature sets in recipes (set to TRUE if passive)
  
  
  # define ratesum function
  ratesum <- function (.x, duration) {
    the_sum <- if (length(.x) > 0) {
      sum(.x, na.rm = TRUE)
    } else 0
    
    return(the_sum / duration)
  }
  
  # nested foreach - period_duration within data_type_value within context_value
  
  features <- foreach (context_value = context_values, .combine = "cbind") %do% {
    
    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all
    
    
    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c
    
      # baseline is constant across period durations so get outside of lower loop
      # some periods are full duration b.c they are at the beginning of data collection
      base_duration <- correct_period_duration(the_subid, the_dttm_label, 
                                               data_start, Inf)  # use Inf to ignore period_duration
      
      baseline <- x %>%
        get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = Inf) %>% # Inf gives all data back to first obs
        summarise("base" := ratesum(.data[[col_name]], base_duration)) %>%
        pull(base)
      
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
      
        # some periods are full duration b.c they are at the beginning of data collection
        true_period_duration <- correct_period_duration(the_subid, the_dttm_label, 
                                                        data_start, period_duration)
        
        raw <- x %>%
          get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = period_duration) %>% 
          summarise("raw" := ratesum(.data[[col_name]], true_period_duration)) %>%
          pull(raw)
        
        passive_label <- if_else(passive, "passive", "NA")
        
        rates <- 
          tibble(
            "{data_type_value}.p{period_duration}.l{lead}.rratesum_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw,
            "{data_type_value}.p{period_duration}.l{lead}.dratesum_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw - baseline)  %>% 
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


score_mean <- function(the_subid, the_dttm_label, x_all, 
                       period_durations, lead, data_start, 
                       col_name, data_type_col_name = NA, data_type_values = NA, 
                       context_col_name = NA, context_values = NA, passive = FALSE) {
  
  # Gets value for col_name and returns a raw mean, a mean change, and a proportion change in mean from baseline
  # i.e., (raw mean - baseline mean) / baseline mean
  # proportion score set to NA if baseline == 0
  
  # the_subid: single integer subid
  # the_dttm_label: single dttm for label onset
  # x_all:  raw data for all subids and communications
  # period_durations: vector of 1+ integer period_durations in hours
  # lead: the lead time for prediction in hours (a single integer)
  # data_start: a df with data_start = min(study_start, comm_start) for all subids
  # col_name: column name for raw data for feature as string - should be continuous var
  # data_type_col_name: name of column name to filter on for data log type values
  # data_type_values: a vector of 1+ meta data log types to filter on (sms, or voice, if empty uses all logs)
  # context_col_name: col_name of context feature. Set to NA if no context filter
  # context_values: a vector of 1+ context values to filter on.  Set to NA if no context filter
  # passive: is a variable to distinguish variables that use no context and to append passive
  # onto those variable names for filtering down feature sets in recipes (set to TRUE if passive)
  
  # define period_mean function
  period_mean <- function (.x) {
    if (length(.x) > 0) { 
      if (!all(is.na(.x))) {
        the_mean <- mean(.x, na.rm = TRUE)
      } else the_mean <- NA
    } else the_mean <- NA
    
    return(the_mean)
  }
  
  # nested foreach - period_duration within data_type_value within context_value

  features <- foreach (context_value = context_values, .combine = "cbind") %do% {

    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all

    
    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c

      # get baseline mean using all data before label dttm
      baseline <- x %>% 
        get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = Inf) %>% # Inf gives all data back to first obs
        summarise("base" := period_mean(.data[[col_name]])) %>% 
        pull(base)
      
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
        
        raw <- x %>%
          get_x_period(the_subid, the_dttm_label, ., lead, period_duration) %>% 
          summarise("raw" := period_mean(.data[[col_name]])) %>%
          pull(raw)
        
        passive_label <- if_else(passive, "passive", "NA")
        
        tibble(
          "{data_type_value}.p{period_duration}.l{lead}.rmean_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw,
          "{data_type_value}.p{period_duration}.l{lead}.dmean_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw - baseline) %>% 
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

score_median <- function(the_subid, the_dttm_label, x_all, 
                       period_durations, lead, data_start, 
                       col_name, data_type_col_name = NA, data_type_values = NA, 
                       context_col_name = NA, context_values = NA, passive = FALSE) {
  
  # Gets value for col_name and returns a raw median, a median change, and a proportion change in median from baseline
  # i.e., (raw mean - baseline mean) / baseline mean
  # proportion score set to NA if baseline == 0
  
  # the_subid: single integer subid
  # the_dttm_label: single dttm for label onset
  # x_all:  raw data for all subids and communications
  # period_durations: vector of 1+ integer period_durations in hours
  # lead: the lead time for prediction in hours (a single integer)
  # data_start: a df with data_start = min(study_start, comm_start) for all subids
  # col_name: column name for raw data for feature as string - should be continuous var
  # data_type_col_name: name of column name to filter on for data log type values
  # data_type_values: a vector of 1+ meta data log types to filter on (sms, or voice, if empty uses all logs)
  # context_col_name: col_name of context feature. Set to NA if no context filter
  # context_values: a vector of 1+ context values to filter on.  Set to NA if no context filter
  # passive: is a variable to distinguish variables that use no context and to append passive
  # onto those variable names for filtering down feature sets in recipes (set to TRUE if passive)
  
  # define period_median function
  period_median <- function (.x) {
    if (length(.x) > 0) { 
      if (!all(is.na(.x))) {
        the_median <- median(.x, na.rm = TRUE)
      } else the_median <- NA
    } else the_median <- NA
    
    return(the_median)
  }
  
  # nested foreach - period_duration within data_type_value within context_value
  
  features <- foreach (context_value = context_values, .combine = "cbind") %do% {
    
    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all
    

    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c
      
      # get baseline median using all data before label dttm
      baseline <- x %>% 
        get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = Inf) %>% # Inf gives all data back to first obs
        summarise("base" := period_median(.data[[col_name]])) %>% 
        pull(base)
      
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
        
        raw <- x %>%
          get_x_period(the_subid, the_dttm_label, ., lead, period_duration) %>% 
          summarise("raw" := period_median(.data[[col_name]])) %>%
          pull(raw)
        
        passive_label <- if_else(passive, "passive", "NA")
        
        tibble(
          "{data_type_value}.p{period_duration}.l{lead}.rmedian_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw,
          "{data_type_value}.p{period_duration}.l{lead}.dmedian_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw - baseline) %>% 
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


score_max <- function(the_subid, the_dttm_label, x_all, 
                         period_durations, lead, data_start, 
                         col_name, data_type_col_name = NA, data_type_values = NA, 
                         context_col_name = NA, context_values = NA, passive = FALSE) {
  
  # Gets value for col_name and returns a raw max, a max change, and a proportion change in max from baseline
  # i.e., (raw mean - baseline mean) / baseline mean
  # proportion score set to NA if baseline == 0
  
  # the_subid: single integer subid
  # the_dttm_label: single dttm for label onset
  # x_all:  raw data for all subids and communications
  # period_durations: vector of 1+ integer period_durations in hours
  # lead: the lead time for prediction in hours (a single integer)
  # data_start: a df with data_start = min(study_start, comm_start) for all subids
  # col_name: column name for raw data for feature as string - should be continuous var
  # data_type_col_name: name of column name to filter on for data log type values
  # data_type_values: a vector of 1+ meta data log types to filter on (sms, or voice, if empty uses all logs)
  # context_col_name: col_name of context feature. Set to NA if no context filter
  # context_values: a vector of 1+ context values to filter on.  Set to NA if no context filter
  # passive: is a variable to distinguish variables that use no context and to append passive
  # onto those variable names for filtering down feature sets in recipes (set to TRUE if passive)
  
  # define max function
  period_max <- function (.x) {
    if (length(.x) > 0) { 
      if (!all(is.na(.x))) {
        the_max <- max(.x, na.rm = TRUE)
      } else the_max <- NA
    } else the_max <- NA
    
    return(the_max)
  }
  
  # define mean function (for baseline change)
  period_mean <- function (.x) {
    if (length(.x) > 0) { 
      if (!all(is.na(.x))) {
        the_mean <- mean(.x, na.rm = TRUE)
      } else the_mean <- NA
    } else the_mean <- NA
    
    return(the_mean)
  }
  
  # nested foreach - period_duration within data_type_value within context_value
  
  features <- foreach (context_value = context_values, .combine = "cbind") %do% {
    
    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all
  
    
    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c
      
      # get baseline mean using all data before label dttm
      baseline <- x %>% 
        get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = Inf) %>% # Inf gives all data back to first obs
        summarise("base" := period_mean(.data[[col_name]])) %>% 
        pull(base)
      
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
        
        raw_max <- x %>%
          get_x_period(the_subid, the_dttm_label, ., lead, period_duration) %>% 
          summarise("raw" := period_max(.data[[col_name]])) %>%
          pull(raw)
        
        passive_label <- if_else(passive, "passive", "NA")
        
        tibble(
          "{data_type_value}.p{period_duration}.l{lead}.rmax_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw_max,
          "{data_type_value}.p{period_duration}.l{lead}.dmax_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw_max - baseline) %>% 
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

score_min <- function(the_subid, the_dttm_label, x_all, 
                      period_durations, lead, data_start, 
                      col_name, data_type_col_name = NA, data_type_values = NA, 
                      context_col_name = NA, context_values = NA, passive = FALSE) {
  
  # Gets value for col_name and returns a raw min, a min change, and a proportion change in min from baseline
  # i.e., (raw mean - baseline mean) / baseline mean
  # proportion score set to NA if baseline == 0
  
  # the_subid: single integer subid
  # the_dttm_label: single dttm for label onset
  # x_all:  raw data for all subids and communications
  # period_durations: vector of 1+ integer period_durations in hours
  # lead: the lead time for prediction in hours (a single integer)
  # data_start: a df with data_start = min(study_start, comm_start) for all subids
  # col_name: column name for raw data for feature as string - should be continuous var
  # data_type_col_name: name of column name to filter on for data log type values
  # data_type_values: a vector of 1+ meta data log types to filter on (sms, or voice, if empty uses all logs)
  # context_col_name: col_name of context feature. Set to NA if no context filter
  # context_values: a vector of 1+ context values to filter on.  Set to NA if no context filter
  # passive: is a variable to distinguish variables that use no context and to append passive
  # onto those variable names for filtering down feature sets in recipes (set to TRUE if passive)
  
  
  # define min function
  period_min <- function (.x) {
    if (length(.x) > 0) { 
      if (!all(is.na(.x))) {
        the_min <- min(.x, na.rm = TRUE)
      } else the_min <- NA
    } else the_min <- NA
    
    return(the_min)
  }
  
  # define mean function (for baseline change)
  period_mean <- function (.x) {
    if (length(.x) > 0) { 
      if (!all(is.na(.x))) {
        the_mean <- mean(.x, na.rm = TRUE)
      } else the_mean <- NA
    } else the_mean <- NA
    
    return(the_mean)
  }
  
  # nested foreach - period_duration within data_type_value within context_value
  
  features <- foreach (context_value = context_values, .combine = "cbind") %do% {
    
    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all
    
    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c
      
      # get baseline mean using all data before label dttm
      baseline <- x %>% 
        get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = Inf) %>% # Inf gives all data back to first obs
        summarise("base" := period_mean(.data[[col_name]])) %>% 
        pull(base)
      
      foreach (period_duration = period_durations, .combine = "cbind") %do% {
        
        raw <- x %>%
          get_x_period(the_subid, the_dttm_label, ., lead, period_duration) %>% 
          summarise("raw" := period_min(.data[[col_name]])) %>%
          pull(raw)
        
        passive_label <- if_else(passive, "passive", "NA")
        
        tibble(
          "{data_type_value}.p{period_duration}.l{lead}.rmin_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw,
          "{data_type_value}.p{period_duration}.l{lead}.dmin_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw - baseline) %>% 
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

score_most_recent <- function(the_subid, the_dttm_label, x_all, 
                              lead, data_start, 
                              col_name, data_type_col_name = NA, data_type_values = NA, 
                              context_col_name = NA, context_values = NA, passive = FALSE) {
  
  
  
  # Gets most recent value (after considering lead) for col_name and returns it and a change of it vs baseline, 
  
  # the_subid: single integer subid
  # the_dttm_label: single dttm for label onset
  # x_all:  raw data for all subids and communications
  # lead: the lead time for prediction in hours (a single integer)
  # data_start: a df with data_start = min(study_start, comm_start) for all subids
  # col_name: column name for raw data for feature as string - should be continuous var
  # data_type_col_name: name of column name to filter on for data log type values
  # data_type_values: a vector of 1+ meta data log types to filter on (sms, or voice, if empty uses all logs)
  # context_col_name: col_name of context feature. Set to NA if no context filter
  # context_values: a vector of 1+ context values to filter on.  Set to NA if no context filter
  # passive: is a variable to distinguish variables that use no context and to append passive
  # onto those variable names for filtering down feature sets in recipes (set to TRUE if passive)
  
  # define mean function (for baseline change)
  period_mean <- function (.x) {
    if (length(.x) > 0) { 
      if (!all(is.na(.x))) {
        the_mean <- mean(.x, na.rm = TRUE)
      } else the_mean <- NA
    } else the_mean <- NA
    
    return(the_mean)
  }
  
  
  # nested foreach - period_duration within data_type_value within context_value
  
  features <- foreach (context_value = context_values, .combine = "cbind") %do% {
    
    # Filter data if context_value provided
    if (!is.na(context_value)) {
      x_c <- x_all %>%
        filter(.data[[context_col_name]] == context_value) 
    } else x_c <- x_all
    
    foreach (data_type_value = data_type_values, .combine = "cbind") %do% {
      
      if (!is.na(data_type_value)) {
        x <- x_c %>% filter(.data[[data_type_col_name]] == data_type_value) 
      } else x <- x_c
      
      # get baseline mean using all data before label dttm
      baseline <- x %>% 
        get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = Inf) %>% # Inf gives all data back to first obs
        summarise("base" := period_mean(.data[[col_name]])) %>% 
        pull(base)
      

        
      raw <- x %>%
        get_x_period(the_subid, the_dttm_label, ., lead, Inf) %>%   # use Inf to get all data
        arrange(desc(dttm_obs)) %>% 
        slice(1) %>%   # slice to most recent (after arrange)
        pull((!!sym(col_name)))
      
      if (length(raw) == 0) raw <- NA  # if no recent data exists
      
      passive_label <- if_else(passive, "passive", "NA")
      
      tibble(
        "{data_type_value}.p0.l{lead}.rrecent_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw,
        "{data_type_value}.p0.l{lead}.drecent_{col_name}.{context_col_name}.{context_value}.{passive_label}" := raw - baseline) %>% 
        rename_with(~str_remove_all(.x, ".NA")) %>% 
        rename_with(~str_remove(.x, "^NA."))
      
    }
  }
  
  features <- features %>%
    mutate(subid = the_subid,
           dttm_label = the_dttm_label) %>%
    relocate(subid, dttm_label)
  
  return(features)  
  
  
}

score_label_day <- function(the_subid, the_dttm_label, the_tz = "America/Chicago") {
#  returns the day of the week (string) for the label  
  
  day <- the_dttm_label %>% 
    with_tz(tzone = the_tz) %>% 
    wday(label = TRUE) %>% 
    as.character()
  
  features <- tibble(subid = the_subid,
                     dttm_label = the_dttm_label,
                     label_day = day)
  
  return(features)  
  
}

score_label_hour <- function(the_subid, the_dttm_label, levels = 24, the_tz = "America/Chicago") {
  #  returns the hour of the day  (as string) for the label  
  # levels = 24 for every hour, levels = 2 for 5 - midnight vs. midnight to 5 pm
  hour <- the_dttm_label %>% 
    with_tz(tzone = the_tz) %>% 
    hour()
  
  if (levels == 2) {
    hour <- if_else(hour >= 17 & hour <= 23, "evening", "other")
  } else {
    hour <- as.character(hour) # treat as nominal variable
  }
  
  features <- tibble(subid = the_subid,
                     dttm_label = the_dttm_label,
                     label_hour = hour)
  
  return(features)  
  
}

