library(readxl)
library(tidyverse)  # local functions so fine to load all tidyverse packages
library(purrr)
library(lubridate)
library(foreach)

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



get_study_hours <- function(subid, study_start, study_end, ema_end, buffer_hours = 0) {
  # old function before we started using get_label_windows
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

get_label_windows <- function(subid, study_start, study_end, ema_end, 
                              buffer_start = 0, window_dur = 3600) {
  # Function: Returns a tibble for one subject with columns for subid (numeric) and 
  #   dttm_label (dttm).  
  # Inputs:
  #   study_start, study_end, ema_end = dttm vars that should be in America/Chicago
  #   buffer_start = adds buffer time in seconds between start_time and first lapses used 
  #     First hour for lapses is midnight on study day 1 by default (0)
  #   window_dur = duration of lapse window in seconds. Defaults to 1 hour
  
  # calculate start_time 
  start_time <- study_start + seconds(buffer_start)
  
  # calculate end_time in 3 steps
  #   1. get study end (11 pm on study_end date)
  study_end <- study_end + (hours(23)) 
  
  #   2. get ema end (rounded down to nearest hour)
  ema_end <-  floor_date(ema_end, unit = "hours") # - hours(1) # John and Kendra removed bc seems unnecessary 
  
  #   3. calculate end time as earliest end time (study end or ema end) 
  end_time <- min(study_end, ema_end)
  
  
  # create label windows
  #   1. create sequence of dttm's
  #      3600 represents one hour in seconds - using seconds because more versatile
  #      in seq.POSIXT
  dttm_label <- seq(from = start_time, to = end_time, by = 3600)

  #   2. create tibble for subid
  label_windows <- tibble(subid, dttm_label)
  
  #   3. remove last observation in sequence to not include end_time
  label_windows <- label_windows %>% 
    group_by(subid) %>% 
    slice(1:(n() - ceiling(window_dur/3600))) %>% # removes last epoch in hours
    ungroup()
  
  
  return(label_windows)
}




get_lapse_labels <- function(the_subid, lapses, dates, buffer_start = 0, window_dur = 3600) {
  # Purpose: Returns a tibble of lapse labels that includes subid, dttm_label, and
  #   boolean lapse and no_lapse columns. 
  # Inputs:
  #   dates = tibble that should contain subid, study_start, study_end, ema_end 
  #     dttm vars should be in America/Chicago
  #   buffer_start = adds buffer time in seconds between start_time and first lapses used 
  #     First hour for lapses is midnight on study day 1 by default (0)
  #   window_dur = duration of lapse window in seconds. Defaults to 1 hour
  
  # subids <- unique(dates$subid)
  
  # get label windows
  labels <- dates %>% 
    filter(subid == the_subid) %>% 
    select(subid, study_start, study_end, ema_end) %>% 
    # pass in other parameters for get_label_windows
    mutate(buffer_start = buffer_start,
           window_dur = window_dur) %>% 
    pmap_dfr(~get_label_windows(..1, ..2, ..3, ..4, ..5, ..6)) %>%
    mutate(lapse = FALSE,
           no_lapse = TRUE)


  # Step 1: handle valid lapses
  valid_lapses <- lapses %>%
    filter(subid == the_subid) %>% 
    filter(!exclude)

  # no_match <- 0 # no longer able to use counter for checks since we have different window durations 
  if(nrow(valid_lapses) != 0) {
    for (i in 1:nrow(valid_lapses)) {
      lapse_subid <- valid_lapses$subid[[i]]
      lapse_start <- valid_lapses$lapse_start[[i]]
    
      row_index <- which(labels$subid == lapse_subid & 
                         labels$dttm_label <= lapse_start &
                         lapse_start < labels$dttm_label + duration(window_dur, units = "seconds"))

      # row index will contain more than one for days
      # if (length(row_index) == 1) {
        labels$lapse[row_index] <- TRUE
      # }

      # some extra checks
      # this should not happen
      # if (length(row_index) > 1) {
      #   stop("get_lapse_labels() multiple match; SubID: ", lapse_subid, " Lapse hour: ", lapse_start, " i: ", i)
      # }

      # this can happen if we have a lapse report outside of the study_hours
      # if (length(row_index) == 0) {
      #   no_match <- no_match + 1
      # }
    }
  }

  # final check for lapses
  # if (! (nrow(valid_lapses) == (sum(labels$lapse) + no_match))) {
  #   stop("Not all lapses accounted")
  # }

  # Step 2: Handle no_lapse exclusions for valid lapse periods +- 24 hours.

  # First set an end time for valid lapses that have a missing end time (there are a couple)
  # We will be cautious and set longest valid lapse duration (24 hours)
  valid_lapses <- valid_lapses %>%
    mutate(lapse_end = if_else(is.na(lapse_end),
                               lapse_start + hours(24),
                               lapse_end))

  if(nrow(valid_lapses) != 0) {
    for (i in 1:nrow(valid_lapses)) {

      # exclude lapse +- 24 hours on each side
      lapse_hours_exclude <- seq(valid_lapses$lapse_start[[i]] - hours(24),
                                 valid_lapses$lapse_end[[i]] + hours(24),
                                 by = "hours")
  
      for (lapse_hour_exclude in lapse_hours_exclude) {
        row_index <- which(labels$subid == valid_lapses$subid[[i]] & 
                           labels$dttm_label <= lapse_hour_exclude &
                           lapse_hour_exclude < labels$dttm_label + duration(window_dur, units = "seconds")) 
  
        # Not valid check for day level - think about built in check
        # if (length(row_index) == 1) {
          labels$no_lapse[row_index] <- FALSE
        # }
      }
    }
  }

  # Step 3: Handle no_lapse exclusions for excluded periods with date but no times
  exclusions <- lapses %>%
    filter(subid == the_subid) %>% 
    filter(exclude & is.na(lapse_start) & is.na(lapse_end))

  if(nrow(exclusions) != 0) {
      for (i in 1:nrow(exclusions)) {
      lapse_start <- as_datetime(exclusions$lapse_start_date[[i]],
                                 format = "%m-%d-%Y", tz = "America/Chicago")
      lapse_end <- as_datetime(exclusions$lapse_end_date[[i]],
                               format = "%m-%d-%Y", tz = "America/Chicago")

      # Exclude full date +- 24 hours
      lapse_hours_exclude <- seq(lapse_start - hours(24),
                                 lapse_end + hours(48), # end of day + 24 hours
                                 by = "hours")

        for (lapse_hour_exclude in lapse_hours_exclude) {
          row_index <- which(labels$subid == exclusions$subid[[i]] & 
                             labels$dttm_label <= lapse_hour_exclude &
                             lapse_hour_exclude < labels$dttm_label + duration(window_dur, units = "seconds")) 
      
          # if (length(row_index) == 1) {
            labels$no_lapse[row_index] <- FALSE
          # }
        }
      }
  }

  # Step 4: Handle no_lapse exclusions for very long duration lapses (> 24 hours)
  # Exclude full period +- 24 hours
  exclusions <- lapses %>%
    filter(subid == the_subid) %>% 
    filter(exclude & duration > 24)

  if(nrow(exclusions) != 0) {
    for (i in 1:nrow(exclusions)) {

      # exclude lapse +- 24 hours on each side
      lapse_hours_exclude <- seq(exclusions$lapse_start[[i]] - hours(24),
                                 exclusions$lapse_end[[i]] + hours(24),
                                 by = "hours")
 
      for (lapse_hour_exclude in lapse_hours_exclude) {
        row_index <- which(labels$subid == exclusions$subid[[i]] & 
                           labels$dttm_label <= lapse_hour_exclude &
                           lapse_hour_exclude < labels$dttm_label + duration(window_dur, units = "seconds")) 
      
        # if (length(row_index) == 1) {
          labels$no_lapse[row_index] <- FALSE
        # }
      }
    }
  }

  # Step 5: Handle no_lapse exclusions for negative duration lapses
  # Flip start and end time and then exclude full period +- 24 hours
  exclusions <- lapses %>%
    filter(subid == the_subid) %>% 
    filter(exclude & duration <= 24)

  if(nrow(exclusions) != 0) {
    for (i in 1:nrow(exclusions)) {

      # exclude lapse +- 24 hours on each side
      lapse_hours_exclude <- seq(exclusions$lapse_end[[i]] - hours(24),
                                 exclusions$lapse_start[[i]] + hours(24),
                                 by = "hours")
  
      for (lapse_hour_exclude in lapse_hours_exclude) {
        row_index <- which(labels$subid == exclusions$subid[[i]] & 
                           labels$dttm_label <= lapse_hour_exclude &
                           lapse_hour_exclude < labels$dttm_label + duration(window_dur, units = "seconds")) 
      
        # if (length(row_index) == 1) {
          labels$no_lapse[row_index] <- FALSE
        # }
      }
    }
  }
  
  return(labels)
  
} 


bind_labels <- function(valid_labels, p_lapse = NULL, seed = NULL) {
  # This function filters out no_lapses that are unreliable or we have otherwise
  # chosen not to sample from.
  # Function can also be used to down sample no_lapses further by specifying a lapse:no_lapse
  # proportion. Default is to not down sample since this will often be done in feature engineering.
  # If down sampling seed should be specified for reproducibility

  lapses <- valid_labels %>% 
    filter(lapse) %>% 
    mutate(label = "lapse") %>% 
    select(subid, dttm_label, label)
  
  if (is.null(p_lapse)) {
    # no down sampling
    no_lapses <- valid_labels %>% 
      filter(no_lapse) %>% 
      mutate(label = "no_lapse") %>% 
      select(subid, dttm_label, label)
    
  } else {
    # sets proportion of lapses:no_lapses for random downsampling
    set.seed(seed)
    n_no_lapse <- nrow(lapses) * ((1 / p_lapse) - 1)
    
    no_lapses <- valid_labels %>% 
      filter(no_lapse) %>% 
      mutate(label = "no_lapse") %>% 
      select(subid, dttm_label, label) %>% 
      sample_n(n_no_lapse)
  }

  
  labels <- lapses %>% 
    bind_rows(no_lapses)
  
  return(labels)
  
}


convert_log <- function (log_file) {
  
  # function takes in a text CHTC log file and outputs a tibble with one row per job
  
  # convert text file to tibble
  log <- enframe(log_file, name = NULL, value = "raw_text") 
  
  # pull out job submissions
  job_start <- log %>% 
    filter(str_detect(raw_text, "Job submitted from host:")) 
  
  # get job number
  job_start <- job_start %>% 
    rowwise() %>% 
    mutate(job_num = as.numeric(str_split(raw_text, "\\.")[[1]][2]) + 1) 
  
  # get submission time
  job_start <- job_start %>% 
    mutate(submission_dttm = str_split(str_split(raw_text, "\\) ")[[1]][2], " Job submitted")[[1]][1],
           submission_dttm = as_datetime(submission_dttm, tz = "America/Chicago")) %>% 
    ungroup()
  
  # pull out job executions
  job_execute <- log %>% 
    filter(str_detect(raw_text, "Job executing on host:")) 
  
  # get job number
  job_execute <- job_execute %>% 
    rowwise() %>% 
    mutate(job_num = as.numeric(str_split(raw_text, "\\.")[[1]][2]) + 1) 
  
  # get execution time
  job_execute <- job_execute %>% 
    mutate(execution_dttm = str_split(str_split(raw_text, "\\) ")[[1]][2], " Job executing")[[1]][1],
           execution_dttm = as_datetime(execution_dttm, tz = "America/Chicago")) %>% 
    ungroup()
  
  # only keep last execution time (duplicates may be caused from held and released jobs)
  job_execute <- job_execute %>% 
    group_by(job_num) %>% 
    arrange(desc(execution_dttm)) %>% 
    slice(1) %>% 
    ungroup()
  
  # merge into final log
  final_log <- job_start %>% 
    select(-raw_text) %>% 
    full_join(job_execute %>% 
                select(-raw_text), by = c("job_num"))
  
  # pull out job terminations
  job_terminate <- log %>% 
    filter(str_detect(raw_text, "Job terminated") & str_detect(raw_text, "^[0-9]") )
  
  # get job number
  job_terminate <- job_terminate %>% 
    rowwise() %>% 
    mutate(job_num = as.numeric(str_split(raw_text, "\\.")[[1]][2]) + 1) 
  
  # get termination time
  job_terminate <- job_terminate %>% 
    mutate(termination_dttm = str_split(str_split(raw_text, "\\) ")[[1]][2], " Job terminated")[[1]][1],
           termination_dttm = as_datetime(termination_dttm, tz = "America/Chicago")) %>% 
    ungroup()
  
  # merge into final log
  final_log <- final_log %>% 
    full_join(job_terminate %>% 
                select(-raw_text), by = c("job_num"))
  
  # calculate total run time
  final_log <- final_log %>% 
    mutate(run_time = difftime(termination_dttm, execution_dttm, units = "mins"))
  
  
  # get CPU usage, disk usage, memory usage
  row_indexes <- which(log$raw_text %in% job_terminate$raw_text)
  
  usage <- foreach (i = row_indexes, .combine = "rbind") %do% {
    log_i <- log %>% 
      slice(i:(i + 16)) 
    
    # get cpus
    cpus <- log_i %>% 
      filter(str_detect(raw_text, "Cpus")) %>% 
      pull(raw_text)
    
    cpus <- str_split(cpus, " ") %>% 
      unlist() %>% 
      enframe(name = NULL, value = "split_text") %>% 
      filter(split_text != "\t" & split_text != "") %>% 
      unlist(use.names = FALSE)
    
    cpus_usage <- cpus[3]
    cpus_requested <- cpus[4]
    cpus_allocated <- cpus[5]
    
    # get disk
    disk <- log_i %>% 
      filter(str_detect(raw_text, "Disk")) %>% 
      pull(raw_text)
    
    disk <- str_split(disk, " ") %>% 
      unlist() %>% 
      enframe(name = NULL, value = "split_text") %>% 
      filter(split_text != "\t" & split_text != "") %>% 
      unlist(use.names = FALSE)
    
    disk_usage <- disk[4]
    disk_requested <- disk[5]
    disk_allocated <- disk[6]
    
    # get memory
    memory <- log_i %>% 
      filter(str_detect(raw_text, "Memory")) %>% 
      pull(raw_text)
    
    memory <- str_split(memory, " ") %>% 
      unlist() %>% 
      enframe(name = NULL, value = "split_text") %>% 
      filter(split_text != "\t" & split_text != "") %>% 
      unlist(use.names = FALSE)
    
    memory_usage <- memory[4]
    memory_requested <- memory[5]
    memory_allocated <- memory[6]
    
    # get job num
    job_num <- log_i %>% 
      slice(1) %>% 
      mutate(job_num = as.numeric(str_split(raw_text, "\\.")[[1]][2]) + 1) %>% 
      pull(job_num)
    
    # create tibble
    tibble_i <- tibble(job_num, cpus_usage, cpus_requested, cpus_allocated, 
                       disk_usage, disk_requested, disk_allocated, 
                       memory_usage, memory_requested, memory_allocated) %>% 
      mutate(across(cpus_usage:memory_allocated, ~ as.numeric(.x)))
  }
  
  # merge into final log
  final_log <- final_log %>% 
    full_join(usage, by = "job_num") 
  
  return(final_log)
}



