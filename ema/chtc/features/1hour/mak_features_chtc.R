# Script to engineer EMA features on CHTC

# Constants: EDIT
window <- "1hour"  # window for calculating labels
period_durations <- c(12, 24, 48, 72, 168) # feature duration window
lead <-  0 # feature lead time


suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(vroom)
  library(foreach)
  library(tidyr) # for pivot_wider.  Using train.tar as kludge b/c it currently contains tidyr
  source("fun_features.R")
})

# get chtc process num
args <- commandArgs(trailingOnly = TRUE) 
# job_start <- 1
# job_stop <- 10
job_start <- as.numeric(args[1]) # CHTC arg starts at 1 because using passed in row numbers
job_stop <- as.numeric(args[2])

# read in ema
ema <- vroom("ema.csv", show_col_types = FALSE) %>% 
  mutate(dttm_obs = with_tz(dttm_obs, tz = "America/Chicago"),
         ema_1 = if_else(ema_1 == "No", 1, 2), # cant use 0
         ema_2 = ema_2 + 1,
         ema_3 = ema_3 + 1,
         ema_4 = ema_4 + 1,
         ema_5 = ema_5 + 1)  %>% 
  arrange(subid, dttm_obs)

# pivot longer to allow feature function to loop over EMA items across rows
ema_long <- ema %>% 
  pivot_longer(
    cols = starts_with("ema_"),
    names_to = "ema_num",
    values_to = "response"
  )

lapses <- vroom("lapses.csv", show_col_types = FALSE) %>% 
  mutate(dttm_obs = with_tz(dttm_obs, tz = "America/Chicago"))
  
labels <- vroom("labels.csv", show_col_types = FALSE) %>% 
  mutate(dttm_label = with_tz(dttm_label, tz = "America/Chicago")) %>% 
  slice(job_start:job_stop) %>% 
  mutate(label_num = seq(job_start, job_stop, by = 1))

dates <- vroom("study_dates.csv", show_col_types = FALSE) %>% 
  select(subid, data_start = study_start) %>% 
  mutate(data_start = with_tz(data_start, tz = "America/Chicago"))


# make features ------------------
# i_label <- 1   # for testing
features <- foreach (i_label = 1:nrow(labels), .combine = "rbind") %do% {
  # message(i_label)  # for testing
  label <- labels %>% slice(i_label)
  subid <- label$subid 
  dttm_label <-  label$dttm_label
  the_label_num <- label$label_num
  
  # rate of previous lapses
  feature_row <- score_ratecount_value(the_subid = subid, 
                                      the_dttm_label = dttm_label, 
                                      x_all = lapses, 
                                      period_durations  = period_durations, lead = lead, 
                                      data_start = dates, 
                                      col_name = "count", 
                                      col_values = "lapse")
  
  

  # median; use because may be more stable than mean
  # ema_1 not included b/c handled by lapses above
  feature_row <- feature_row %>%
    full_join(score_median(the_subid = subid,
                           the_dttm_label = dttm_label,
                           x_all  = ema_long,
                           period_durations = period_durations,
                           lead = lead,
                           data_start = dates,
                           col_name = "response",
                           data_type_col_name = "ema_num",
                           data_type_values = str_c("ema_", 2:10)),
              by = c("subid", "dttm_label"))

  # max
  feature_row <- feature_row %>%
    full_join(score_max(the_subid = subid,
                        the_dttm_label = dttm_label,
                        x_all  = ema_long,
                        period_durations = period_durations,
                        lead = lead,
                        data_start = dates,
                        col_name = "response",
                        data_type_col_name = "ema_num",
                        data_type_values = str_c("ema_", 1:10)),
              by = c("subid", "dttm_label"))

  # min
  feature_row <- feature_row %>%
    full_join(score_min(the_subid = subid,
                        the_dttm_label = dttm_label,
                        x_all  = ema_long,
                        period_durations = period_durations,
                        lead = lead,
                        data_start = dates,
                        col_name = "response",
                        data_type_col_name = "ema_num",
                        data_type_values = str_c("ema_", 1:10)),
              by = c("subid", "dttm_label"))
  
  feature_row <- feature_row %>% 
    mutate(label_num = the_label_num)
  
  feature_row
}

# Add outcome label and other info to features
features %>%
  mutate(lapse = labels$lapse) %>% 
  relocate(label_num, subid, dttm_label, lapse) %>% 
  vroom_write(str_c("features_", window, "_", job_start, "_", job_stop, ".csv"), delim = ",")
