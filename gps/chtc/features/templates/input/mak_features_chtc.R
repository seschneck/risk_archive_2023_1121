# Script to engineer features on CHTC

# Constants
dist_max <- 50   # only use context if places are within 50 meters

suppressPackageStartupMessages({
  library(dplyr)
  # library(readr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(tis)
  library(vroom)
  source("fun_risk.R")
})

# get chtc process num ------------------
# label_num <- 1
args <- commandArgs(trailingOnly = TRUE) 
label_num <- as.numeric(args[1]) # CHTC arg starts at 1 because using passed in integers

# read in data ------------------
data <- vroom("data.csv.xz")%>% 
  mutate(time = with_tz(time, tz = "America/Chicago")) %>% 
  mutate(duration = difftime(lead(time), time, units = "mins"),
         duration = as.numeric(duration)) %>% 
  rename(dttm_obs = time) %>% 
  filter(dist_context <= dist_max)

labels <- vroom("labels.csv") %>% 
  mutate(dttm_label = with_tz(dttm_label, tz = "America/Chicago"))

dates <- vroom("study_dates.csv") %>% 
  select(subid, data_start = study_start) %>% 
  mutate(data_start = with_tz(data_start, tz = "America/Chicago"))

# Slice out label based on label_num ------------------
label <- slice(labels, label_num)


# initialize period durations and lead hours ------------------
period_durations <- c(6, 12, 24, 48, 72, 168) 
lead <-  0 


# make features ------------------

# type
features <- score_ratesum(label$subid, 
                          label$dttm_label,
                          x_all  = data,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = dates, 
                          col_name = "duration", 
                          context_col_name = "type",
                          context_values = c("aa", "bar", "cafe", "church", "family",
                                             "fitness", "healthcare", "home",
                                             "liquorstore", "park", "restaurant",
                                             "school", "volunteer", "work"))
  
# drank
features <- features %>% 
   full_join(score_ratesum(label$subid, 
                          label$dttm_label,
                          x_all  = data,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = dates, 
                          col_name = "duration",
                          context_col_name = "drank",
                          context_values = c("yes", "no")), 
             by = c("subid", "dttm_label"))

# alcohol
features <- features %>% 
  full_join(score_ratesum(label$subid, 
                          label$dttm_label,
                          x_all  = data,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = dates, 
                          col_name = "duration",
                          context_col_name = "alcohol",
                          context_values = c("yes", "no")), 
            by = c("subid", "dttm_label"))

# emotion
features <- features %>% 
  full_join(score_ratesum(label$subid, 
                          label$dttm_label,
                          x_all  = data,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = dates, 
                          col_name = "duration",
                          context_col_name = "emotion",
                          context_values = c("pleasant", "unpleasant", "mixed", "neutral")), 
            by = c("subid", "dttm_label"))

# risk
features <- features %>% 
  full_join(score_ratesum(label$subid, 
                          label$dttm_label,
                          x_all  = data,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = dates, 
                          col_name = "duration",
                          context_col_name = "risk",
                          context_values = c("high", "medium", "low", "no")), 
            by = c("subid", "dttm_label"))

# avoid
features <- features %>% 
  full_join(score_ratesum(label$subid, 
                          label$dttm_label,
                          x_all  = data,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = dates, 
                          col_name = "duration",
                          context_col_name = "avoid",
                          context_values = c("yes", "no")), 
            by = c("subid", "dttm_label"))


# Add outcome label and other info to features ------------------
features %>%
  mutate(label = label$label) %>% 
  mutate(label_num = label_num) %>% 
  relocate(label_num, subid, dttm_label, label) %>% 
  vroom_write(str_c("features_", label_num, ".csv"), delim = ",")
