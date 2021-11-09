# Script to engineer features on CHTC

suppressPackageStartupMessages({
  require(dplyr)
  require(readr)
  require(tidyr)
  require(stringr)
  library(lubridate)
  library(tis)
  
  source("fun_risk.R")
})

# get chtc process num ------------------
# label_num <- 1
args <- commandArgs(trailingOnly = TRUE) 
label_num <- as.numeric(args[1]) # CHTC arg starts at 1 because using passed in integers

# read in data ------------------
logs_all <- read_rds("data.rds") %>% 
  mutate(dttm_obs = with_tz(dttm_obs, tzone = "America/Chicago")) 

labels_05 <- read_rds("labels.rds") %>% 
  mutate(dttm_label = with_tz(dttm_label, tzone = "America/Chicago")) 

data_start <- read_rds("study_dates.rds") %>% 
  select(subid, study_start, comm_start, data_start) %>% 
  mutate(across(study_start:data_start), with_tz(., tz = "America/Chicago")) 

screen_features <- read_rds("static_features.rds")


# Slice out label based on label_num ------------------
label <- slice(labels_05, label_num)



# initialize period durations and lead hours ------------------
period_durations <- c(6, 12, 24, 48, 72, 168) 
lead <-  0 


# make features ------------------

# incoming/outgoing communications 
# on own 
meta_features <- score_ratecount_value(label$subid, 
                          label$dttm_label,
                          x_all  = logs_all,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = data_start, 
                          col_name = "org", 
                          col_values = c("incoming", "outgoing"), 
                          data_type_col_name = "log_type",
                          data_type_values = c("sms", "voi"),
                          passive = TRUE)


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                     label$dttm_label,
                     x_all  = logs_all,
                     period_durations = period_durations,
                     lead = lead, 
                     col_name = "org", 
                     col_values = c("incoming", "outgoing"), 
                     data_type_col_name = "log_type",
                     data_type_values = c("sms", "voi"),
                     passive = TRUE), by = c("subid", "dttm_label"))

# with context
meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                     label$dttm_label,
                     x_all  = logs_all,
                     period_durations = period_durations,
                     lead = lead, 
                     data_start = data_start, 
                     col_name = "org", 
                     col_values = c("incoming", "outgoing"), 
                     data_type_col_name = "log_type",
                     data_type_values = c("sms", "voi"),
                     context_col_name = "drinker",
                     context_values = c("yes", "no")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                     label$dttm_label,
                     x_all  = logs_all,
                     period_durations = period_durations,
                     lead = lead, 
                     col_name = "org", 
                     col_values = c("incoming", "outgoing"), 
                     data_type_col_name = "log_type",
                     data_type_values = c("sms", "voi"),
                     context_col_name = "drinker",
                     context_values = c("yes", "no")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "org", 
                                  col_values = c("incoming", "outgoing"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "drnk_past",
                                  context_values = c("always_almost_always", "occasionally", "never_almost_never")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "org", 
                                  col_values = c("incoming", "outgoing"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "drnk_past",
                                  context_values = c("always_almost_always", "occasionally", "never_almost_never")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "org", 
                                  col_values = c("incoming", "outgoing"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "drnk_presence",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "org", 
                                  col_values = c("incoming", "outgoing"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "drnk_presence",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "org", 
                                  col_values = c("incoming", "outgoing"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "recovery",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "org", 
                                  col_values = c("incoming", "outgoing"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "recovery",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                     label$dttm_label,
                     x_all  = logs_all,
                     period_durations = period_durations,
                     lead = lead, 
                     data_start = data_start, 
                     col_name = "org", 
                     col_values = c("incoming", "outgoing"), 
                     data_type_col_name = "log_type",
                     data_type_values = c("sms", "voi"),
                     context_col_name = "supportive",
                     context_values = c("supportive", "unsupportive", "mixed")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                     label$dttm_label,
                     x_all  = logs_all,
                     period_durations = period_durations,
                     lead = lead, 
                     col_name = "org", 
                     col_values = c("incoming", "outgoing"), 
                     data_type_col_name = "log_type",
                     data_type_values = c("sms", "voi"),
                     context_col_name = "supportive",
                     context_values = c("supportive", "unsupportive", "mixed")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "org", 
                                  col_values = c("incoming", "outgoing"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "experience",
                                  context_values = c("pleasant", "unpleasant", "mixed", "neutral")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "org", 
                                  col_values = c("incoming", "outgoing"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "experience",
                                  context_values = c("pleasant", "unpleasant", "mixed", "neutral")), by = c("subid", "dttm_label")) 


# context alone 
meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "drnk_past", 
                                  col_values = c("always_almost_always", "occasionally", "never_almost_never"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "drnk_past", 
                                  col_values = c("always_almost_always", "occasionally", "never_almost_never"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "drinker", 
                                  col_values = c("no", "yes"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "drinker", 
                                  col_values = c("no", "yes"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "drnk_presence", 
                                  col_values = c("no", "yes"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "drnk_presence", 
                                  col_values = c("no", "yes"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                     label$dttm_label,
                     x_all  = logs_all,
                     period_durations = period_durations,
                     lead = lead, 
                     data_start = data_start, 
                     col_name = "recovery", 
                     col_values = c("no", "yes"), 
                     data_type_col_name = "log_type",
                     data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                     label$dttm_label,
                     x_all  = logs_all,
                     period_durations = period_durations,
                     lead = lead, 
                     col_name = "recovery", 
                     col_values = c("no", "yes"), 
                     data_type_col_name = "log_type",
                     data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "supportive", 
                                  col_values = c("supportive", "unsupportive", "mixed"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "supportive", 
                                  col_values = c("supportive", "unsupportive", "mixed"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "experience", 
                                  col_values = c("pleasant", "unpleasant", "mixed", "neutral"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "experience", 
                                  col_values = c("pleasant", "unpleasant", "mixed", "neutral"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 

# call duration
# on own
meta_features <- meta_features %>% 
  full_join(score_ratesum(label$subid, 
                                 label$dttm_label,
                                 x_all  = logs_all,
                                 period_durations = period_durations,
                                 lead = lead, 
                                 data_start = data_start, 
                                 col_name = "duration",
                                 data_type_col_name = "log_type",
                                 data_type_values = c("voi"),
                                 passive = TRUE), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_mean(label$subid, 
                          label$dttm_label,
                          x_all  = logs_all,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = data_start, 
                          col_name = "duration",
                          data_type_col_name = "log_type",
                          data_type_values = c("voi"),
                          passive = TRUE), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "duration", 
                                  col_values = c(0), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("voi"),
                                  passive = TRUE), by = c("subid", "dttm_label"))

# by incoming/outgoing
meta_features <- meta_features %>% 
  full_join(score_ratesum(label$subid, 
                                 label$dttm_label,
                                 x_all  = logs_all,
                                 period_durations = period_durations,
                                 lead = lead, 
                                 data_start = data_start, 
                                 col_name = "duration", 
                                 data_type_col_name = "log_type",
                                 data_type_values = c("voi"),
                                 context_col_name = "org",
                                 context_values = c("incoming", "outgoing"),
                                 passive = TRUE), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_mean(label$subid, 
                          label$dttm_label,
                          x_all  = logs_all,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = data_start, 
                          col_name = "duration", 
                          data_type_col_name = "log_type",
                          data_type_values = c("voi"),
                          context_col_name = "org",
                          context_values = c("incoming", "outgoing"),
                          passive = TRUE), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "duration", 
                                  col_values = c(0), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("voi"),
                                  context_col_name = "org",
                                  context_values = c("incoming", "outgoing"),
                                  passive = TRUE), by = c("subid", "dttm_label"))

# with context
meta_features <- meta_features %>% 
  full_join(score_ratesum(label$subid, 
                                 label$dttm_label,
                                 x_all  = logs_all,
                                 period_durations = period_durations,
                                 lead = lead, 
                                 data_start = data_start, 
                                 col_name = "duration", 
                                 data_type_col_name = "log_type",
                                 data_type_values = c("voi"),
                                 context_col_name = "drnk_past",
                                 context_values = c("always_almost_always", "never_almost_never", "occasionally")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_mean(label$subid, 
                          label$dttm_label,
                          x_all  = logs_all,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = data_start, 
                          col_name = "duration", 
                          data_type_col_name = "log_type",
                          data_type_values = c("voi"),
                          context_col_name = "drnk_past",
                          context_values = c("always_almost_always", "never_almost_never", "occasionally")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "duration", 
                                  col_values = c(0), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("voi"),
                                  context_col_name = "drnk_past",
                                  context_values = c("always_almost_always", "never_almost_never", "occasionally")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_ratesum(label$subid, 
                                 label$dttm_label,
                                 x_all  = logs_all,
                                 period_durations = period_durations,
                                 lead = lead, 
                                 data_start = data_start, 
                                 col_name = "duration", 
                                 data_type_col_name = "log_type",
                                 data_type_values = c("voi"),
                                 context_col_name = "drinker",
                                 context_values = c("yes", "no")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_mean(label$subid, 
                          label$dttm_label,
                          x_all  = logs_all,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = data_start, 
                          col_name = "duration", 
                          data_type_col_name = "log_type",
                          data_type_values = c("voi"),
                          context_col_name = "drinker",
                          context_values = c("yes", "no")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "duration", 
                                  col_values = c(0), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("voi"),
                                  context_col_name = "drinker",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_ratesum(label$subid, 
                                 label$dttm_label,
                                 x_all  = logs_all,
                                 period_durations = period_durations,
                                 lead = lead, 
                                 data_start = data_start, 
                                 col_name = "duration", 
                                 data_type_col_name = "log_type",
                                 data_type_values = c("voi"),
                                 context_col_name = "drnk_presence",
                                 context_values = c("yes", "no")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_mean(label$subid, 
                          label$dttm_label,
                          x_all  = logs_all,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = data_start, 
                          col_name = "duration", 
                          data_type_col_name = "log_type",
                          data_type_values = c("voi"),
                          context_col_name = "drnk_presence",
                          context_values = c("yes", "no")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "duration", 
                                  col_values = c(0), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("voi"),
                                  context_col_name = "drnk_presence",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_ratesum(label$subid, 
                                 label$dttm_label,
                                 x_all  = logs_all,
                                 period_durations = period_durations,
                                 lead = lead, 
                                 data_start = data_start, 
                                 col_name = "duration", 
                                 data_type_col_name = "log_type",
                                 data_type_values = c("voi"),
                                 context_col_name = "recovery",
                                 context_values = c("yes", "no")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_mean(label$subid, 
                          label$dttm_label,
                          x_all  = logs_all,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = data_start, 
                          col_name = "duration", 
                          data_type_col_name = "log_type",
                          data_type_values = c("voi"),
                          context_col_name = "recovery",
                          context_values = c("yes", "no")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "duration", 
                                  col_values = c(0), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("voi"),
                                  context_col_name = "recovery",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_ratesum(label$subid, 
                                 label$dttm_label,
                                 x_all  = logs_all,
                                 period_durations = period_durations,
                                 lead = lead, 
                                 data_start = data_start, 
                                 col_name = "duration", 
                                 data_type_col_name = "log_type",
                                 data_type_values = c("voi"),
                                 context_col_name = "supportive",
                                 context_values = c("supportive", "unsupportive", "mixed")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_mean(label$subid, 
                          label$dttm_label,
                          x_all  = logs_all,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = data_start, 
                          col_name = "duration", 
                          data_type_col_name = "log_type",
                          data_type_values = c("voi"),
                          context_col_name = "supportive",
                          context_values = c("supportive", "unsupportive", "mixed")), by = c("subid", "dttm_label")) 


meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "duration", 
                                  col_values = c(0), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("voi"),
                                  context_col_name = "supportive",
                                  context_values = c("supportive", "unsupportive", "mixed")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_ratesum(label$subid, 
                                 label$dttm_label,
                                 x_all  = logs_all,
                                 period_durations = period_durations,
                                 lead = lead, 
                                 data_start = data_start, 
                                 col_name = "duration", 
                                 data_type_col_name = "log_type",
                                 data_type_values = c("voi"),
                                 context_col_name = "experience",
                                 context_values = c("pleasant", "unpleasant", "mixed", "neutral")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_mean(label$subid, 
                          label$dttm_label,
                          x_all  = logs_all,
                          period_durations = period_durations,
                          lead = lead, 
                          data_start = data_start, 
                          col_name = "duration", 
                          data_type_col_name = "log_type",
                          data_type_values = c("voi"),
                          context_col_name = "experience",
                          context_values = c("pleasant", "unpleasant", "mixed", "neutral")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "duration", 
                                  col_values = c(0), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("voi"),
                                  context_col_name = "experience",
                                  context_values = c("pleasant", "unpleasant", "mixed", "neutral")), by = c("subid", "dttm_label"))



# abbreviate contact_type categories

logs_all <- logs_all %>% 
  mutate(cont_type_abr = case_when(contact_type %in% c("parent", "child", "aunt_uncle",
                                                          "cousin", "family_other", "sibling",
                                                          "grandparent", "significant_other") ~ "family",
                                      contact_type == "friend" ~ "friend",
                                      contact_type == "coworker" ~ "coworker",
                                      contact_type %in% c("counselor", "social_worker") ~ "counselor_socialworker",
                                      contact_type %in% c("irrelevant", "other", "self") ~ "irrelevant_or_unknown",
                                      is.na(contact_type) ~ "irrelevant_or_unknown"))

# cont type 
# own own
meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 


# with context

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "drnk_past",
                                  context_values = c("always_almost_always", "never_almost_never", "occasionally")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "drnk_past",
                                  context_values = c("always_almost_always", "never_almost_never", "occasionally")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                     label$dttm_label,
                     x_all  = logs_all,
                     period_durations = period_durations,
                     lead = lead, 
                     data_start = data_start, 
                     col_name = "cont_type_abr", 
                     col_values = c("family", "friend"), 
                     data_type_col_name = "log_type",
                     data_type_values = c("sms", "voi"),
                     context_col_name = "drinker",
                     context_values = c("yes", "no")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                     label$dttm_label,
                     x_all  = logs_all,
                     period_durations = period_durations,
                     lead = lead, 
                     col_name = "cont_type_abr", 
                     col_values = c("family", "friend"), 
                     data_type_col_name = "log_type",
                     data_type_values = c("sms", "voi"),
                     context_col_name = "drinker",
                     context_values = c("yes", "no")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "drnk_presence",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "drnk_presence",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "recovery",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "recovery",
                                  context_values = c("yes", "no")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "supportive",
                                  context_values = c("supportive", "unsupportive", "mixed")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "supportive",
                                  context_values = c("supportive", "unsupportive", "mixed")), by = c("subid", "dttm_label")) 



meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "experience", 
                                  context_values = c("pleasant", "unpleasant", "neutral", "mixed")), by = c("subid", "dttm_label"))


meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "cont_type_abr", 
                                  col_values = c("family", "friend"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "experience", 
                                  context_values = c("pleasant", "unpleasant", "neutral", "mixed")), by = c("subid", "dttm_label")) 


# other combinations
meta_features <- meta_features %>% 
  full_join(score_ratecount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  data_start = data_start, 
                                  col_name = "drinker", 
                                  col_values = c("yes", "no"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "experience", 
                                  context_values = c("pleasant", "unpleasant", "neutral", "mixed")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_propcount_value(label$subid, 
                                  label$dttm_label,
                                  x_all  = logs_all,
                                  period_durations = period_durations,
                                  lead = lead, 
                                  col_name = "drinker", 
                                  col_values = c("yes", "no"), 
                                  data_type_col_name = "log_type",
                                  data_type_values = c("sms", "voi"),
                                  context_col_name = "experience", 
                                  context_values = c("pleasant", "unpleasant", "neutral", "mixed")), by = c("subid", "dttm_label")) 


# Features from phone numbers ------------------

# Feature engineer numbers (blocked, out of country, n unique numbers, repeated outgoing or incoming calls to/from a single number)



# Features from Dates ------------------

# Rates of communications at certain times, days, and frequency bursts (mins instead of hrs) or bursts of
# outgoing calls or communications w/single contact

# proportion weekend communications - use at least one week for period duration
meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 168],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(wday(get(dttm_col_name), label = TRUE) %in% c("Fri", "Sat", "Sun")), 
                               dttm_description = "fri_sat_sun",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 

# w/context
meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 168],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(wday(get(dttm_col_name), label = TRUE) %in% c("Fri", "Sat", "Sun")), 
                               dttm_description = "fri_sat_sun",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "drinker", 
                               context_values = c("yes", "no")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 168],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(wday(get(dttm_col_name), label = TRUE) %in% c("Fri", "Sat", "Sun")), 
                               dttm_description = "fri_sat_sun",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "drnk_past", 
                               context_values = c("always_almost_always", "never_almost_never", "occasionally")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 168],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(wday(get(dttm_col_name), label = TRUE) %in% c("Fri", "Sat", "Sun")), 
                               dttm_description = "fri_sat_sun",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "drnk_presence", 
                               context_values = c("no", "yes")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 168],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(wday(get(dttm_col_name), label = TRUE) %in% c("Fri", "Sat", "Sun")), 
                               dttm_description = "fri_sat_sun",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "recovery", 
                               context_values = c("no", "yes")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 168],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(wday(get(dttm_col_name), label = TRUE) %in% c("Fri", "Sat", "Sun")), 
                               dttm_description = "fri_sat_sun",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "supportive", 
                               context_values = c("supportive", "unsupportive", "mixed", "neutral")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 168],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(wday(get(dttm_col_name), label = TRUE) %in% c("Fri", "Sat", "Sun")), 
                               dttm_description = "fri_sat_sun",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "experience", 
                               context_values = c("pleasant", "unpleasant", "mixed", "neutral")), by = c("subid", "dttm_label"))

# incoming vs outgoing
meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 168],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(wday(get(dttm_col_name), label = TRUE) %in% c("Fri", "Sat", "Sun")), 
                               dttm_description = "fri_sat_sun",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "org", 
                               context_values = c("incoming", "outgoing"),
                               passive = TRUE), by = c("subid", "dttm_label"))


# proportion of evening calls - use at least 24 hours for period duration
meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 24],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(hour(get(dttm_col_name)) %in% c(19, 20, 21, 22, 23, 24, 1)), 
                               dttm_description = "7pm_to_1am",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi")), by = c("subid", "dttm_label")) 

# w/context

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 24],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(hour(get(dttm_col_name)) %in% c(19, 20, 21, 22, 23, 24, 1)), 
                               dttm_description = "7pm_to_1am",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "drinker", 
                               context_values = c("yes", "no")), by = c("subid", "dttm_label")) 

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 24],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(hour(get(dttm_col_name)) %in% c(19, 20, 21, 22, 23, 24, 1)), 
                               dttm_description = "7pm_to_1am",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "drnk_past", 
                               context_values = c("always_almost_always", "never_almost_never", "occasionally")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 24],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(hour(get(dttm_col_name)) %in% c(19, 20, 21, 22, 23, 24, 1)), 
                               dttm_description = "7pm_to_1am",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "drnk_presence", 
                               context_values = c("no", "yes")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 24],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(hour(get(dttm_col_name)) %in% c(19, 20, 21, 22, 23, 24, 1)), 
                               dttm_description = "7pm_to_1am",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "recovery", 
                               context_values = c("no", "yes")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 24],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(hour(get(dttm_col_name)) %in% c(19, 20, 21, 22, 23, 24, 1)), 
                               dttm_description = "7pm_to_1am",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "supportive", 
                               context_values = c("supportive", "unsupportive", "mixed", "neutral")), by = c("subid", "dttm_label"))

meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 24],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(hour(get(dttm_col_name)) %in% c(19, 20, 21, 22, 23, 24, 1)), 
                               dttm_description = "7pm_to_1am",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "experience", 
                               context_values = c("pleasant", "unpleasant", "mixed", "neutral")), by = c("subid", "dttm_label"))

# incoming vs outgoing
meta_features <- meta_features %>% 
  full_join(score_propdatetime(label$subid, 
                               label$dttm_label,
                               x_all  = logs_all,
                               period_durations = period_durations[period_durations >= 24],
                               lead = lead, 
                               dttm_col_name = "dttm_obs", 
                               dttm_window = quote(hour(get(dttm_col_name)) %in% c(19, 20, 21, 22, 23, 24, 1)), 
                               dttm_description = "7pm_to_1am",
                               data_type_col_name = "log_type",
                               data_type_values = c("sms", "voi"),
                               context_col_name = "org", 
                               context_values = c("incoming", "outgoing"),
                               passive = TRUE), by = c("subid", "dttm_label"))

# Additional feature engineering ------------------

# Pull other static features from context - Does someone have a significant other that is a drinker, 
# how many supportive contacts do they have?
# Add general screenshot descriptives about social network - i.e., unique number of supportive contacts overall, 
# number of family members that drink that they communicate with, how often do they communicate with friends vs co-workers vs 
# family

# combine response options within context variables?


# Add screen variables to feature set ------------------
meta_features <- meta_features %>%
  left_join(screen_features, by = "subid") 


# Feature engineering based on temporal info inherent in lapse label ------------------
# Days since quit date
meta_features <- meta_features %>%
  mutate(label_days_since_quit_date = round(as.numeric(difftime(dttm_label, id_quit_date, units = "days")), 2)) %>%
  select(-id_quit_date)


# Day of week and time of lapse
meta_features <- meta_features %>%
  rowwise() %>%
  mutate(label_weekday = wday(dttm_label, label = TRUE),
         label_hour = hour(dttm_label),
         label_hour = case_when(label_hour == 0 ~ 24,
                                TRUE ~ as.numeric(label_hour))) %>%
  ungroup()


# 
# Holidays (+/- 1 day)
# Note: only using federal holidays and easter for now
# Federal law defines 10 holidays. Four of them, NewYears, Independence, Veterans and Christmas, fall on the same date every year. 
# The other six fall on particular days of the week and months (MLK, Presidents, Memorial, Labor, Columbus, and Thanksgiving).

meta_features <- meta_features %>%
  mutate(label_holiday = if_else(isHoliday(date(dttm_label), businessOnly = FALSE) |
                                   isHoliday(date(dttm_label) + days(1), businessOnly = FALSE) |
                                   isHoliday(date(dttm_label) - days(1), businessOnly  = FALSE) |
                                   isEaster(date(dttm_label)) |
                                   isEaster(date(dttm_label) + days(1)) |
                                   isEaster(date(dttm_label) - days(1)),
                                 "yes", "no")) 


# Other holidays to consider:
# 
# - mothers/fathers day (different date every year)
# - Halloween
# - personalized important days (Most are labeled as "Other" so unclear what significance is in these days)
# - Jewish Holidays


# Seasons
# Spring: Starts on March 20, and ends on June 20.
# Summer: Begins on June 21, and lasts until September 21.
# Fall: Goes from September 22 until December 20.
# Winter: Starts on December 21, and lasts until March 19.

meta_features <- meta_features %>%
  mutate(month = lubridate::month(dttm_label),
         day = lubridate::day(dttm_label),
         label_season = case_when(month %in% c(4, 5) | (month == 6 & day < 21) | (month == 3 & day >= 20) ~ "Spring",
                                  month %in% c(7, 8) | (month == 9 & day < 22) | (month == 6 & day >= 21) ~ "Summer",
                                  month %in% c(10, 11) | (month == 12 & day < 21) | (month == 9 & day >= 22) ~ "Fall",
                                  month %in% c(1, 2) | (month == 3 & day < 20) | (month == 12 & day >= 21) ~ "Winter",
                                  TRUE ~ NA_character_)) %>%
  select(-c(month, day)) 




# Add outcome label (lapse/no lapse) back in ------------------
meta_features <- meta_features %>%
  left_join(labels_05 %>%
              mutate(label = dplyr::recode(label, "lapse" = "yes", "no_lapse" = "no")),
            by = c("subid", "dttm_label"))

# add job num to row ------------------
meta_features <- meta_features %>% 
  mutate(label_num = label_num) %>% 
  relocate(label_num)

# save row as rds ------------------
meta_features %>% 
  write_rds(., str_c("features_", label_num, ".rds"))




