# Script to engineer META features on CHTC

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(vroom)
  library(foreach)
  source("fun_features.R")
})

# get chtc process num
args <- commandArgs(trailingOnly = TRUE) 
#job_start <- 2001
#job_stop <- 2002
job_start <- as.numeric(args[1]) # CHTC arg starts at 1 because using passed in row numbers
job_stop <- as.numeric(args[2])

# read in data
data <- vroom("data.csv.xz", show_col_types = FALSE)%>% 
  mutate(time = with_tz(time, tz = "America/Chicago"),
         duration = duration / 60) %>% #convert minutes to hours because period duration units are hours
  rename(dttm_obs = time) %>% 
  filter(dist_context <= dist_max) # currently only care about observations with context
                                   # this may change when need to figure out total duration
                                   # of observations in window in future

labels <- vroom("labels.csv", show_col_types = FALSE) %>% 
  mutate(dttm_label = with_tz(dttm_label, tz = "America/Chicago")) %>% 
  slice(job_start:job_stop) %>% 
  mutate(label_num = seq(job_start, job_stop, by = 1))

dates <- vroom("study_dates.csv", show_col_types = FALSE) %>% 
  select(subid, data_start = study_start) %>% 
  mutate(data_start = with_tz(data_start, tz = "America/Chicago"))


# initialize period durations and lead hours
period_durations <- c(6, 12, 24, 48, 72, 168) 
lead <-  0 

# make features
# i_label <- 1   # for testing

# # Done by hand in feature input file (meta_logs_reduced.csv.xz)
# meta1<- meta1 %>% 
#   mutate(contact_type1 = case_when(contact_type %in% c("parent", "child", "aunt_uncle", "cousin", "family_other", "sibling","grandparent", "significant_other") ~ "family",
#                                    contact_type == "friend" ~ "friend",
#                                    contact_type %in% c("counselor", "social_worker") ~ "counselor_socialworker",
#                                    contact_type %in% c("irrelevant", "other", "self", "coworker") ~ "irrelevant_or_unknown",
#                                    is.na(contact_type) ~ "irrelevant_or_unknown"))
#
# meta1<- meta1 %>% 
#   mutate(supportive1 = case_when(supportive == "supportive" ~ "supportive",
#                                  supportive == "dont_know" ~ "dont_know",
#                                  supportive %in% c("unsupportive", "mixed") ~ "unsupportive_mixed"))
# 
# meta1<- meta1 %>% 
#   mutate(experience1 = case_when(experience == "pleasant" ~ "pleasant",
#                                  experience == "neutral" ~ "neutral",
#                                  experience %in% c("unpleasant", "mixed") ~ "unpleasant_mixed"))

features <- foreach (i_label = 1:nrow(labels), .combine = "rbind") %do% {
  
  label <- labels %>% slice(i_label)
  subid <- label$subid 
  dttm_label <-  label$dttm_label
  the_label_num <- label$label_num
  

  # rate and proportion of calls for context
  
  feature_row <- score_ratecount_value(subid, 
                                       dttm_label,
                                       x_all  = data,
                                       period_durations = period_durations,
                                       lead = lead, 
                                       data_start = dates, 
                                       col_name = "contact_type1", 
                                       col_values = c("counselor_socialworker", "family", "friend"), 
                                       data_type_col_name = "log_type",
                                       data_type_values = c("sms", "voi")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_propcount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    col_name = "contact_type1", 
                                    col_values = c("counselor_socialworker", "family", "friend"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 
  
  feature_row <- feature_row %>% 
    full_join(score_ratecount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    data_start = dates, 
                                    col_name = "drnk_past", 
                                    col_values = c("always_almost_always", "occasionally", "never_almost_never"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_propcount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    col_name = "drnk_past", 
                                    col_values = c("always_almost_always", "occasionally", "never_almost_never"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 
  
  feature_row <- feature_row %>% 
    full_join(score_ratecount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    data_start = dates, 
                                    col_name = "drinker", 
                                    col_values = c("no", "yes"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_propcount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    col_name = "drinker", 
                                    col_values = c("no", "yes"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_ratecount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    data_start = dates, 
                                    col_name = "drnk_presence", 
                                    col_values = c("no", "yes"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_propcount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    col_name = "drnk_presence", 
                                    col_values = c("no", "yes"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_ratecount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    data_start = dates, 
                                    col_name = "recovery", 
                                    col_values = c("no", "yes"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_propcount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    col_name = "recovery", 
                                    col_values = c("no", "yes"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 

  
  feature_row <- feature_row %>% 
    full_join(score_ratecount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    data_start = dates, 
                                    col_name = "supportive1", 
                                    col_values = c("supportive", "unsupportive_mixed", "dont_know"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_propcount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    col_name = "supportive1", 
                                    col_values = c("supportive", "unsupportive_mixed", "dont_know"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label"))   
  

  feature_row <- feature_row %>% 
    full_join(score_ratecount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    data_start = dates, 
                                    col_name = "experience1", 
                                    col_values = c("pleasant", "unpleasant_mixed"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_propcount_value(subid, 
                                    dttm_label,
                                    x_all  = data,
                                    period_durations = period_durations,
                                    lead = lead, 
                                    col_name = "experience1", 
                                    col_values = c("pleasant", "unpleasant_mixed"), 
                                    data_type_col_name = "log_type",
                                    data_type_values = c("sms", "voi")), 
              by = c("subid", "dttm_label"))   
  
  
  
  # call duration by context
  
  feature_row <- feature_row %>% 
    full_join(score_ratesum(subset(labels, label_num == the_label_num)$subid, 
                            subset(labels, label_num == the_label_num)$dttm_label,
                            x_all  = logs_all,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = data_start, 
                            col_name = "duration", 
                            data_type_col_name = "log_type",
                            data_type_values = c("voi"),
                            context_col_name = "contact_type1",
                            context_values = c("counselor_socialworker", "family", "friend")), 
              by = c("subid", "dttm_label")) 
  
  feature_row <- feature_row %>% 
    full_join(score_ratesum(subset(labels, label_num == the_label_num)$subid, 
                            subset(labels, label_num == the_label_num)$dttm_label,
                            x_all  = logs_all,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = data_start, 
                            col_name = "duration", 
                            data_type_col_name = "log_type",
                            data_type_values = c("voi"),
                            context_col_name = "drnk_past",
                            context_values = c("always_almost_always", "never_almost_never", "occasionally")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_ratesum(subset(labels, label_num == the_label_num)$subid, 
                            subset(labels, label_num == the_label_num)$dttm_label,
                            x_all  = logs_all,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = data_start, 
                            col_name = "duration", 
                            data_type_col_name = "log_type",
                            data_type_values = c("voi"),
                            context_col_name = "drinker",
                            context_values = c("yes", "no")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_ratesum(subset(labels, label_num == the_label_num)$subid, 
                            subset(labels, label_num == the_label_num)$dttm_label,
                            x_all  = logs_all,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = data_start, 
                            col_name = "duration", 
                            data_type_col_name = "log_type",
                            data_type_values = c("voi"),
                            context_col_name = "drnk_presence",
                            context_values = c("yes", "no")), 
              by = c("subid", "dttm_label")) 
  
 
  
  feature_row <- feature_row %>% 
    full_join(score_ratesum(subset(labels, label_num == the_label_num)$subid, 
                            subset(labels, label_num == the_label_num)$dttm_label,
                            x_all  = logs_all,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = data_start, 
                            col_name = "duration", 
                            data_type_col_name = "log_type",
                            data_type_values = c("voi"),
                            context_col_name = "recovery",
                            context_values = c("yes", "no")), 
              by = c("subid", "dttm_label")) 
  
  
  feature_row <- feature_row %>% 
    full_join(score_ratesum(subset(labels, label_num == the_label_num)$subid, 
                            subset(labels, label_num == the_label_num)$dttm_label,
                            x_all  = logs_all,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = data_start, 
                            col_name = "duration", 
                            data_type_col_name = "log_type",
                            data_type_values = c("voi"),
                            context_col_name = "supportive1",
                            context_values = c("supportive", "unsupportive_mixed", "dont_know")), 
              by = c("subid", "dttm_label")) 
  
  
  
  feature_row <- feature_row %>% 
    full_join(score_ratesum(subset(labels, label_num == the_label_num)$subid, 
                            subset(labels, label_num == the_label_num)$dttm_label,
                            x_all  = logs_all,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = data_start, 
                            col_name = "duration", 
                            data_type_col_name = "log_type",
                            data_type_values = c("voi"),
                            context_col_name = "experience1",
                            context_values = c("pleasant", "unpleasant_mixed")), 
              by = c("subid", "dttm_label"))
  

  feature_row <- feature_row %>% 
    mutate(label_num = the_label_num)
  
  feature_row
}

# Add outcome label and other info to features
features %>%
  mutate(lapse = labels$lapse) %>% 
  relocate(label_num, subid, dttm_label, lapse) %>% 
  vroom_write(str_c("features_", job_start, "_", job_stop, ".csv"), delim = ",")