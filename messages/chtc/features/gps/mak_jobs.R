# setup jobs to make GPS features on chtc for messages

# EDIT THESE
study <- "messages"
window <- "1day"   # remember to edit these in make_features_chtc.R as well
data_type <- "gps"
lead <- 0
version <- "v1"

# load packages
library(here)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("filter", "dplyr")

library(tidyverse)
library(lubridate)
library(vroom)
library(stringr)


# Paths and filenames
path_jobs <- str_c("P:/studydata/risk/chtc/", study)
name_job <- str_c("features_", data_type, "_", window, "_", lead, "_", version)

path_processed <- str_c("P:/studydata/risk/data_processed/", study) 
name_gps <- "gps_enriched.csv.xz"
name_labels <- str_c("labels_", window, ".csv")
name_study_dates <- "study_dates_all.csv"

name_fun <- "fun_features.R"
name_script <- "mak_gps_features_chtc.R"

# create new job directory (if it does not already exist) 
if (!dir.exists(here(path_jobs, name_job))) {
  dir.create(here(path_jobs, name_job))
  dir.create(here(path_jobs, name_job, "input"))
  dir.create(here(path_jobs, name_job, "output"))
  dir.create(here(path_jobs, name_job, "output", "features"))
  dir.create(here(path_jobs, name_job, "output", "error"))
  
} else {
  stop("Job folder already exists. No new folders created.")
}

# save out jobs csv file for queue
n_jobs <- nrow(vroom(here(path_processed, name_labels)))
labels_per_job <- 200
job_start <- seq(1, n_jobs, by = labels_per_job) 
job_stop <- c(seq(job_start[2] - 1, n_jobs, by = labels_per_job), n_jobs)
tibble(job_start, job_stop) %>% 
  vroom_write(here(path_jobs, name_job, "input", "jobs.csv"), delim = ",", 
              col_names = FALSE, append = FALSE)

# select and format relevant variables and then copy enriched gps
vroom(here(path_processed, name_gps), show_col_types = FALSE) %>% 
  select(subid, time, duration, dist_context, type, drank, alcohol, emotion, risk, avoid) %>% 
  arrange(subid, time) %>% 
  vroom_write(here(path_jobs, name_job, "input", "data.csv.xz"), delim = ",")

# copy over other data files verbatim
file.copy(from = here(path_processed, name_labels),
          to = here(path_jobs, name_job, "input", "labels.csv")) 
file.copy(from = here(path_processed, name_study_dates),
          to = here(path_jobs, name_job, "input", "study_dates.csv")) 

# copy over function script
file.copy(from = here("shared", name_fun),
          to = here(path_jobs, name_job, "input", name_fun))


# copy over unix files(run script, submit, pre, post files)
file.copy(from = here(study, "chtc", "features", data_type, "unix", c(list.files(here("messages", "chtc", "features", data_type, "unix")))),
          to = here(path_jobs, name_job, "input"),
          recursive = TRUE)

# copy R script
file.copy(from = here(study, "chtc", "features", data_type, name_script),
          to = here(path_jobs, name_job, "input", name_script))

