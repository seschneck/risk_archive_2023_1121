# setup jobs to make GPS features on chtc 

# load packages
library(tidyverse)
library(conflicted)
library(here)
library(lubridate)
library(vroom)

# Paths and filenames
name_job <- "features_1day"
path_jobs <- "P:/studydata/risk/chtc/gps"
path_gps <- "P:/studydata/risk/data_processed/gps" 
name_gps <- "gps_enriched.csv.xz"
name_labels <- "labels_1day.csv"
name_study_dates <- "study_dates.csv"
name_fun <- "fun_features.R"
name_script <- "mak_features_chtc.R"

# create new job directory (if it does not already exist) 
if (!dir.exists(here(path_jobs, name_job))) {
  dir.create(here(path_jobs, name_job))
  dir.create(here(path_jobs, name_job, "input"))
  dir.create(here(path_jobs, name_job, "output"))
  dir.create(here(path_jobs, name_job, "output", "features"))
  dir.create(here(path_jobs, name_job, "output", "error"))
  dir.create(here(path_jobs, name_job, "output", "out"))
  
} else {
  stop("Job folder already exists. No new folders created.")
}

# save out jobs csv file for queue
n_jobs <- nrow(vroom(here(path_gps, name_labels)))
labels_per_job <- 200
job_start <- seq(1, n_jobs, by = labels_per_job) 
job_stop <- c(seq(job_start[2] - 1, n_jobs, by = labels_per_job), n_jobs)
tibble(job_start, job_stop) %>% 
  vroom_write(here(path_jobs, name_job, "input", "jobs.csv"), delim = ",", 
              col_names = FALSE, append = FALSE)

# select and format relevant variables and then copy enriched gps
vroom(here(path_gps, name_gps), show_col_types = FALSE) %>% 
  select(subid, time, duration, dist_context, type, drank, alcohol, emotion, risk, avoid) %>% 
  arrange(subid, time) %>% 
  vroom_write(here(path_jobs, name_job, "input", "data.csv.xz"), delim = ",")

# copy over other data files verbatim
file.copy(from = here(path_gps, name_labels),
          to = here(path_jobs, name_job, "input", "labels.csv")) 
file.copy(from = here(path_gps, name_study_dates),
          to = here(path_jobs, name_job, "input", "study_dates.csv")) 

# copy over function script
file.copy(from = here("shared", name_fun),
          to = here(path_jobs, name_job, "input", name_fun))

# copy over unix files(run script, submit, pre, post files)
file.copy(from = here("gps", "chtc", "features", "unix", c(list.files(here("gps", "chtc", "features", "unix")))),
          to = here(path_jobs, name_job, "input"),
          recursive = TRUE)

# copy R script
file.copy(from = here("gps", "chtc", "features", name_script),
          to = here(path_jobs, name_job, "input", name_script))
