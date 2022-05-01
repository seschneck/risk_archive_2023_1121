# setup jobs to make EMA features on CHTC 

# EDIT THESE
window <- "1week"   # remember to edit these in make_features_chtc.R as well
data_type <- "ema"
lead <- 0
version <- "v4"


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
path_jobs <- str_c("P:/studydata/risk/chtc/", data_type)
name_job <- str_c("features_", window, "_", lead, "_", version)

path_processed <- "P:/studydata/risk/data_processed/ema" 
name_ema <- "ema.csv"
name_labels <- str_c("labels_", window, ".csv")
name_study_dates <- "study_dates.csv"

path_shared <- "P:/studydata/risk/data_processed/shared" 
name_lapses <- "lapses.csv"

name_fun <- "fun_features.R"
name_script <- "mak_features_chtc.R"


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
n_jobs <- nrow(vroom(here(path_processed, name_labels), show_col_types = FALSE))
labels_per_job <- 300
job_start <- seq(1, n_jobs, by = labels_per_job) 
job_stop <- c(seq(job_start[2] - 1, n_jobs, by = labels_per_job), n_jobs)
tibble(job_start, job_stop) %>% 
  vroom_write(here(path_jobs, name_job, "input", "jobs.csv"), delim = ",", 
              col_names = FALSE, append = FALSE)

# select and format relevant variables and then copy ema
vroom(here(path_processed, name_ema), show_col_types = FALSE) %>% 
  select(-ema_type, -ema_1_1, -ema_1_2, -ema_1_3, -ema_1_4, -ema_1_5, -ema_1_6) %>% 
  arrange(subid, dttm_obs) %>% 
  vroom_write(here(path_jobs, name_job, "input", "ema.csv"), delim = ",")

# select and format relevant variables and then copy lapses
vroom(here(path_shared, name_lapses), show_col_types = FALSE) %>% 
  filter(!exclude) %>% 
  select(subid, dttm_obs = lapse_start) %>% 
  arrange(subid, dttm_obs) %>% 
  mutate(count = "lapse") %>% 
  vroom_write(here(path_jobs, name_job, "input", "lapses.csv"), delim = ",")

# copy over other data files verbatim
file.copy(from = here(path_processed, name_labels),
          to = here(path_jobs, name_job, "input", "labels.csv")) 
file.copy(from = here(path_processed, name_study_dates),
          to = here(path_jobs, name_job, "input", "study_dates.csv")) 
file.copy(from = here(path_shared, "screen.csv"),
          to = here(path_jobs, name_job, "input", "screen.csv")) 

# copy over function script
file.copy(from = here("shared", name_fun),
          to = here(path_jobs, name_job, "input", name_fun))

# copy over unix files(run script, submit, pre, post files)
file.copy(from = here(data_type, "chtc", "features", "unix", c(list.files(here(data_type, "chtc", "features", "unix")))),
          to = here(path_jobs, name_job, "input"),
          recursive = TRUE)

# copy R script
file.copy(from = here(data_type, "chtc", "features", name_script),
          to = here(path_jobs, name_job, "input", name_script))
