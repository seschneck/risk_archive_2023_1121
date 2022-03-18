# setup jobs to make EMA features on chtc 

# load packages
library(here)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("filter", "dplyr")

library(tidyverse)
library(lubridate)
library(vroom)

# Paths and filenames
name_job <- "features_1day"
path_jobs <- "P:/studydata/risk/chtc/ema"
path_ema <- "P:/studydata/risk/data_processed/ema" 
name_ema <- "ema.csv"
path_lapses <- "P:/studydata/risk/data_processed/shared" 
name_lapses <- "lapses.csv"
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
n_jobs <- nrow(vroom(here(path_ema, name_labels), show_col_types = FALSE))
labels_per_job <- 300
job_start <- seq(1, n_jobs, by = labels_per_job) 
job_stop <- c(seq(job_start[2] - 1, n_jobs, by = labels_per_job), n_jobs)
tibble(job_start, job_stop) %>% 
  vroom_write(here(path_jobs, name_job, "input", "jobs.csv"), delim = ",", 
              col_names = FALSE, append = FALSE)

# select and format relevant variables and then copy ema
vroom(here(path_ema, name_ema), show_col_types = FALSE) %>% 
  select(-ema_type, -ema_1_1, -ema_1_2, -ema_1_3, -ema_1_4, -ema_1_5, -ema_1_6) %>% 
  arrange(subid, dttm_obs) %>% 
  vroom_write(here(path_jobs, name_job, "input", "ema.csv"), delim = ",")

# select and format relevant variables and then copy lapses
vroom(here(path_lapses, name_lapses), show_col_types = FALSE) %>% 
  filter(!exclude) %>% 
  select(subid, dttm_obs = lapse_start) %>% 
  arrange(subid, dttm_obs) %>% 
  mutate(count = "lapse") %>% 
  vroom_write(here(path_jobs, name_job, "input", "lapses.csv"), delim = ",")

# copy over other data files verbatim
file.copy(from = here(path_ema, name_labels),
          to = here(path_jobs, name_job, "input", "labels.csv")) 
file.copy(from = here(path_ema, name_study_dates),
          to = here(path_jobs, name_job, "input", "study_dates.csv")) 

# copy over function script
file.copy(from = here("shared", name_fun),
          to = here(path_jobs, name_job, "input", name_fun))

# copy over unix files(run script, submit, pre, post files)
file.copy(from = here("ema", "chtc", "features", "1day", "unix", c(list.files(here("ema", "chtc", "features", "1day", "unix")))),
          to = here(path_jobs, name_job, "input"),
          recursive = TRUE)

# copy R script
file.copy(from = here("gps", "chtc", "features", "1day", name_script),
          to = here(path_jobs, name_job, "input", name_script))
