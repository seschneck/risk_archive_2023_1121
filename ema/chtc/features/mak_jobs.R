# setup jobs to make EMA features on CHTC 

# EDIT THESE
window <- "1week"   # remember to edit these in make_features_chtc.R as well
lead <- 0
version <- "v5"

# load packages
library(tidyverse)
library(lubridate)

# Paths and filenames
path_jobs <- str_c("~/mnt/studydata/risk/chtc/ema")
name_job <- str_c("features_", window, "_", lead, "_", version)

path_processed <- "~/mnt/private/studydata/risk/data_processed/ema" 
name_ema <- "ema.csv"
name_labels <- str_c("labels_", window, ".csv")
name_study_dates <- "study_dates.csv"

path_shared <- "~/mnt/private/studydata/risk/data_processed/shared" 
name_lapses <- "lapses.csv"

name_fun <- "fun_features.R"
name_script <- "mak_features_chtc.R"


# create new job directory (if it does not already exist) 
if (!dir.exists(file.path(path_jobs, name_job))) {
  dir.create(file.path(path_jobs, name_job))
  dir.create(file.path(path_jobs, name_job, "input"))
  dir.create(file.path(path_jobs, name_job, "output"))
} else {
  stop("Job folder already exists. No new folders created.")
}

# save out jobs csv file for queue
n_jobs <- nrow(read_csv(file.path(path_processed, name_labels), show_col_types = FALSE))
labels_per_job <- 300
job_start <- seq(1, n_jobs, by = labels_per_job) 
job_stop <- c(seq(job_start[2] - 1, n_jobs, by = labels_per_job), n_jobs)
tibble(job_start, job_stop) %>% 
  write_csv(file.path(path_jobs, name_job, "input", "jobs.csv"), 
              col_names = FALSE, append = FALSE)

# select and format relevant variables and then copy ema
read_csv(file.path(path_processed, name_ema), show_col_types = FALSE) %>% 
  select(-ema_type, -ema_1_1, -ema_1_2, -ema_1_3, -ema_1_4, -ema_1_5, -ema_1_6) %>% 
  arrange(subid, dttm_obs) %>% 
  write_csv(file.path(path_jobs, name_job, "input", "ema.csv"))

# select and format relevant variables and then copy lapses
read_csv(file.path(path_shared, name_lapses), show_col_types = FALSE) %>% 
  filter(!exclude) %>% 
  select(subid, dttm_obs = lapse_start) %>% 
  arrange(subid, dttm_obs) %>% 
  mutate(count = "lapse") %>% 
  write_csv(file.path(path_jobs, name_job, "input", "lapses.csv"))

# copy over other data files verbatim
file.copy(from = file.path(path_processed, name_labels),
          to = file.path(path_jobs, name_job, "input", "labels.csv")) 
file.copy(from = file.path(path_processed, name_study_dates),
          to = file.path(path_jobs, name_job, "input", "study_dates.csv")) 
file.copy(from = file.path(path_shared, "screen.csv"),
          to = file.path(path_jobs, name_job, "input", "screen.csv")) 

# copy over function script
file.copy(from = file.path("shared", name_fun),
          to = file.path(path_jobs, name_job, "input", name_fun))

# copy over unix files(run script, submit)
file.copy(from = file.path("ema", "chtc", "features", "unix", c(list.files(file.path("ema", "chtc", "features", "unix")))),
          to = file.path(path_jobs, name_job, "input"),
          recursive = TRUE)

# copy R script
file.copy(from = file.path("ema", "chtc", "features", name_script),
          to = file.path(path_jobs, name_job, "input", name_script))
