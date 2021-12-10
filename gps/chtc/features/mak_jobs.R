# setup jobs to make features on chtc 

# load packages
library(tidyverse)
library(conflicted)
library(here)
library(lubridate)
library(vroom)

# Paths and filenames
name_job <- "features_all"
path_jobs <- "P:/studydata/risk/chtc/gps"
path_templates <- "gps/chtc/features/templates"
path_gps <- "P:/studydata/risk/data_processed/gps" 
path_fun <- "shared/fun_risk.R"
name_fun <- "fun_risk.R"
name_labels <- "labels_05.csv"
name_gps <- "gps_enriched.csv.xz"
name_study_dates <- "study_dates.csv"

n_jobs <- nrow(read_csv(here(path_gps, name_labels)))
jobs <- seq(1:n_jobs) # this is equivalent to row numbers of labels that will be used 

# create new job directory (if it does not already exist) 
if (!dir.exists(here(path_jobs, name_job))) {
  dir.create(here(path_jobs, name_job))
  dir.create(here(path_jobs, name_job, "input"))
  dir.create(here(path_jobs, name_job, "output"))
} else {
  stop("Job folder already exists. No new folders created.")
}

# save out jobs txt file for queue
write_lines(jobs, here(path_jobs, name_job, "input/jobs.csv"))

# select and format relevant variables and then copy enriched gps
vroom(here(path_gps, name_gps)) %>% 
  select(subid, time, dist_context, type, drank, alcohol, emotion, risk, avoid) %>% 
  arrange(subid, time) %>% 
  vroom_write(here(path_jobs, name_job, "input", "data.csv.xz"), delim = ",")

# copy over other data files verbatim
file.copy(from = here(path_gps, name_labels),
          to = here(path_jobs, name_job, "input", "labels.csv")) 
file.copy(from = here(path_gps, name_study_dates),
          to = here(path_jobs, name_job, "input", "study_dates.csv")) 

# copy over function script
file.copy(from = path_fun,
          to = here(path_jobs, name_job, "input", name_fun))

# copy over input templates (run script, submit, pre, post files)
file.copy(from = here(path_templates, "input", c(list.files(here(path_templates, "input")))),
          to = here(path_jobs, name_job, "input"),
          recursive = TRUE)

# copy over output template (aggregate rows)
file.copy(from = here(path_templates, "output", "post_chtc_processing.Rmd"),
          to = here(path_jobs, name_job, "output", "post_chtc_processing.Rmd"))