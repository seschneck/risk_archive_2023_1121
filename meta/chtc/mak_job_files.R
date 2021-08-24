# setup chtc jobs & associated files/folders

# CHANGE ME -------------------
name_job <- "fit_test" # the name of the job to set folder names

period_duration_hrs <- c(720) # 1+ feature period duration
lead_hours <- c(0) # 1+ lead hours for prediction
feature_set <- c("all_context") # 1 data stream to use
# options = voice, sms, all (voice and sms), add _context to include context features

algorithm <- c("random_forest") # 1+ statistical algorithms
hp1 <- c(5, 10, 20, 100, 200) # RF: mtry
hp2 <- c(2, 15, 25, 40) # RF: min_n
hp3 <- 3500 # RF: trees
n_splits <- 100 # number of bootstraps or folds


# load libraries & source files ------------------
library(tidyverse)

# set paths -------------------- 
path_jobs <- "P:/studydata/risk/chtc/meta/jobs"  

# create new job directory (if it does not already exist) -------------------
if (!dir.exists(file.path(path_jobs, name_job))) {
  dir.create(file.path(path_jobs, name_job))
  dir.create(file.path(path_jobs, name_job, "input"))
  dir.create(file.path(path_jobs, name_job, "output"))
  dir.create(file.path(path_jobs, name_job, "output", "results"))
  dir.create(file.path(path_jobs, name_job, "output", "err"))
  dir.create(file.path(path_jobs, name_job, "output", "out"))
} else {
  stop("Job folder already exists. No new folders created.")
  }

# create jobs tibble ---------------
jobs <- expand_grid(n_split = 1:n_splits,
                    algorithm,
                    feature_set,
                    period_duration_hrs,
                    lead_hours,
                    hp1,
                    hp2,
                    hp3)

print(str_c(nrow(jobs), " jobs created"))


# write jobs file to input folder ---------------
jobs %>% 
  write_csv(file.path(path_jobs, name_job, "input", "jobs.csv"))


# FIX: Need a copy of template R files - not sure what this is   
# probably is the mak, fit, fun chtc files but I will keep these in the repo so 
# don't need to move over to input folder?

# copy template R files to input folder -----------------
# path_templates <- "scripts/fit_chtc/templates"
# file.copy(from = file.path(path_templates, dir(path_templates)),
#           to = file.path(path_jobs, name_job, "input"),
#           recursive = TRUE)
