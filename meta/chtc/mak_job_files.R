# setup chtc jobs & associated files/folders

# CHANGE ME -------------------
name_job <- "fit_test" # the name of the job to set folder names
feature_set <- c("items", "scales") # 1+ specific feature set characteristics
algorithm <- c("random_forest") # 1+ statistical algorithms
hp1 <- c(5, 10, 20, 100, 200) # RF: mtry
hp2 <- c(2, 15, 25, 40) # RF: min_n
hp3 <- 3500 # RF: trees
n_splits <- 100 # number of bootstraps or folds
n_repeats <- 0 # number of repeats if cv_type is "repeated"
cv_type <- "boot" # cv type; can be "boot", "kfold", or "repeated"
preprocess <- c("ppwk26_drop_25") # unique text string that sets outcome & tolerable missingness pct

# load libraries & source files ------------------
library(tidyverse)

# set paths -------------------- 
path_jobs <- "P:/studydata/match/jobs" 
path_templates <- "scripts/fit_chtc/templates"

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
if (n_repeats > 0) {
  jobs <- expand_grid(n_split = 1:n_splits,
                      n_repeat = 1:n_repeats,
                      algorithm,
                      feature_set,
                      hp1,
                      hp2,
                      hp3,
                      cv_type,
                      preprocess)
} else {
  jobs <- expand_grid(n_split = 1:n_splits,
                      algorithm,
                      feature_set,
                      hp1,
                      hp2,
                      hp3,
                      cv_type,
                      preprocess)
}

nrow(jobs)

# write jobs file to input folder ---------------
jobs %>% 
  write_csv(file.path(path_jobs, name_job, "input", "jobs.csv"))

# copy template R files to input folder -----------------
file.copy(from = file.path(path_templates, dir(path_templates)),
          to = file.path(path_jobs, name_job, "input"),
          recursive = TRUE)
