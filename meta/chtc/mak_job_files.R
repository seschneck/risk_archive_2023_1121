# setup chtc jobs & associated files/folders

# CHANGE ME -------------------
data_trn <- "period_720_lead_0.csv"
name_job <- "random_forest" # the name of the job to set folder names
feature_set <- "all_features" # 1 data stream to use (all_features or passive_only)
algorithm <- c("random_forest") # 1+ statistical algorithms
hp1 <- c(5, 10, 20, 100, 200) # RF: mtry; KNN: neighbors; glmnet: alpha (mixture)
hp2 <- c(2, 15, 25, 40) # RF: min_n; glmnet: lambda (penalty) - This gets set on CHTC 
hp3 <- 2000 # RF: trees (10 x's number of predictors), set to NA_integer_ if not using
n_folds <- 10 # number of folds
n_repeats <- 1 # number of repeats
resample <- c("none", "up", "down", "smote") # 1+ upsampling methods (up, down, smote, or none)

# set paths -------------------- 
path_jobs <- "P:/studydata/risk/chtc/meta/jobs" 
path_templates <- "templates"
path_data <- "P:/studydata/risk/data_processed/meta/features"

# load libraries & source files ------------------
library(tidyverse)

# create new job directory (if it does not already exist) -------------------
if (!dir.exists(file.path(path_jobs, name_job))) {
  dir.create(file.path(path_jobs, name_job))
  dir.create(file.path(path_jobs, name_job, "input"))
  dir.create(file.path(path_jobs, name_job, "output"))
} else {
  stop("Job folder already exists. No new folders created.")
}

# create jobs tibble ---------------

if (algorithm == "glmnet") {
  # note for glmnet we only put total number of folds and repeats for creating splits
  # in tune_grid. Jobs not broken down by fold.
  jobs <- expand_grid(n_fold = n_folds,
                      n_repeat = n_repeats,
                      algorithm,
                      feature_set,
                      hp1,
                      hp2,
                      hp3,
                      resample)
  
} else { 
  jobs <- expand_grid(n_fold = 1:n_folds,
                      n_repeat = 1:n_repeats,
                      algorithm,
                      feature_set,
                      hp1,
                      hp2,
                      hp3,
                      resample)
}


# add job num to file --------------- 
jobs <- jobs %>% 
  rownames_to_column("job_num") 

# write jobs file to input folder ---------------
jobs %>% 
  write_csv(file.path(path_jobs, name_job, "input", "jobs.csv"))

# copy data to input folder as data_trn -----------------
file.copy(from = file.path(path_data, data_trn),
          to = file.path(path_jobs, name_job, "input/data_trn.csv")) %>% 
  invisible()

# copy template R files to input folder -----------------
file.copy(from = file.path(path_templates, "input", c(list.files(file.path(path_templates, "input")))),
          to = file.path(path_jobs, name_job, "input"),
          recursive = TRUE) %>% 
  invisible()

# copy template aggregate script to output folder ---------------
file.copy(from = file.path(path_templates, "post_chtc_processing.Rmd"),
          to = file.path(path_jobs, name_job, "output/post_chtc_processing.Rmd")) %>% 
  invisible()

# update queue on submit file -----------------
queue <- str_c("queue ", nrow(jobs))
write(queue, file.path(path_jobs, name_job, "input/sub_meta.sub"), append = TRUE)
