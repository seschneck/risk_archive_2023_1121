# setup chtc jobs & associated files/folders

# CHANGE ME -------------------
data_trn <- "period_720_lead_0.csv"
name_job <- "test_glmnet" # the name of the job to set folder names
feature_set <- "all_features" # 1 data stream to use (all_features or passive_only)
algorithm <- c("glmnet") # 1+ statistical algorithms
hp1 <- seq(0, 1, length.out = 11) # RF: mtry; KNN: neighbors; glmnet: alpha (mixture)
hp2 <- NULL # RF: min_n; 
hp3 <- NULL # RF: trees (10 x's number of predictors)
n_folds <- 10 # number of folds
n_repeats <- 1 # number of repeats
resample <- c("none", "up") # 1+ upsampling methods (up, down, smote, or none)

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
jobs <- expand_grid(n_fold = 1:n_folds,
                    n_repeat = 1:n_repeats,
                    algorithm,
                    feature_set,
                    hp1,
                    hp2,
                    hp3,
                    resample)

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
