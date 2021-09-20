# setup chtc jobs & associated files/folders
# the first two sections should be changed for each job

# CHANGE GLOBAL JOB PARAMETERS -------------------
data_trn <- "period_720_lead_0.csv"
name_job <- "test_algorithms" # the name of the job to set folder names
feature_set <- c("all_features") # 1+ data stream to use (all_features or passive_only)
algorithm <- c("glmnet", "random_forest") # 1+ algorithm (glmnet, random_forest) 
resample <- c("none", "up", "down", "smote") # 1+ upsampling methods (up, down, smote, or none)
cv_type <- "1_x_10" # format should be n_repeats_x_n_folds (e.g., 1_x_10, 50_x_10)

# CHANGE ALGORITHM-SPECIFIC HYPERPARAMETERS -------------------
hp1_glmnet <- seq(0, 1, length.out = 11) # alpha (mixture) 
hp1_rf <- c(5, 10, 20, 50) # mtry (p/3 for reg or square root of p for class)
hp2_rf <- c(2, 10, 20) # min_n
hp3_rf <- 2000 # trees (10 x's number of predictors)

# set paths -------------------- 
path_jobs <- "P:/studydata/risk/chtc/meta/jobs" 
path_templates <- "templates"
path_data <- "P:/studydata/risk/data_processed/meta/features"

# load libraries & source files ------------------
library(tidyverse)

# create jobs tibble ---------------
for (i in algorithm) {
  if (i == "glmnet") { 
    jobs_tmp <- expand_grid(n_repeat = NA_integer_,
                      n_fold = NA_integer_,
                      algorithm = "glmnet",
                      feature_set,
                      hp1 = hp1_glmnet,
                      hp2 = NA_integer_,
                      hp3 = NA_integer_,
                      resample,
                      cv_type) 
    } else if (i == "random_forest") {
    jobs_tmp <- expand_grid(n_repeat = 1:as.numeric(str_split(cv_type, "_x_")[[1]][1]),
                        n_fold = 1:as.numeric(str_split(cv_type, "_x_")[[1]][2]),
                        algorithm = "random_forest",
                        feature_set,
                        hp1 = hp1_rf,
                        hp2 = hp2_rf,
                        hp3 = hp3_rf,
                        resample,
                        cv_type)
    }
  # bind jobs files
  jobs <- if (i == algorithm[1])
    jobs_tmp
  else
    rbind(jobs, jobs_tmp)
}

# add job num to file --------------- 
jobs <- jobs %>% 
  rownames_to_column("job_num") 

# create new job directory (if it does not already exist) -------------------
if (!dir.exists(file.path(path_jobs, name_job))) {
  dir.create(file.path(path_jobs, name_job))
  dir.create(file.path(path_jobs, name_job, "input"))
  dir.create(file.path(path_jobs, name_job, "output"))
} else {
  stop("Job folder already exists. No new folders created.")
}

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
