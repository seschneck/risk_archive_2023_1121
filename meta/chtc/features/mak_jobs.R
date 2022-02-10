# setup jobs to make features on chtc 
suppressPackageStartupMessages({
  require(dplyr)
  require(readr)
})

# CHANGE ME
name_job <- "features_1_hour"
path_jobs <- "P:/studydata/risk/chtc/meta/jobs/features/"
path_templates <- "./meta/chtc/features/templates"
path_data <- "P:/studydata/risk/data_processed/meta" 
path_fun <- "./shared/fun_chtc_features.R"
fun_name <- "fun_chtc_features.R" # this will be name that function appears as on CHTC
labels_file_name <- "labels_1_hour.rds"
raw_data_file_name <- "meta_logs.rds"
start_dates_file_name <- "study_dates.rds"
static_features_file_name <- "static_features.rds"
n_jobs <- nrow(read_rds(file.path(path_data, labels_file_name)))
jobs <- seq(1:n_jobs) # this is equivalent to row numbers of labels that will be used 


# DON'T CHANGE
# create new job directory (if it does not already exist) 
if (!dir.exists(file.path(path_jobs, name_job))) {
  dir.create(file.path(path_jobs, name_job))
  dir.create(file.path(path_jobs, name_job, "input"))
  dir.create(file.path(path_jobs, name_job, "output"))
} else {
  stop("Job folder already exists. No new folders created.")
}

# save out jobs txt file for queue
readr::write_lines(jobs, file.path(path_jobs, name_job, "input/jobs.csv"))

# copy over data files
file.copy(from = file.path(path_data, raw_data_file_name),
          to = file.path(path_jobs, name_job, "input/data.rds")) 
file.copy(from = file.path(path_data, labels_file_name),
          to = file.path(path_jobs, name_job, "input/labels.rds"))
file.copy(from = file.path(path_data, start_dates_file_name),
          to = file.path(path_jobs, name_job, "input/study_dates.rds"))
file.copy(from = file.path(path_data, static_features_file_name),
          to = file.path(path_jobs, name_job, "input/static_features.rds")) 

# copy over function script
file.copy(from = path_fun,
          to = file.path(path_jobs, name_job, "input", fun_name))

# copy over input templates (run script, submit, pre, post files)
file.copy(from = file.path(path_templates, "input", c(list.files(file.path(path_templates, "input")))),
          to = file.path(path_jobs, name_job, "input"),
          recursive = TRUE) 

# copy over output template (aggregate rows)
file.copy(from = file.path(path_templates, "output", "post_chtc_processing.rmd"),
          to = file.path(path_jobs, name_job, "output", "post_chtc_processing.rmd")) 
