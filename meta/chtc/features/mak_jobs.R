# setup jobs to make features on chtc 
suppressPackageStartupMessages({
  require(dplyr)
  require(readr)
})

# CHANGE ME
name_job <- "features_1_day"
path_jobs <- "P:/studydata/risk/chtc/meta/jobs/features/"
path_templates <- "./meta/chtc/features/unix"
path_mak_script <- "./meta/chtc/features/1day_full"
path_data <- "P:/studydata/risk/data_processed/meta" 
path_fun <- "./shared/fun_features.R"
fun_name <- "fun_features.R" # this will be name that function appears as on CHTC
labels_file_name <- "labels_1_day.rds"
raw_data_file_name <- "meta_logs.rds"
start_dates_file_name <- "study_dates.rds"
static_features_file_name <- "static_features.rds"
n_jobs <- nrow(read_rds(file.path(path_data, labels_file_name)))
# create start and end job number for slicing labels
labels_per_job <- 100
job_start <- seq(1, n_jobs, by = labels_per_job) 
job_stop <- c(seq(job_start[2]-1, n_jobs, by = labels_per_job), n_jobs)


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
jobs <- tibble(job_start, job_stop)
readr::write_csv(jobs, file.path(path_jobs, name_job, "input/jobs.csv"), col_names = FALSE)

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

# copy over mak_features script
file.copy(from = path_fun,
          to = file.path(path_jobs, name_job, "input", fun_name))

# copy over mak_features script
file.copy(from = file.path(path_mak_script, "mak_features_chtc.R"),
          to = file.path(path_jobs, name_job, "input"))

# copy over input templates (submit, pre, post files)
file.copy(from = file.path(path_templates, c(list.files(path_templates))),
          to = file.path(path_jobs, name_job, "input"),
          recursive = TRUE) 


