# setup jobs to make features on chtc 
suppressPackageStartupMessages({
  require(dplyr)
  require(readr)
})

# CHANGE ME -------
window <- "1day"   # remember to edit these first 3 in make_features_chtc.R as well
lead <- 0
version <- "v2"
data_type <- "meta"

# CHECK ME ------
name_job <- str_c("features_", window, "_", lead, "_", version)
path_jobs <- str_c("P:/studydata/risk/chtc/", data_type)
path_data <- str_c("P:/studydata/risk/data_processed/", data_type) 
labels_file_name <- str_c("labels_", window, ".csv")
raw_data_file_name <- "meta_logs.rds"
start_dates_file_name <- "study_dates.rds"
static_features_file_name <- "static_features.rds"
fun_name <- "fun_features.R" # this will be name that function appears as on CHTC
labels_per_job <- 100

path_templates <- "./meta/chtc/features/unix"
path_mak_script <- "./meta/chtc/features"
path_fun <- "./shared/fun_features.R"


# DON'T CHANGE --------
# create new job directory (if it does not already exist) 
if (!dir.exists(file.path(path_jobs, name_job))) {
  dir.create(file.path(path_jobs, name_job))
  dir.create(file.path(path_jobs, name_job, "input"))
  dir.create(file.path(path_jobs, name_job, "output"))
} else {
  stop("Job folder already exists. No new folders created.")
}

# create jobs file
n_jobs <- nrow(read_rds(file.path(path_data, labels_file_name)))
job_start <- seq(1, n_jobs, by = labels_per_job) 
job_stop <- c(seq(job_start[2]-1, n_jobs, by = labels_per_job), n_jobs)
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

# copy over function script
file.copy(from = path_fun,
          to = file.path(path_jobs, name_job, "input", fun_name))

# copy over study mak_features script
file.copy(from = file.path(path_mak_script, "mak_features_chtc.R"),
          to = file.path(path_jobs, name_job, "input"))

# copy over input templates (submit, pre, post files)
file.copy(from = file.path(path_templates, c(list.files(path_templates))),
          to = file.path(path_jobs, name_job, "input"),
          recursive = TRUE) 


