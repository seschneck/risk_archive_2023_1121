# fit model at chtc with grouped k-fold cv 

# libraries & source functions file ----------------
suppressPackageStartupMessages({
  require(dplyr)
  require(readr)
  require(tidyr)
})
source("fun_chtc_meta.R")

# set up job_num ---------------
# process_num <- 1
args <- commandArgs(trailingOnly = TRUE) 
process_num <- as.numeric(args[1]) + 1 # process/job arg starts at 0

# read in jobs.csv file ------------------
jobs <- read_csv("jobs.csv", col_types = cols()) 

# get total splits and repeats before slicing job ------------------
n_folds <- max(jobs$n_fold)
n_repeats <- max(jobs$n_repeat)

# pull out job ------------------
job <- slice(jobs, process_num)

# read in data train --------------- 
d <- read_csv("data_trn.csv", col_types = cols())

# create splits object ---------------
set.seed(102030)
folds <- make_folds(d = d, n_folds = n_folds, n_repeats = n_repeats)

# build recipe ----------------
rec <- build_recipe(d = d, job = job)

# fit model and get predictions and metrics ----------------
results <- tune_model(job = job, rec = rec, folds = folds, n_repeats = n_repeats)

# write out results tibble ------------
file_name <- paste0("results_", process_num, ".csv")
results %>% 
  write_csv(., file_name)
