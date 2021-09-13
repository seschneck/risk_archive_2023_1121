# fit model at chtc with bootstrapping 

# libraries & source functions file ----------------
suppressPackageStartupMessages({
  require(dplyr)
  require(readr)
  require(tidyr)
})
source("fun_chtc_meta.R")

# set up job_num ---------------
# job_num <- 1
args <- commandArgs(trailingOnly = TRUE) 
job_num <- as.numeric(args[1]) + 1 # process/job arg starts at 0

# read in jobs.csv file ------------------
jobs <- read_csv("jobs.csv", col_types = cols()) 

# get total splits and repeats before slicing job ------------------
n_folds <- max(jobs$n_fold)
n_repeats <- max(jobs$n_repeat)

# pull out job ------------------
job <- slice(jobs, job_num)

# read in data train --------------- 
d <- read_csv("data_trn.csv", col_types = cols())

# create splits object ---------------
set.seed(102030)
folds <- make_folds(d = d, n_folds = n_folds, n_repeats = n_repeats)

# build recipe ----------------
rec <- build_recipe(d = d, job = job)

# build feature matrices ---------------
features <- make_features(job = job, n_repeats = n_repeats, folds = folds, rec = rec)
feat_in <- features$feat_in
feat_out <- features$feat_out

# retain held-in split raw data for glmnet hyperparameter tuning ---------------
d_in <- features$d_in

# fit model ----------------
model <- fit_model(feat_in = feat_in, d_in = d_in, rec = rec, job = job)

# get predictions & metrics -------------
results <- get_metrics(model = model, feat_out = feat_out)

# write out results tibble ------------
file_name <- paste0("results_", job_num, ".csv")
results %>% 
  pivot_wider(., names_from = "metric",
              values_from = "estimate") %>% 
  bind_cols(job, .) %>% 
  write_csv(., file_name)
