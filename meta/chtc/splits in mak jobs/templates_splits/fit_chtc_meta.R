# fit model at chtc with bootstrapping 

# libraries & source functions file ----------------
library(dplyr)
library(readr)
library(tidyr)
source("fun_chtc_meta.R")

# set up job_num ---------------
args <- commandArgs(trailingOnly = TRUE) 
# job_num <- 1
job_num <- as.numeric(args[1]) + 1 # process/job arg starts at 0

# read in jobs.csv file ------------------
jobs <- read_csv("jobs.csv", col_types = cols()) 

# get total splits and repeats before slicing job ------------------
n_splits <- max(jobs$n_split)
n_repeats <- max(jobs$n_repeat)

# pull out job ------------------
job <- slice(jobs, job_num)

# read in split object --------------


# build recipe ----------------
# pass in split
rec <- build_recipe(d = d, job = job)



# build feature matrices ---------------
features <- make_features(job = job, n_repeats, splits = splits, rec = rec)
feat_in <- features$feat_in
feat_out <- features$feat_out

# fit model ----------------
model <- fit_model(feat_in = feat_in, job = job)

# get predictions & metrics -------------
results <- get_metrics(model = model, feat_out = feat_out)

# write out results tibble ------------
file_name <- paste("results_", job_num, ".csv", sep = "")
results %>% 
  pivot_wider(., names_from = "metric",
              values_from = "estimate") %>% 
  bind_cols(job, .) %>% 
  write_csv(., file_name)
