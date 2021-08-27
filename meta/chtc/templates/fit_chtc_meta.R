# fit model at chtc with bootstrapping 

# libraries & source functions file ----------------
library(dplyr)
library(readr)
library(tidyr)
source("fun_chtc_meta.R")

# Parallel processing for running local test job ----------------
# library(doParallel)
# n_core <- detectCores(logical = FALSE)
# cl <- makePSOCKcluster(n_core - 1)
# registerDoParallel(cl)

# set up job_num ---------------
args <- commandArgs(trailingOnly = TRUE) 
# job_num <- 1
job_num <- as.numeric(args[1]) + 1 # process/job arg starts at 0

# read in jobs.csv file ------------------
# jobs <- read_csv(file.choose(), col_types = cols())
jobs <- read_csv("jobs.csv", col_types = cols()) 

# get total splits and repeats before slicing job ------------------
n_splits <- max(jobs$n_split)
n_repeats <- max(jobs$n_repeat)

# pull out job ------------------
job <- slice(jobs, job_num)

# read in data train --------------
# d <- read_csv("/Volumes/private/studydata/risk/data_processed/meta/features/period_720_lead_0.csv", col_types = cols())
d <- read_csv("data_trn.csv", col_types = cols())

# build recipe ----------------
rec <- build_recipe(d = d, job = job)

# create cv splits -------------------
set.seed(102030)
splits <- split_data(d = d, job = job, n_splits = n_splits, n_repeats = n_repeats)

# build feature matrices ---------------
matrices <- make_feature_matrices(job = job, splits = splits, rec = rec)
feat_in <- matrices[[1]]
feat_out <- matrices[[2]]

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