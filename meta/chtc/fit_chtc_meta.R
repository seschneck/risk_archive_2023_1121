# fit model at chtc with bootstrapping for cv

# libraries & source functions file ----------------
library(readr)
library(dplyr)
library(tidyr)
source("fun_chtc_meta.R")

# set up job_num ---------------
args <- commandArgs(trailingOnly = TRUE) 
# job_num <- 1
job_num <- as.numeric(args[1]) + 1 # process/job arg starts at 0

# read in jobs.csv file ------------------
# jobs <- read_csv(file.choose())
jobs <- read_csv("jobs.csv",
                 col_types = cols()) 

n_splits <- max(jobs$n_split)

job <- slice(jobs, job_num)

# read in match_trn_cln.csv file & prep data --------------
d <- read_csv("data_trn_meta.csv",
              col_types = cols())

# create bootstrapping for cv splits -------------------
set.seed(11151994)
splits <- split_data(d = d, job = job, n_splits = n_splits)

# build recipe ----------------
rec <- build_recipe(d = d, job = job)

# fit model ----------------
model <- fit_model(rec = rec, splits = splits, job = job)

# write out results tibble ------------------
file_name <- paste("results_", job_num, ".rds", sep = "")
model %>% 
  select(-splits) %>% 
  bind_cols(., job) %>% 
  select(-n_split) %>% 
  write_rds(., file_name)
