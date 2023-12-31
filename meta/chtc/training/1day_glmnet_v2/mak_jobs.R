# setup chtc jobs & associated files/folders

# NOTE: this script should be run from a project on the repo (i.e., RISK or RISK2) 
# for the relative paths to access local copy of lab_support to work

# Set path for training_controls.R -------------------- 
path_training_controls <- "./meta/chtc/training/1day_glmnet_v2/training_controls.R" # location of study specific training_controls.R (can use relative path if in repo) 


# Make jobs -------------------- 
suppressPackageStartupMessages(library(tidyverse))
source("../lab_support/chtc/static_files/input/fun_chtc.R", echo = FALSE)

# Don't forget to add/remove hyperparameters using/not using for algorithms
make_jobs(path_training_controls)


