# setup chtc jobs & associated files/folders

library(tidyverse)
library(here)
path_training_controls <- here("ema/chtc/training/training_controls_ema.R") 
source(here("../lab_support/chtc/static_files/input/fun_chtc.R"))

make_jobs(path_training_controls, overwrite_jobs = FALSE)


# NOTE: Currently glmnet does not have separate folds/splits - training function takes in whole splits
# object and tunes for lambda. As a result the jobs file will show NA for n_fold, n_repeat, and hp2.
# The hp2 ranges for tuning will be detected by fit functions from training_controls.R 