# setup chtc jobs & associated files/folders

library(tidyverse)
library(here)
path_training_controls <- here("ema/chtc/training/training_controls_ema.R") 
source(here("../lab_support/chtc/static_files/fun_chtc.R"))

make_jobs(path_training_controls, overwrite_jobs = TRUE)
