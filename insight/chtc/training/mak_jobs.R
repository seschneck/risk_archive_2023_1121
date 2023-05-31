# setup chtc jobs & associated files/folders

library(tidyverse)
library(here)
path_training_controls <- here("insight/chtc/training/training_controls_insight.R") 
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/chtc/static_files/fun_chtc.R?raw=true")

make_jobs(path_training_controls, overwrite_jobs = FALSE)
