#libraries-------------------
source("P:/toolboxes/IAML/IAML.R")
library(caret)
library(tidyverse)

#Model Info----------------------------------------------------------------------------------
model_name <- "GPS" 
root_path <- file.path("GPS/data/models")

#read in jobs file---------------
jobs <- read_rds(file.path(root_path, model_name, "input", "jobs.rds"))
#d <- read_rds(file.path(root_path, model_name, "input", "data.rds"))
#d_orig <- d

#open all jobs---------------
unzip_chtc(model_name = model_name, root_path = root_path)
files <- dir(file.path(root_path, model_name, "output", "results"), pattern = "*.rds", full.names = FALSE)
if (length(files) == nrow(jobs)) {
  message(str_c(length(files), " results files detected. All good"))
}else{
  message("Problem detected. Missing results files: ", str_c(which(!(1:nrow(jobs)) %in% as.numeric(str_extract(files, "(\\d)+"))), "; "))
}

#GET BEST CONFIG----------------

#make all jobs file for all iters across all results.rds
all_jobs <- files %>%
  map_df(~ read_rds(file.path(root_path, model_name, "output", "results", .))) %>%
  mutate(hp3 = as.character(hp3),
         hp3 = replace_na(hp3, "no_hp3"),
         hp3 = as.factor(hp3)) %>% 
  write_rds(file.path(root_path, model_name, "output", "all_jobs.rds"))

#make all_configs file mean across folds by config
all_configs <- all_jobs %>%
  group_by(model_type, features, hp1, hp2, hp3) %>% 
  summarise(mean_auc = mean(auc_inner, na.rm = TRUE), nfolds = n()) %>% 
  ungroup() %>% 
  write_rds(file.path(root_path, model_name, "output", "all_configs.rds"))
  
#check all_configs
#FIX: HANDLE NA CASE FOR AUC WITH SOME CHECKS OF NUMBER OF NA ACROSS FOLDS
all_configs %>% 
  summarise(n_configs = n(), 
            min_auc = min(mean_auc, na.rm = TRUE),
            max_auc = max(mean_auc, na.rm = TRUE),
            min_nfolds = min(nfolds),
            max_nfolds = max(nfolds)) %>% 
  print()

#make a best_configs file that chooses the configuration with the highest auc within each inner fold
final_model_info <- all_configs %>% 
  mutate(the_rank = rank(-mean_auc, ties.method = "random")) %>% 
  filter(the_rank == 1) %>% 
  select(-the_rank) %>% 
  write_rds(file.path(root_path, model_name, "output", "final_model_info.rds")) %>% 
  print()