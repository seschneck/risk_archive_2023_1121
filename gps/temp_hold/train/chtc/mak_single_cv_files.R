#Libraries and Functions------------------------------------------------------------------------------------
library(tidyverse)
source('P:/Toolboxes/IAML/IAML.R') #for simplify_cv(), mak_iters(), mak_jobs()

#CV settings----------------------------------------------------------------------------------------------
n_folds_inner <- 10
n_repeats_inner <- 1
n_folds_outer <- 0
n_repeats_outer <- 0
grouped <- TRUE
iter_seed <- 19690127

#Models Info----------------------------------------------------------------------------------
raw_data_path <- "GPS/data"  #path to the raw data folder
model_path <- file.path(raw_data_path, "models")     #path to model folders with inputs/outputs
n_models <- 1
model_info = vector("list", n_models)

model_info[[1]] <- list(model_name = "GPS",
                        raw_data_name = "data_gps.rds",
                        model_types = c("glmnet", "ranger"),
                        features = c("all"))

#Create Inputs for Models-----------------------------------
input_path <- "input"   #/model_path/input
output_path <- "output" #/model_path/output
for(i_model in seq_along(model_info)){
  model_name <- model_info[[i_model]]$model_name
  if (!dir.exists(file.path(model_path, model_name))) dir.create(file.path(model_path, model_name))
  if (!dir.exists(file.path(model_path, model_name, input_path))) dir.create(file.path(model_path, model_name, input_path))
  if (!dir.exists(file.path(model_path, model_name, output_path))) dir.create(file.path(model_path, model_name, output_path))
  if (!dir.exists(file.path(model_path, model_name, output_path, "results"))) dir.create(file.path(model_path, model_name, output_path, "results"))
  if (!dir.exists(file.path(model_path, model_name, output_path, "out"))) dir.create(file.path(model_path, model_name, output_path, "out")) 
  if (!dir.exists(file.path(model_path, model_name, output_path, "err"))) dir.create(file.path(model_path, model_name, output_path, "err"))
  raw_data_name <- model_info[[i_model]]$raw_data_name
  model_types <- model_info[[i_model]]$model_types
  features <- model_info[[i_model]]$features
  
  d <- read_rds(file.path(raw_data_path, raw_data_name))
  iters <- mak_iters(d, n_folds_outer, n_repeats_outer, n_folds_inner, n_repeats_inner, iter_seed, grouped = grouped, group_id = "subid")
  jobs <- mak_jobs(iters, model_info[[i_model]])
  message(str_c(nrow(jobs), " jobs created for ", model_name))
  
  write_rds(d, file.path(model_path, model_name, input_path, "data.rds"))
  write_rds(iters, file.path(model_path, model_name, input_path, "iters.rds"))
  write_rds(jobs, file.path(model_path, model_name, input_path, "jobs.rds"))
}
