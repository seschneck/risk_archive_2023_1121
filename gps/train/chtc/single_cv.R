#Packages & function libraries--------------------------------------------------------------------
library(caret)
library(readr)
library(dplyr)
library(stringr)
source("functions_chtc.R")

args <- commandArgs(trailingOnly=TRUE) 
i_job <- as.numeric(args[1])   #Process/job arg starts at 0      


#Open data files and set arugments for this job------------------------
jobs <- read_rds("jobs.rds")
d <- read_rds("data.rds")
iters <- read_rds("iters.rds")

#testing
# jobs <- read_rds(file.choose())
# d <- read_rds(file.choose())
# iters <- read_rds(file.choose())

cv_type <- iters$cv_type
i_iter_inner <- jobs$iter_inner[i_job + 1]
model_type <- jobs$model_types[i_job + 1]
features <- jobs$features[i_job + 1]
model_name <- jobs$model_name[i_job + 1]
raw_data_name <- jobs$raw_data_name[i_job + 1]

# if (cv_type == 'nested') {
#   i_iter_outer <- jobs$iter_outer[i_job + 1]
# }

#Train and evaluate via nested CV-----------------------------------------------
# if (cv_type == 'nested') {
#   print (str_c("JobNum: ", i_job, 
#                "; Model name: ", model_name,
#                "; Iter_outer: ", i_iter_outer, 
#                "; Iter_inner: ", i_iter_inner, 
#                "; Model type: ", model_type, 
#                "; Features: ", features))
#   
#   results <- fit_model(d, iters, i_iter_outer, i_iter_inner, 
#                        model_type = model_type, features = features)
# }

if (cv_type == 'single_loop') {
  print (str_c("JobNum: ", i_job, 
               "; Model name: ", model_name,
               "; Iter_inner: ", i_iter_inner, 
               "; Model type: ", model_type, 
               "; Features: ", features))
  
  results <- fit_model(d, iters, i_iter_outer = NA, i_iter_inner, 
                       model_type = model_type, features = features)
}

results <- results %>% 
  mutate(model_name = model_name,
         raw_data_name = raw_data_name) %>% 
  select(model_name, raw_data_name, everything())

write_rds(results, str_c("results_", i_job, ".rds"))