# Description: 

# load libraries
library(dplyr)
library(recipes)
library(parsnip)
library(themis)
library(tune)
library(yardstick)
library(rsample)
library(ranger)


# grouped k-fold - grouped by subid
split_data <- function(d, job, n_splits, n_repeats) {
  
  # d: (training) dataset to be resampled (subid = grouping id)
  # job: single-row job-specific tibble
  # n_splits: total number of splits, obtained from jobs before slicing
  # n_repeats: total number of repeats
  splits <- tibble()
  
  for (i in 1:n_repeats) {
    split <- d %>% 
      group_vfold_cv(group = subid, v = n_splits) %>% 
      mutate(n_repeat = i)
    
    if (nrow(splits) != 0) {
      splits <- splits %>% 
        bind_rows(split)
    } else  splits <- split
  }
  
  return(splits)
}


build_recipe <- function(d, job) {
  
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  # lapse = outcome variable (lapse/no lapse)
  # feature_set = all_features or passive_only
  # upsample = none, up, down, or smote
  
  algorithm <- job$algorithm
  feature_set <- job$feature_set
  upsample <- job$upsample
  
  rec <- recipe(lapse ~ ., data = d) %>%
    step_string2factor(lapse, levels = c("no", "yes")) %>% 
    update_role(subid, new_role = "id variable") %>%
    step_string2factor(all_nominal_predictors()) %>% 
    step_impute_knn(all_predictors()) %>% 
    step_dummy(all_nominal_predictors())
  
  # filter out context features if job uses passive only
  if (feature_set == "passive_only") {
    rec <- rec %>%
      step_rm(contains("context"))
  } 
   
  # control for uneven outcome variable
  if (upsample == "up") {
    rec <- rec %>% 
      themis::step_upsample(lapse)
  } else if (upsample == "down") {
    rec <- rec %>% 
      themis::step_downsample(lapse)
  } else if (upsample == "smote") {
    rec <- rec %>% 
      themis::step_smote(lapse, neighbors = 5)
  }
  
  return(rec)
}


fit_model <- function(rec, splits, job) {
  
  # rec: recipe (created manually or via build_recipe() function)
  # splits: rset object that contains all resamples
  # job: single-row job-specific tibble
  
  algorithm <- job$algorithm
  n_split <- job$n_split
  
  split_job <- splits[n_split, ]
  rset_job <- manual_rset(split_job$splits, split_job$id)
  
  if (algorithm == "random_forest") {
    model <- rand_forest(mtry = job$hp1,
                         min_n = job$hp2,
                         trees = job$hp3) %>% 
      set_engine("ranger",
                 importance = "impurity",
                 respect.unordered.factors = "order",
                 oob.error = FALSE,
                 seed = 102030) %>% 
      set_mode("classification") %>% 
      fit_resamples(preprocessor = rec, 
                    resamples = rset_job,  
                    metrics = metric_set(accuracy, bal_accuracy, roc_auc,
                                         sensitivity, specificity))
    
  }
  
  return(model)
}
