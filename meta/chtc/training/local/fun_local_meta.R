# Description: 

# load libraries
suppressPackageStartupMessages({
  require(dplyr)
  require(recipes)
  require(parsnip)
  require(themis)
  require(tune)
  require(yardstick)
  require(rsample)
  require(ranger)
  require(psych)
  require(purrr)
  require(glmnet)
  require(kknn)
  require(vip)
})


make_splits <- function(d, cv_type) {
  
  # d: (training) dataset to be resampled 
  # job: single job to get cv_type parameter from - may directly pass in cv_type
  # if it becomes global parameter
  
  # bootstrap splits
  if (cv_type == "boot") {
    # add bootstap splits here
  }
  
  kfold_type <- if (str_split(str_remove(cv_type, "_x"), "_")[[1]][1] == "kfold") {
    "kfold"
  } else if (str_split(str_remove(cv_type, "_x"), "_")[[1]][1] == "group") {
    "group_kfold"
  }
  
  
  # kfold splits
  if (kfold_type == "kfold"){ 
    n_repeats <- as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][2])
    n_folds <- as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][3])
    
    split <- d %>% 
      vfold_cv(v = n_folds, repeats = n_repeats) 
  }
  
  # grouped kfold splits 
  # grouping variable is hardcoded to be subid
  if (kfold_type == "group_kfold"){ 
    n_repeats <- as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][3])
    n_folds <- as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][4])
    
    for (i in 1:n_repeats) {
      split <- d %>% 
        group_vfold_cv(group = subid, v = n_folds) %>% 
        mutate(n_repeat = i)
      
      splits <- if (i == 1)
        split
      else
        rbind(splits, split)
    }
  }
  
  
  return(splits)
}


# build_recipe <- function(d, algorithm, resample) {
# 
#   # d: (training) dataset from which to build recipe
#   # algorithm: glmnet or random_forest
#   # lapse = outcome variable (lapse/no lapse)
#   # resample = none, up, down, or smote
# 
# 
#   rec <- recipe(lapse ~ ., data = d) %>%
#     step_string2factor(lapse, levels = c("no", "yes")) %>%
#     update_role(subid, dttm_label, new_role = "id variable") %>%
#     step_string2factor(all_nominal()) %>%
#     step_impute_median(all_numeric()) %>%
#     step_impute_mode(all_nominal(), -lapse) %>%
#     step_zv(all_predictors()) %>%
#     step_dummy(all_nominal(), -lapse)
# 
# 
#   # control for unbalanced outcome variable
#   if (resample == "up") {
#     rec <- rec %>%
#       themis::step_upsample(lapse, seed = 10)
#   } else if (resample == "down") {
#     rec <- rec %>%
#       themis::step_downsample(lapse, seed = 10)
#   } else if (resample == "smote") {
#     rec <- rec %>%
#       themis::step_smote(lapse, seed = 10)
#   }
# 
#   # algorithm specific steps
#   if (algorithm == "glmnet" | algorithm == "knn") {
#     rec <- rec %>%
#       step_normalize(all_predictors())
#   }
# 
#   return(rec)
# }

build_recipe <- function(d, algorithm, resample) {
  
  # d: (training) dataset from which to build recipe
  # lapse = outcome variable (lapse/no lapse)
  # feature_set = all_features or passive_only
  # resample = type + under_ratio or none
  
  
  if (resample == "none") {
    resample <- resample
  } else {
    under_ratio <- as.numeric(str_split(resample, "_")[[1]][2])
    resample <- str_split(resample, "_")[[1]][1]
  }
  
  
  rec <- recipe(lapse ~ ., data = d) %>%
    step_string2factor(lapse, levels = c("no", "yes")) %>% 
    update_role(subid, dttm_label, new_role = "id variable") %>%
    step_string2factor(all_nominal()) %>% 
    step_impute_median(all_numeric()) %>% 
    step_impute_mode(all_nominal(), -lapse) %>% 
    step_zv(all_predictors()) %>% 
    step_dummy(all_nominal(), -lapse)
  
  
  
  # control for unbalanced outcome variable
  if (resample == "up") {
    if (under_ratio != 1) { over_ratio <- under_ratio / (under_ratio + 1)
    } else over_ratio <- under_ratio
    rec <- rec %>% 
      themis::step_upsample(lapse, over_ratio = over_ratio, seed = 10)
  } else if (resample == "down") {
    rec <- rec %>% 
      themis::step_downsample(lapse, under_ratio = under_ratio, seed = 10) 
  } else if (resample == "smote") {
    if (under_ratio != 1) { over_ratio <- under_ratio / (under_ratio + 1)
    } else over_ratio <- under_ratio
    rec <- rec %>% 
      themis::step_smote(lapse, over_ratio = over_ratio, seed = 10) 
  }
  
  # algorithm specific steps
  if (algorithm == "glmnet" | algorithm == "knn") {
    rec <- rec %>% 
      step_normalize(all_predictors())
  } 
  
  return(rec)
}



