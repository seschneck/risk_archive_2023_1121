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
  # require(kknn)
  require(vip)
})


# splits for grouped repeated k-fold (subid = grouping factor)
make_folds <- function(d, cv_type) {
  
  # d: (training) dataset to be resampled (subid = grouping id)
  # cv_type: variable to get n_repeats and n_folds
  
  n_repeats <- as.numeric(str_split(cv_type, "_x_")[[1]][1])
  n_folds <- as.numeric(str_split(cv_type, "_x_")[[1]][2])
  
  for (i in 1:n_repeats) {
    fold <- d %>% 
      group_vfold_cv(group = subid, v = n_folds) %>% 
      mutate(n_repeat = i)
    
    folds <- if (i == 1)
      fold
    else
      rbind(folds, fold)
  }
  
  return(folds)
}


build_recipe <- function(d, algorithm, resample) {
  
  # d: (training) dataset from which to build recipe
  # algorithm: glmnet or random_forest
  # lapse = outcome variable (lapse/no lapse)
  # resample = none, up, down, or smote

  
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
    rec <- rec %>% 
      themis::step_upsample(lapse)
  } else if (resample == "down") {
    rec <- rec %>% 
      themis::step_downsample(lapse) 
  } else if (resample == "smote") {
    rec <- rec %>% 
      themis::step_smote(lapse) 
  }
  
  # algorithm specific steps
  if (algorithm == "glmnet" | algorithm == "knn") {
    rec <- rec %>% 
      step_normalize(all_predictors())
  } 
  
  return(rec)
}



