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


split_data <- function(d, job, n_splits, n_repeats = NULL) {
  
  # d: (training) dataset to be resampled
  # job: single-row job-specific tibble
  # n_splits: total number of splits, obtained from jobs before slicing
  # n_repeats: total number of repeats, if repeated CV
  
  cv_type <- job$cv_type
  
  if (cv_type == "boot") {
    splits <- d %>% 
      bootstraps(times = n_splits, strata = "y")
  }
  
  if (cv_type == "kfold") {
    splits <- d %>% 
      vfold_cv(v = n_splits, strata = "y")
  }
  
  if (cv_type == "repeated") {
    splits <- d %>% 
      vfold_cv(v = n_splits, repeats = n_repeats, strata = "y")
  }
  
  return(splits)
  
}


build_recipe <- function(d, job) {
  
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  
  algorithm <- job$algorithm
  feature_set <- job$feature_set
  preprocess <- job$preprocess
  if (preprocess == "ppwk26_drop_25") {
    drop_pct_miss <- .25
  }
  
  rec <- recipe(y ~ ., data = d) %>%
    update_role(subid, new_role = "id variable") %>%
    step_select(where(~ sum(is.na(.x))/length(.x) < drop_pct_miss)) %>% 
    step_impute_knn(all_predictors()) %>%
    themis::step_upsample(y)
    
    if (feature_set == "items") {
      rec <- rec %>% 
        step_rm(contains("scale"))
    }
    
    if (feature_set == "scales") {
      rec <- rec %>% 
        step_rm(contains("item"))
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
