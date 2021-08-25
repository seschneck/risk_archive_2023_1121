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


# Only using bootstrap resampling for model selection   
# FIX: Need to nest by subid for splits
# grouped k-fold (10 x)
split_data <- function(d, job, n_splits) {
  
  # d: (training) dataset to be resampled
  # job: single-row job-specific tibble
  # n_splits: total number of splits, obtained from jobs before slicing

  splits <- d %>% 
      bootstraps(times = n_splits, strata = "y")
  
  return(splits)
  
}


build_recipe <- function(d, job) {
  
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  # lapse = outcome variable (lapse/no lapse)
  # feature_set = data streams to include features for
  
  algorithm <- job$algorithm
  feature_set <- job$feature_set
  
  rec <- recipe(lapse ~ ., data = d) %>%
    # make label factor
    update_role(subid, new_role = "id variable") %>%
    step_impute_knn(all_predictors()) # %>%
    # FIX: downsample majority class in recipe?
    # themis::step_upsample(y)

  if (feature_set == "all_context") {
    rec <- rec 
  } else if (feature_set == "sms_context") {
    rec <- rec %>%
      step_rm(contains("voice"), contains("all"))
  } else if (feature_set == "voice_context") {
    rec <- rec %>%
      step_rm(contains("sms"), contains("all"))
  } else if (feature_set == "all") {
    rec <- rec %>%
      step_rm(contains("context"))
  } else if (feature_set == "sms") {
    rec <- rec %>%
      step_rm(contains("voice"), contains("all"), contains("context"))
  } else if (feature_set == "voice") {
    rec <- rec %>%
      step_rm(contains("sms"), contains("all"), contains("context"))
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
