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
library(psych)
library(purrr)
library(kknn)
library(vip)


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
    step_string2factor(all_nominal()) %>% 
    step_knnimpute(all_predictors()) %>% 
    step_dummy(all_nominal(), -lapse)
  
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
  
  
  # FIX: add more if statements to add steps for specific algorithms
  
  return(rec)
}


make_feature_matrices <- function(job, splits, rec) {
  
  # job: single-row job-specific tibble
  # splits: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  
  n_split <- job$n_split
  d_in <- analysis(splits$splits[[n_split]])
  d_out <- assessment(splits$splits[[n_split]])
  
  feat_in <- rec %>% 
    prep(training = d_in) %>% 
    bake(new_data = d_in)
  
<<<<<<< HEAD
  d_in <- analysis(splits$splits[[n_split]])
  d_out <- assessment(splits$splits[[n_split]])
  
  feat_in <- rec %>% 
    prep(training = d_in) %>% 
    bake(new_data = d_in)
  
=======
>>>>>>> 131ded67368c3c431802b55c5a0d731757a26990
  feat_out <- rec %>% 
    prep(training = d_in) %>% 
    bake(new_data = d_out)
  
  return(list(feat_in, feat_out))
  
}


fit_model <- function(feat_in, job) {
  
  # feat_in: feature matrix built from held-in data
  # job: single-row job-specific tibble
  
  algorithm <- job$algorithm
  
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
      fit(lapse ~ .,
          data = feat_in)
  }
  
  return(model)
}


get_metrics <- function(model, feat_out) {
  
  # model: model object fit via fit_model()
  # feat_in: feature matrix built from held-in data
  
  preds <- predict(model, feat_out, type = "class")$.pred_class
  
<<<<<<< HEAD
  cm <- tibble(truth = feat_out$y,
=======
  cm <- tibble(truth = feat_out$lapse,
>>>>>>> 131ded67368c3c431802b55c5a0d731757a26990
               estimate = preds) %>% 
    conf_mat(truth, estimate)
  
  model_metrics <- cm %>% 
    summary(event_level = "second") %>% 
    select(metric = .metric,
           estimate = .estimate) %>% 
    filter(metric %in% c("accuracy", "bal_accuracy",
                         "sens", "spec"))
  
<<<<<<< HEAD
  roc <- tibble(truth = feat_out$y,
                prob = predict(model, feat_out,
                               type = "prob")$.pred_abstinent) %>% 
=======
  roc <- tibble(truth = feat_out$lapse,
                prob = predict(model, feat_out,
                               type = "prob")$.pred_lapse) %>% 
>>>>>>> 131ded67368c3c431802b55c5a0d731757a26990
    roc_auc(prob, truth = truth, event_level = "second") %>% 
    select(metric = .metric,
           estimate = .estimate)
  
  model_metrics <- bind_rows(model_metrics, roc)
  
  return(model_metrics)
}