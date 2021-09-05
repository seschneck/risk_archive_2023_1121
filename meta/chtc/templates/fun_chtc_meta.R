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
make_folds <- function(d, n_folds, n_repeats) {
  
  # d: (training) dataset to be resampled (subid = grouping id)
  # n_splits: total number of splits, obtained from jobs before slicing
  # n_repeats: total number of repeats
  
  folds <- tibble()
  
  for (i in 1:n_repeats) {
    fold <- d %>% 
      group_vfold_cv(group = subid, v = n_folds) %>% 
      mutate(n_repeat = i)
    
    if (nrow(folds) != 0) {
      folds <- folds %>% 
        bind_rows(fold)
    } else  folds <- fold
  }
  
  return(folds)
}


build_recipe <- function(d, job) {
  
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  # lapse = outcome variable (lapse/no lapse)
  # feature_set = all_features or passive_only
  # resample = none, up, down, or smote
   
  algorithm <- job$algorithm
  feature_set <- job$feature_set
  resample <- job$resample
  
  rec <- recipe(lapse ~ ., data = d) %>%
    step_string2factor(lapse, levels = c("no", "yes")) %>% 
    update_role(subid, dttm_label, new_role = "id variable") %>%
    step_string2factor(all_nominal()) %>% 
    step_impute_median(all_numeric()) %>% 
    step_impute_mode(all_nominal(), -lapse) %>% 
    step_zv(all_predictors()) %>% 
    step_dummy(all_nominal(), -lapse)
    
  
  # filter out context features if job uses passive only
  if (feature_set == "passive_only") {
    rec <- rec %>%
      step_rm(contains("context"))
  } 
  
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


make_features <- function(job, n_repeats, folds, rec) {
  
  # job: single-row job-specific tibble
  # splits: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  
  n_fold <- job$n_fold
  n_repeat <- job$n_repeat
  
  fold_index <- n_fold + (n_repeat - 1) * n_repeats
  
  d_in <- analysis(folds$splits[[fold_index]])
  d_out <- assessment(folds$splits[[fold_index]])
  
  feat_in <- rec %>% 
    prep(training = d_in) %>% 
    bake(new_data = NULL)
  
  feat_out <- rec %>% 
    prep(training = d_in) %>% 
    bake(new_data = d_out)
  
  return(list(feat_in = feat_in, feat_out = feat_out))
  
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
  } else if (algorithm == "knn") {
    model <- nearest_neighbor(neighbors = job$hp1) %>% 
      set_engine("kknn") %>% 
      set_mode("classification") %>% 
      fit(lapse ~ .,
          data = feat_in)
  } else if (algorithm == "glmnet") {
    model <- logistic_reg(penalty = job$hp1,
                          mixture = job$hp2) %>% 
      set_engine("glmnet") %>% 
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
  
  cm <- tibble(truth = feat_out$lapse,
               estimate = preds) %>% 
    conf_mat(truth, estimate)
  
  model_metrics <- cm %>% 
    summary(event_level = "second") %>% 
    select(metric = .metric,
           estimate = .estimate) %>% 
    filter(metric %in% c("accuracy", "bal_accuracy",
                         "sens", "spec")) %>% 
    suppressWarnings() # warning not about metrics we are returning
  
  roc <- tibble(truth = feat_out$lapse,
                prob = predict(model, feat_out,
                               type = "prob")$.pred_yes) %>% 
    roc_auc(prob, truth = truth, event_level = "second") %>% 
    select(metric = .metric,
           estimate = .estimate)
  
  model_metrics <- bind_rows(model_metrics, roc)
  
  return(model_metrics)
}
