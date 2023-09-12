library(tidyverse)
library(tidymodels)
library(probably)

devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/chtc/static_files/fun_chtc.R?raw=true")

metrics_raw <- 
  read_csv(file.path("P:/studydata/risk/models/insight/inner_metrics_v2_nested.csv"), 
           col_types = "iiiiccdddcdddddddc") |> 
  glimpse()

metrics_avg <- metrics_raw |> 
  filter(outcome == "dichotomous" & feature_set == "aase_only") %>% 
  group_by(outcome, algorithm, feature_set, 
           hp1, hp2, hp3, resample, outer_split_num) |> 
  summarize(across(c(accuracy, bal_accuracy, roc_auc, sens, spec, ppv, npv),
                   median),
            n_jobs = n(), .groups = "drop") |> 
  relocate(outer_split_num, n_jobs) |> 
  arrange(outer_split_num, desc(roc_auc))

configs_best <- metrics_avg |> 
  group_by(outer_split_num) |> 
  arrange(desc(roc_auc)) |> 
  slice(1) |> 
  ungroup() |> 
  relocate(roc_auc, .before = accuracy)

source(file.path("P:/studydata/risk/chtc/insight/train_xgboost_1week_nested_1_x_10_3_x_10_v2_batch3", 
                 "input", "training_controls.R"))

d <- read_csv(file.path("P:/studydata/risk/chtc/insight/train_xgboost_1week_nested_1_x_10_3_x_10_v2_batch3", 
                        "input", "data_trn.csv"),
              show_col_types = FALSE)

d <- format_data(d) |> 
  mutate(id_obs = 1:nrow(d))  # tmp add for linking obs to SHAPs

splits <- d %>% 
  make_splits(cv_resample_type, cv_resample, cv_outer_resample, 
              cv_inner_resample, cv_group, seed_splits)

# split that works
split_num_works <- 1

d_in_works <- training(splits$splits[[split_num_works]]) |> 
  select(-id_obs)  # not used for training; only needed in d_out to tag SHAPs
d_out_works <- testing(splits$splits[[split_num_works]])

config_best_works <- configs_best |> 
  slice(split_num_works) |> 
  rename(n_jobs_in = n_jobs, accuracy_in = accuracy, 
         bal_accuracy_in = bal_accuracy,
         roc_auc_in = roc_auc, sens_in =  sens, spec_in = spec, 
         ppv_in = ppv, npv_in = npv)

rec_works <- build_recipe(d = d_in_works, config = config_best_works)
model_best_works <- fit_best_model(config_best_works, rec_works, d_in_works, "classification")

feat_out_works <- rec_works %>% 
  prep(training = d_in_works, strings_as_factors = FALSE) %>% 
  bake(new_data = d_out_works)

preds_prob_works <- predict(model_best_works, feat_out_works,
                      type = "prob")
preds_class_works <- predict(model_best_works, feat_out_works, type = "class")$.pred_class

set.seed(2468)
cal_split_works <- d_in_works |> 
  group_initial_split(group = all_of(cv_group), prop = 3/4)

d_cal_in_works <- training(cal_split_works)
d_cal_out_works <- testing(cal_split_works)

feat_cal_out_works <- rec_works %>% 
  prep(training = d_cal_in_works, strings_as_factors = FALSE) %>% 
  bake(new_data = d_cal_out_works) 

model_cal_works <- fit_best_model(config_best_works, rec_works, d_cal_in_works, "classification")

beta_works <- predict(model_cal_works, feat_cal_out_works,
                type = "prob") |> 
  mutate(truth = feat_cal_out_works$y) |> 
  cal_estimate_beta(truth = truth,
                    estimate = dplyr::starts_with(".pred_"),
                    smooth = TRUE)
preds_prob_beta_works <- preds_prob_works |> 
  cal_apply(beta_works)

# split that doesn't work
split_num_err <- 19 # 9, 19

d_in_err <- training(splits$splits[[split_num_err]]) |> 
  select(-id_obs)  # not used for training; only needed in d_out to tag SHAPs
d_out_err <- testing(splits$splits[[split_num_err]])

config_best_err <- configs_best |> 
  slice(split_num_err) |> 
  rename(n_jobs_in = n_jobs, accuracy_in = accuracy, 
         bal_accuracy_in = bal_accuracy,
         roc_auc_in = roc_auc, sens_in =  sens, spec_in = spec, 
         ppv_in = ppv, npv_in = npv)

rec_err <- build_recipe(d = d_in_err, config = config_best_err)
model_best_err <- fit_best_model(config_best_err, rec_err, d_in_err, "classification")

feat_out_err <- rec_err %>% 
  prep(training = d_in_err, strings_as_factors = FALSE) %>% 
  bake(new_data = d_out_err)

preds_prob_err <- predict(model_best_err, feat_out_err,
                            type = "prob")
preds_class_err <- predict(model_best_err, feat_out_err, type = "class")$.pred_class

set.seed(2468)
cal_split_err <- d_in_err |> 
  group_initial_split(group = all_of(cv_group), prop = 3/4)

d_cal_in_err <- training(cal_split_err)
d_cal_out_err <- testing(cal_split_err)

feat_cal_out_err <- rec_err %>% 
  prep(training = d_cal_in_err, strings_as_factors = FALSE) %>% 
  bake(new_data = d_cal_out_err) 

model_cal_err <- fit_best_model(config_best_err, rec_err, d_cal_in_err, "classification")

# produces error
beta_err <- predict(model_cal_err, feat_cal_out_err,
                      type = "prob") |> 
  mutate(truth = feat_cal_out_err$y) |> 
  cal_estimate_beta(truth = truth,
                    estimate = dplyr::starts_with(".pred_"),
                    smooth = TRUE)
# preds_prob_beta_err <- preds_prob_err |> 
#   cal_apply(beta_err)

# comparisons
janitor::tabyl(preds_class_works) # all negative class
janitor::tabyl(preds_class_err) # 93%/6% split negative/positive class
# but tried another working split (2) and there is a 93/6 split in that one as well

glimpse(feat_cal_out_works)
hist(feat_cal_out_works$aase_total) # has variability
length(unique(feat_cal_out_works$aase_total)) # 22 unique values

glimpse(feat_cal_out_err)
hist(feat_cal_out_err$aase_total) # has variability
length(unique(feat_cal_out_err$aase_total)) # 25 unique values


