# This is a study level script for defining the recipe and global attributes that are 
# the same across jobs.

# Note: Should have a single build_recipe function to be compatible with fit script.   


# SET GLOBAL PARAMETERS --------
data_trn <- "features_aggregate.rds"
name_job <- "feature_selection" # the name of the job to set folder names
feature_set <- c("feat_all", "feat_all_passive") # 1+ feature sets 
feature_fun_type <- c("raw", "perc", "perc_raw")
algorithm <- c("glmnet", "random_forest", "knn") # 1+ algorithm (glmnet, random_forest) 
resample <- c("up_1", "down_1", "smote_1") # 1+ resampling methods (up, down, smote, or none)
# all resamples should be in form resample type underscore under_ratio (e.g., 3 = 25% minority cases)
y_col_name <- "label" # outcome variable - will be changed to y in recipe for consistency across studies 
cv_type <- "group_kfold_1_x_10" # cv type - can be boot, group_kfold, or kfold
# format for kfold should be kfold_n_repeats_x_n_folds (e.g., kfold_1_x_10, group_kfold_10_x_10)
# determine where to pass in global cv_type parameter
group <- "subid" # grouping variable for grouped k-fold - remove if not using group_kfold
perf_metric <- "bal_accuracy" # used for determining best model in post-processing script 
# note other classification metrics will still be collected (bal_accuracy, roc_auc, sens, spec, accuracy)
remove_nzv <- TRUE # using as variable instead of in recipe to be able to calculate features before removing nzv

# CHANGE ALGORITHM-SPECIFIC HYPERPARAMETERS -------------------
hp1_glmnet <- seq(0.5, 1, length.out = 11) # alpha (mixture) 
hp2_glmnet_min <- -9 # min for penalty grid - will be passed into exp(seq(min, max, length.out = out))
hp2_glmnet_max <- 2 # max for penalty grid
hp2_glmnet_out <- 100 # length of penalty grid


# CHANGE STUDY PATHS -------------------- 
path_jobs <- "P:/studydata/risk/chtc/meta/jobs/training" # location of where you want your jobs to be setup
path_data <- "P:/studydata/risk/chtc/meta/jobs/features/features_all/output" # location of data set


# BUILD RECIPE ---------

# Classification recipe for meta study

build_recipe <- function(d, job) {
  
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  
  # get relevant info from job (algorithm, feature_set, resample, under_ratio)
  algorithm <- job$algorithm
  feature_set <- job$feature_set
  feature_fun_type <- job$feature_fun_type
  
  if (job$resample == "none") {
    resample <- job$resample
  } else {
    resample <- str_split(job$resample, "_")[[1]][1]
    under_ratio <- as.numeric(str_split(job$resample, "_")[[1]][2])
  }
  
  # Set recipe steps generalizable to all model configurations
  rec <- recipe(y ~ ., data = d) %>%
    update_role(subid, dttm_label, new_role = "id variable") %>%
    step_rm(label_num, id_quit_date, starts_with("id_"), starts_with("label_")) %>% 
    step_string2factor(y, levels = c("no", "yes")) %>% 
    # reference group will be first level in factor - specify levels to choose reference group
    step_string2factor(all_nominal()) %>% 
    step_zv(all_predictors()) %>%
    step_impute_median(all_numeric()) %>%
    step_impute_mode(all_nominal(),  -y) 
  
  # If statements for filtering features based on feature set
  if (feature_set == "feat_all_passive") {
    rec <- rec %>%
      step_rm(starts_with("sms"), -contains("passive")) %>% 
      step_rm(starts_with("voi"), -contains("passive"))
  } 
  # filter based on feature function type
  if (feature_fun_type == "raw") {
    rec <- rec %>% 
      step_rm(contains("prate")) %>% 
      step_rm(contains("pprop")) %>% 
      step_rm(contains("pmean"))

  } else if (feature_fun_type == "perc") {
    rec <- rec %>% 
      step_rm(contains("rrate")) %>% 
      step_rm(contains("rprop")) %>% 
      step_rm(contains("rmean"))
  }
  
  # resampling options for unbalanced outcome variable
  if (resample == "down") {
    rec <- rec %>% 
      themis::step_downsample(y, under_ratio = under_ratio, seed = 10) 
  } else if (resample == "smote") {
    if (under_ratio != 1) { over_ratio <- under_ratio / (under_ratio + 1)
    } else over_ratio <- under_ratio
    rec <- rec %>% 
      themis::step_smote(y, over_ratio = over_ratio, seed = 10) 
  } else if (resample == "up") {
    if (under_ratio != 1) { over_ratio <- under_ratio / (under_ratio + 1)
    } else over_ratio <- under_ratio
    rec <- rec %>% 
      themis::step_upsample(y, over_ratio = over_ratio, seed = 10)
  }
  
  # algorithm specific steps
  if (algorithm == "glmnet") {
    rec <- rec  %>%
      step_dummy(all_nominal(), -y) %>%
      step_normalize(all_numeric())
  } 
  
  if (algorithm == "knn") {
    rec <- rec  %>% 
      step_dummy(all_nominal(), -y) %>% 
      step_normalize(all_predictors())
  } 
  
  return(rec)
}
