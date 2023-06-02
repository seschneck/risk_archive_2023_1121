# This is a study level script for defining the recipe and global attributes that are 
# the same across jobs.

# DO NOT EDIT DIRECTLY IN THIS SCRIPT - this script serves as a template with all 
# possible parameters specified/demo'd

#EDIT THIS
study <- "insight"
data_type <- "all"   # but still need to change more (e.g., feature set) to switch data_type
version <- "v1"
batch <- "batch1" # specify a batch (numbered sequentially) if you need to add to an existing set of jobs (e.g., add hyperparameters but maintain version, algorithm, cv, etc.)
algorithm <- "glmnet" # specify one algorithm per training control file - can be glmnet, knn, random_forest, xgboost


# GEF: search on "DECIDE" to find remaining decision points/updates


# SET GLOBAL PARAMETERS --------
ml_mode <- "classification"   # regression or classification

if (algorithm == "random_forest") {
  feature_set <- c("insight_only") # eventually also "comparison"
  resample <- c("none", "up_1", "down_1", "up_.5", "down_2") # DECIDE
} else { # xgb & glmnet
  feature_set <- c("insight_only_dummy",
                   "insight_only_ordinal") # also "comparison_dummy/ordinal"
  resample <- c("none", "up_1", "down_1", "smote_1",
                "up_.5", "down_2") # DECIDE
}

data_trn <- "insight.csv" # set to NULL if using chtc staging for large data

y_col_name <- "lapse" # outcome variable - will be changed to y in recipe for consistency across studies 
y_level_pos <- "yes" # character string of the outcome variable's positive level (e.g., "yes", "abstinent")
y_level_neg <- "no" # character string of the outcome variable's negative level (e.g., "no", "smoking")

# CV PARAMETERS
# All cv parameters must have a value or be set to NULL
# cv_resample will be used to specify kfold and bootstrapping splits (non-nested)
# nested cv should use cv_inner_resample and cv_outer_resample instead of cv_resample
# see resampling demo in lab_support/chtc for examples
cv_resample_type <- "kfold" # can be boot, kfold, or nested # DECIDE
cv_resample = "1_x_10" # can be repeats_x_folds (e.g., 1_x_10, 10_x_10) or number of bootstraps (e.g., 100)
cv_inner_resample <- NULL # can also be a single number for bootstrapping (i.e., 100)
cv_outer_resample <- NULL # outer resample will always be kfold
cv_group <- "subid" # set to NULL if not grouping


cv_name <- if_else(cv_resample_type == "nested",
                   str_c(cv_resample_type, "_", cv_inner_resample, "_",
                         cv_outer_resample),
                   str_c(cv_resample_type, "_", cv_resample))

# SET STUDY PATHS
# the name of the job to set folder names
name_job <- str_c("train_", version, "_", algorithm, "_", batch, "_", cv_name) 
# location of where you want your jobs to be setup
path_jobs <- str_c("P:/studydata/risk/chtc/", study) 
# location of data set
path_data <- str_c("P:/studydata/risk/data_processed/", study) 


# SET ALGORITHM-SPECIFIC HYPERPARAMETERS
hp1_glmnet <- c(0.05, seq(.1, 1, length.out = 10)) # alpha (mixture)
hp2_glmnet_min <- -8 # min for penalty grid - will be passed into exp(seq(min, max, length.out = out))
hp2_glmnet_max <- 2 # max for penalty grid
hp2_glmnet_out <- 200 # length of penalty grid

hp1_knn <- seq(5, 255, length.out = 26) # neighbors (must be integer)

# DECIDE
# changed mtry already [max = length(unique(d$ema_10))]
# min_n stays the same? or change?
# trees - should it just be 10 then..?
hp1_rf <- c(1:11) # mtry (p/3 for reg or square root of p for class)
hp2_rf <- c(2, 15, 30) # min_n
hp3_rf <- 1500 # trees (10 x's number of predictors)

# DECIDE
hp1_xgboost <- c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.3, .4)  # learn_rate
hp2_xgboost <- c(1, 2, 3, 4) # tree_depth
hp3_xgboost <- c(20, 30, 40, 50)  # mtry (previously included 2 and 10 but not needed)
# trees = 500
# early stopping = 20



# CHANGE CHTC SPECIFIC CONTROLS
# DECIDE - study-specific tar needed?
tar <- c("train.tar.gz", "other_project_specific.tar.gz") # name of tar packages for submit file - does not transfer these anywhere 
max_idle <- 1000 # according to CHTC we should set this at 1000 to not flood the server. It will not limit the number of jobs running at one time 
request_cpus <- 1 
request_memory <- "28000MB"
request_disk <- "1600MB"
flock <- FALSE
glide <- FALSE

# FORMAT RAW DATA ---------
# Script should have a single function that classes all variables in the
# training set as they should be set up prior to recipe
# Numeric predictors set to numeric, nominal variables (unordered and ordered) set 
# to factor with levels ordered correctly.  Rename outcome to 'y'.  Can also select
# out any columns that will not be used for prediction across all training batches.
# IMPORTANT: This function should be the same for all training batches. 
# No branching for algorithm, etc.  It is used by post-processing scripts regardless
# of the best configuration selected.

format_data <- function (df){

  df %>% 
    rename(y = !!y_col_name) %>% 
    mutate(y = factor(y, levels = c(!!y_level_pos, !!y_level_neg))) %>%  # set pos class first
    select(-ema_type, -finished, -status, -utc, -starts_with("ema_1_"),
           -ema_1, -starts_with("window"), -ema_start, -ema_end) %>% 
    mutate(across(c(ema_2, ema_3, ema_4, ema_5),
                  ~ factor(. levels = c("0", "1", "2", "3", "4", "5",
                                        "6", "7", "8", "9", "10", "11", "12"),
                           ordered = TRUE)),
           across(c(ema_6, ema_7_, ema_8, ema_9, ema_10),
                  ~ factor(., levels = c("1", "2", "3", "4", "5", "6",
                                         "7", "8", "9", "10", "11"),
                           ordered = TRUE))) %>% 
    rename(insight = ema_10)
    
  # Now include additional mutates to change classes for columns as needed
  # see https://jjcurtin.github.io/dwt/file_and_path_management.html#using-a-separate-mutate
}
  

# BUILD RECIPE ---------

# Script should have a single build_recipe function to be compatible with fit script. 
# Use if statements to customize recipes instead of using multiple different recipe functions.  


# Sample recipe from meta project below - this is for fitting classification models
build_recipe <- function(d, job) {
  
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  
  # get relevant info from job (algorithm, feature_set, resample, under_ratio)
  algorithm <- job$algorithm
  feature_set <- job$feature_set
  
  if (job$resample == "none") {
    resample <- job$resample
  } else {
    resample <- str_split(job$resample, "_")[[1]][1]
    ratio <- as.numeric(str_split(job$resample, "_")[[1]][2])
  }
  
  # Set recipe steps generalizable to all model configurations
  rec <- recipe(y ~ ., data = d) %>%
    step_zv(all_predictors()) %>% 
    step_impute_median(all_numeric_predictors()) %>% 
    step_impute_mode(all_nominal_predictors())
  
  # If statements for filtering features based on feature set
  if (str_detect(feature_set, "insight")) {
    rec <- rec %>%
      step_rm(starts_with("ema_"))
  } 
  
  # algorithm specific steps
  if (algorithm == "glmnet") {
    if(str_detect(feature_set, "dummy")) {
      rec <- rec  %>%
        step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
        step_zv(all_predictors()) %>% 
        step_normalize(all_predictors())
    }
    if(str_detect(feature_set, "ordinal")) {
      rec <- rec %>% 
        step_ordinalscore(all_nominal_predictors()) %>% 
        step_zv(all_predictors()) %>% 
        step_normalize(all_predictors())
    }
  } 
  
  if (algorithm == "random_forest") {
    # no algorithm specific steps
  } 
  
  if (algorithm == "xgboost") {
    if(str_detect(feature_set, "dummy")) {
      rec <- rec  %>%
        step_dummy(all_nominal_predictors(), one_hot = TRUE)
    }
    if(str_detect(feature_set, "ordinal")) {
      rec <- rec %>% 
        step_ordinalscore(all_nominal_predictors()) 
    }
  } 
  
  if (algorithm == "knn") {
    rec <- rec  %>% 
      step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
      step_zv(all_predictors()) %>% 
      step_normalize(all_predictors())
  }
  
  # resampling options for unbalanced outcome variable
  if (resample == "down") {
    rec <- rec %>% 
      themis::step_downsample(y, under_ratio = ratio, seed = 10) 
  }
  if (resample == "smote") {
    rec <- rec %>% 
      themis::step_smote(y, over_ratio = ratio, seed = 10) 
  }
  if (resample == "up") {
    rec <- rec %>% 
      themis::step_upsample(y, over_ratio = ratio, seed = 10)
  }
  
  # final steps for all algorithms
  rec <- rec %>%
    # drop columns with NA values after imputation (100% NA)
    step_select(where(~ !any(is.na(.)))) %>%
    step_nzv()
  
  return(rec)
}


