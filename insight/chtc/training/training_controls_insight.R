# Training controls for Insight study

# SET GLOBAL PARAMETERS--------------------
study <- "insight"
version <- "v2"
algorithm <- "xgboost" # keep as "xgboost"
batch <- "batch2"
window <- "1week"
lead <- 0

configs_per_job <- 200  # number of model configurations that will be fit/evaluated within each CHTC

# RESAMPLING FOR OUTCOME-----------------------------------
# note that ratio is under_ratio, which is used by downsampling as is
# It is converted to  overratio (1/ratio) for up and smote

if (algorithm == "random_forest") {
  resample <- c("none", "up_1", "down_1", "up_2", "down_2") 
} else {
  resample <- c("none", "up_1", "down_1", "smote_1",
                "up_2", "down_2", "smote_2") 
}


# DATA, SPLITS AND OUTCOME-------------------------------------
feature_set <- c("all") # all # insight_only # aase_only
 
if (window == "dichotomous") {
  data_trn <- "aase_dich_v1.csv"
} else {
  data_trn <- "features_v1.csv"
}
 
seed_splits <- 102030

ml_mode <- "classification"   # regression or classification
y_col_name <- "lapse" 
y_level_pos <- "yes" 
y_level_neg <- "no"


# CV SETTINGS---------------------------------
cv_resample_type <- "nested" # can be boot, kfold, or nested
cv_resample = NULL # can be repeats_x_folds (e.g., 1_x_10, 10_x_10) or number of bootstraps (e.g., 100)
cv_inner_resample <- "1_x_10" # can also be a single number for bootstrapping (i.e., 100)
cv_outer_resample <- "3_x_10" # outer resample will always be kfold
if (window == "dichotomous") {
  cv_group <- NULL
} else {
  cv_group <- "subid"
}

cv_name <- if_else(cv_resample_type == "nested",
                   str_c(cv_resample_type, "_", cv_inner_resample, "_",
                         cv_outer_resample),
                   str_c(cv_resample_type, "_", cv_resample))

# STUDY PATHS----------------------------
# the name of the batch of jobs to set folder name
name_batch <- str_c("train_", algorithm, "_", window, "_", 
                    cv_name, "_", version, "_", batch) 
# the path to the batch of jobs
path_batch <- str_c("studydata/risk/chtc/", study, "/", name_batch) 
# location of data set
path_data <- str_c("studydata/risk/data_processed/", study) 


# ALGORITHM-SPECIFIC HYPERPARAMETERS-----------
hp1_glmnet <- c(0.05, seq(.1, 1, length.out = 10)) # alpha (mixture)
hp2_glmnet_min <- -8 # min for penalty grid - will be passed into exp(seq(min, max, length.out = out))
hp2_glmnet_max <- 2 # max for penalty grid
hp2_glmnet_out <- 200 # length of penalty grid

hp1_knn <- seq(5, 255, length.out = 26) # neighbors (must be integer)

if (str_detect(feature_set, "only")) {
  hp1_rf <- 1 # mtry
} else {
  hp1_rf <- c(2, 10, 20, 30, 40) # mtry (p/3 for reg or square root of p for class)
}
hp2_rf <- c(2, 15, 30, 50, 75, 100) # min_n
hp3_rf <- c(10, 100, 500, 1000) # trees (10 x's number of predictors)

if (feature_set == "all" | window == "dichotomous") {
  hp1_xgboost <- c(0.00001, 0.00005, 0.0001, 0.001, 0.01, 0.1, 0.2)  # learn_rate
} else {
  hp1_xgboost <- c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.3, 0.4) # learn_rate
}

# manual override
# hp1_xgboost <- c(.6, .8, .95)

hp2_xgboost <- c(1, 2, 3, 4) # tree_depth
if (str_detect(feature_set, "only")) {
  hp3_xgboost <- 1 # mtry
} else {
  hp3_xgboost <- c(20, 30, 40, 50)  # mtry
}
# trees = 500
# early stopping = 20
 

# CHTC SPECIFIC CONTROLS----------------------------
max_idle <- 1000
request_cpus <- 1 
request_memory <- "5000MB"
request_disk <- "1000MB"
flock <- TRUE
glide <- TRUE

# Batches
# batch1: 
# insight_only feature_set w corresponding reduced hp values

# batch2: all feature_set w corresponding hp values (full set)
# request_memory <- "5000MB" request_disk <- "1000MB"
# 300 configs per job (random_forest)
# went down to 200 configs per job (xgb, glmnet) based on RF timing

# batch3: aase -> 1 week labels

# FORMAT DATA-----------------------------------------
format_data <- function (df) {
  
  df %>% 
    rename(y = !!y_col_name) %>% 
    mutate(y = factor(y, levels = c(!!y_level_pos, !!y_level_neg))) %>%  # set pos class first
    select(-label_num, -dttm_label) %>% 
    mutate(label_day = factor(label_day, levels = c("Mon", "Tue", "Wed", 
                                                    "Thu", "Fri", "Sat", 
                                                    "Sun")),
           label_hour = factor(label_hour, levels = c("other", "evening")),
           demo_sex = factor(demo_sex, levels = c("Male", "Female")),
           demo_educ = factor(demo_educ, levels = c("High school or less",
                                                    "Some college",
                                                    "College or more")),
           demo_marital = factor(demo_marital, levels = c("Married",
                                                          "Never Married",
                                                          "Other")),
           demo_race = factor(demo_race, levels = c("White/Caucasian",
                                                    "Other")))

}


# BUILD RECIPE---------------------------------------
# Script should have a single build_recipe function to be compatible with fit script. 
build_recipe <- function(d, config) {
  # d: (training) dataset from which to build recipe
  # config: single-row config-specific tibble
  
  # get relevant info from job (algorithm, feature_set, resample, under_ratio)
  algorithm <- config$algorithm
  feature_set <- config$feature_set
  
  if (config$resample == "none") {
    resample <- config$resample
  } else {
    resample <- str_split(config$resample, "_")[[1]][1]
    ratio <- as.numeric(str_split(config$resample, "_")[[1]][2])
  }
  
  # Set recipe steps generalizable to all model configurations
  rec <- recipe(y ~ ., data = d) %>%
    step_rm(subid) %>%
    step_zv(all_predictors()) 
  
  if (feature_set == "insight_only") {
    rec <- rec %>% 
      step_select(y, ema_10.p0.l0.rrecent_response)
  }
  
  if (feature_set == "aase_only") {
    rec <- rec %>% 
      step_select(y, aase_total)
  }
  
  if (feature_set == "all") {
    rec <- rec %>% 
      step_rm(aase_total, starts_with("demo"), label_day, label_hour)
  }
  
  # algorithm specific steps
  if (algorithm == "glmnet") {
      rec <- rec  %>%
        step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
        step_zv(all_predictors()) %>% 
        step_normalize(all_predictors())
  }
  
  if (algorithm == "glm") {
    # no algorithm specific steps
  }
  
  if (algorithm == "random_forest") {
    # no algorithm specific steps
  } 
  
  if (algorithm == "xgboost") {
    rec <- rec  %>% 
      step_dummy(all_nominal_predictors(), one_hot = TRUE)
  } 
  
  # resampling options for unbalanced outcome variable
  if (resample == "down") {
    # under_ratio = ratio.  No conversion needed
    rec <- rec %>% 
      themis::step_downsample(y, under_ratio = ratio, seed = 10) 
  }
  
  if (resample == "smote") {
    # correct ratio to over_ratio
    rec <- rec %>% 
      themis::step_smote(y, over_ratio = 1 / ratio, seed = 10) 
  }
  
  if (resample == "up") {
    # correct ratio to over_ratio
    rec <- rec %>% 
      themis::step_upsample(y, over_ratio = 1 / ratio, seed = 10)
  }
  
  # final steps for all algorithms
  rec <- rec %>%
    # drop columns with NA values after imputation (100% NA)
    step_select(where(~ !any(is.na(.)))) %>%
    step_nzv()
  
  return(rec)
}

# Update paths for OS--------------------------------
# This does NOT need to be edited.  This will work for Windows, Mac and Linux OSs
path_batch <- case_when(Sys.info()[["sysname"]] == "Windows" ~str_c("P:/", path_batch),
                        Sys.info()[["sysname"]] == "Linux" ~str_c("~/mnt/private/", path_batch),
                        .default = str_c("/Volumes/private/", path_batch))

path_data <- case_when(Sys.info()[["sysname"]] == "Windows" ~str_c("P:/", path_data),
                       Sys.info()[["sysname"]] == "Linux" ~str_c("~/mnt/private/", path_data),
                       .default = str_c("/Volumes/private/", path_data))