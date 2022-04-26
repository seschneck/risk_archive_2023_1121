# This is a study level script for defining the recipe and global attributes that are 
# the same across jobs.

# SET GLOBAL PARAMETERS --------
data_trn <- "features_1day_0_v2.rds.xz" # can be csv or rds file
feature_set <- c("feat_all", "feat_all_passive", "feat_logs") # 1+ feature sets
feature_fun_type <- c("raw", "diff", "r_d")
algorithm <- c("glmnet") # 1+ algorithm (glmnet, random_forest) 
resample <- c("up_1", "down_1") # 1+ resampling methods (up, down, smote, or none)
y_col_name <- "label" # outcome variable - will be changed to y in recipe for consistency across studies 
cv_type <- "group_kfold_1_x_10" # cv type - can be boot, group_kfold, or kfold
group <- "subid" # grouping variable for grouped k-fold - remove if not using group_kfold
remove_nzv <- TRUE # using as variable instead of in recipe to be able to calculate number of features before removing nzv

# CHANGE ALGORITHM-SPECIFIC HYPERPARAMETERS -------------------
hp1_glmnet <- c(0.05, seq(.1, 1, length.out = 11)) # alpha (mixture) 
hp2_glmnet_min <- -8 # min for penalty grid - will be passed into exp(seq(min, max, length.out = out))
hp2_glmnet_max <- 2 # max for penalty grid
hp2_glmnet_out <- 100 # length of penalty grid


# CHANGE STUDY PATHS -------------------- 
name_job <- "train_1day_0_v2_glmnet" # the name of the job to set folder names
path_jobs <- "P:/studydata/risk/chtc/meta" # location of where you want your jobs to be setup
path_data <- "P:/studydata/risk/data_processed/meta" # location of data set

# CHANGE CHTC SPECIFIC CONTROLS
tar <- c("train.tar.gz") # name of tar packages for submit file - does not transfer these anywhere 
max_idle <- 1000 # according to CHTC we should set this at 1000 to not flood the server. It will not limit the number of jobs running at one time 
request_cpus <- 1 
request_memory <- "10000MB"
request_disk <- "1500000KB" # this is pretty large - necessary for meta
flock <- FALSE
glide <- FALSE


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
    under_ratio <- as.numeric(str_split(job$resample, "_")[[1]][2])
  }
  
  # Set recipe steps generalizable to all model configurations
  rec <- recipe(y ~ ., data = d) %>%
    step_rm(label_num, subid, dttm_label) %>% 
    step_string2factor(y, levels = c("no", "yes")) %>% 
    # reference group will be first level in factor - specify levels to choose reference group
    step_string2factor(label_weekday, levels = c("Mon", "Tues", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
    step_num2factor(label_hour, levels = c("4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
                                           "14", "15", "16", "17", "18", "19", "20", "21", "22",
                                           "23", "24", "1", "2", "3")) %>%
    step_string2factor(label_season, levels = c("Spring", "Summer", "Fall", "Winter")) %>% 
    step_string2factor(all_nominal()) %>% 
    step_zv(all_predictors()) %>% 
    step_impute_median(all_numeric()) %>% 
    step_impute_mode(all_nominal(),  -y) 
  
  # If statements for filtering features based on feature set
  if (feature_set == "feat_all_passive") {
    rec <- rec %>%
      step_rm(starts_with("sms"), -contains("passive")) %>% 
      step_rm(starts_with("voi"), -contains("passive"))
  } else if (feature_set == "feat_logs") {
    rec <- rec %>% 
      step_rm(starts_with("id")) %>% 
      step_rm(starts_with("label"))
  }
  
  # If statements for filtering features based on EMA feature set
  if (feature_fun_type == "raw") {
    rec <- rec   %>% 
      step_rm(contains("dratecount")) %>% 
      step_rm(contains("dpropcount")) %>% 
      step_rm(contains("dpropdatetime")) %>% 
      step_rm(contains("dmean")) %>% 
      step_rm(contains("dratesum")) 
  }
  if (feature_fun_type == "diff") {
    rec <- rec   %>% 
      step_rm(contains("rratecount")) %>% 
      step_rm(contains("rpropcount")) %>% 
      step_rm(contains("rpropdatetime")) %>% 
      step_rm(contains("rmean")) %>% 
      step_rm(contains("rratesum")) 
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
      step_normalize(all_predictors()) %>% 
      # drop columns with NA values after imputation (100% NA)
      step_select(where(~ !any(is.na(.))))
      # step nzv done within fit if training controls remove_nzv is set to TRUE
  } 
  
  if (algorithm == "knn") {
    rec <- rec  %>% 
      step_dummy(all_nominal(), -y) %>% 
      step_normalize(all_predictors()) %>% 
      # drop columns with NA values after imputation (100% NA)
      step_select(where(~ !any(is.na(.))))
  } 
  
  return(rec)
}


