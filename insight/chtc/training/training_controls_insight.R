# This is a study level script for defining the recipe and global attributes that are 
# the same across jobs.

#EDIT THIS
study <- "ema"
data_type <- "all"   # but still need to change more (e.g., feature set) to switch data_type
window <- "1hour"
lead <- 0
version <- "v5"
algorithm <- "glmnet"


# SET GLOBAL PARAMETERS
feature_set <- c("all") # EMA Features set names
data_trn <- str_c("features_", data_type, "_", window, "_", lead, "_", version, ".csv.xz") 
resample <- c("up_1", "down_1") 
y_col_name <- "lapse" # outcome variable - will be changed to y in recipe for consistency across studies 
cv_type <- "group_kfold_1_x_10" # cv type - can be boot, group_kfold, or kfold
group <- "subid" # grouping variable for grouped k-fold - remove if not using group_kfold
y_level_pos <- "yes" # character string of the outcome variable's positive level (e.g., "yes", "abstinent")
y_level_neg <- "no" # character string of the outcome variable's negative level (e.g., "no", "smoking")

remove_nzv <- TRUE # using as variable instead of in recipe to be able to calculate features before removing nzv

cv_resample_type <- "kfold" # can be boot, kfold, or nested
cv_resample = "1_x_10" # can be repeats_x_folds (e.g., 1_x_10, 10_x_10) or number of bootstraps (e.g., 100)
cv_inner_resample <- NULL # can also be a single number for bootstrapping (i.e., 100)
cv_outer_resample <- NULL # outer resample will always be kfold
cv_group <- "subid" # set to NULL if not grouping

cv_name <- if_else(cv_resample_type == "nested",
                   str_c(cv_resample_type, "_", cv_inner_resample, "_",
                         cv_outer_resample),
                   str_c(cv_resample_type, "_", cv_resample))

# SET STUDY PATHS
name_job <- str_c("train_", version, "_", algorithm, "_", cv_name) # the name of the job to set folder names
path_jobs <- str_c("P:/studydata/risk/chtc/", study) # location of where you want your jobs to be setup
path_data <- str_c("P:/studydata/risk/data_processed/", study) # location of data set

# SET ALGORITHM-SPECIFIC HYPERPARAMETERS
hp1_glmnet <- c(0.05, seq(.1, 1, length.out = 10)) # alpha (mixture)
hp2_glmnet_min <- -8 # min for penalty grid - will be passed into exp(seq(min, max, length.out = out))
hp2_glmnet_max <- 2 # max for penalty grid
hp2_glmnet_out <- 200 # length of penalty grid

hp1_knn <- seq(5, 255, length.out = 26) # neighbors (must be integer)

hp1_rf <- c(2, 10, 20, 30, 40) # mtry (p/3 for reg or square root of p for class)
hp2_rf <- c(2, 15, 30) # min_n
hp3_rf <- 1500 # trees (10 x's number of predictors)

hp1_xgboost <- c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.3, .4)  # learn_rate
hp2_xgboost <- c(1, 2, 3, 4) # tree_depth
hp3_xgboost <- c(20, 30, 40, 50)  # mtry (previously included 2 and 10 but not needed)
# trees = 100
# early stopping = 10
 

# SET CHTC SPECIFIC CONTROLS
tar <- c("train.tar.gz") # name of tar packages for submit file - does not transfer these anywhere 
max_idle <- 1000
request_cpus <- 1 
request_memory <- "16000MB"
request_disk <- "1000MB"
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
    step_rm(subid, dttm_label, label_num) %>% 
    # MUST CHANGE DICHOTMOUS OUTCOME TO POS/NEG VALUES
    step_mutate(y = if_else(y == !!y_level_pos, "pos", "neg")) %>% 
    step_string2factor(y, levels = c("pos", "neg")) %>% # positive case should be first
    step_relevel(y, ref_level = "pos") %>% # confirming positive case is set as first level
    step_string2factor(all_nominal()) %>% 
    step_zv(all_predictors()) %>% 
    step_impute_median(all_numeric()) %>% 
    step_impute_mode(all_nominal(),  -y) 
  
  # # If statements for filtering features based on EMA feature set
  # # no removals if set = "all"
  # if (feature_set == "raw") {
  #   rec <- rec   %>% 
  #     step_rm(contains("dratecount")) %>% 
  #     step_rm(contains("pratecount")) %>% 
  #     step_rm(contains("dmedian")) %>% 
  #     step_rm(contains("pmedian")) %>% 
  #     step_rm(contains("dmax")) %>% 
  #     step_rm(contains("pmax")) %>% 
  #     step_rm(contains("dmin")) %>% 
  #     step_rm(contains("pmin"))
  # }
  # if (feature_set == "diff") {
  #   rec <- rec   %>% 
  #     step_rm(contains("rratecount")) %>% 
  #     step_rm(contains("pratecount")) %>% 
  #     step_rm(contains("rmedian")) %>% 
  #     step_rm(contains("pmedian")) %>% 
  #     step_rm(contains("rmax")) %>% 
  #     step_rm(contains("pmax")) %>% 
  #     step_rm(contains("rmin")) %>% 
  #     step_rm(contains("pmin"))
  # }
  # if (feature_set == "prop") {
  #   rec <- rec   %>% 
  #     step_rm(contains("dratecount")) %>% 
  #     step_rm(contains("rratecount")) %>% 
  #     step_rm(contains("dmedian")) %>% 
  #     step_rm(contains("rmedian")) %>% 
  #     step_rm(contains("dmax")) %>% 
  #     step_rm(contains("rmax")) %>% 
  #     step_rm(contains("dmin")) %>% 
  #     step_rm(contains("rmin"))
  # }
  # if (feature_set == "r_d") {
  #   rec <- rec   %>% 
  #     step_rm(contains("pratecount")) %>% 
  #     step_rm(contains("pmedian")) %>% 
  #     step_rm(contains("pmax")) %>% 
  #     step_rm(contains("pmin"))
  # }
  # if (feature_set == "r_p") {
  #   rec <- rec   %>% 
  #     step_rm(contains("dratecount")) %>% 
  #     step_rm(contains("dmedian")) %>% 
  #     step_rm(contains("dmax")) %>% 
  #     step_rm(contains("dmin")) 
  # }
  # if (feature_set == "d_p") {
  #   rec <- rec   %>% 
  #     step_rm(contains("rratecount")) %>% 
  #     step_rm(contains("rmedian")) %>% 
  #     step_rm(contains("rmax")) %>% 
  #     step_rm(contains("rmin")) 
  # }
    
  
  # resampling options for unbalanced outcome variable
  if (resample == "down") {
    rec <- rec %>% 
      themis::step_downsample(y, under_ratio = under_ratio, seed = 10) 
  }
  
  if (resample == "smote") {
    if (under_ratio != 1) { over_ratio <- under_ratio / (under_ratio + 1)
    } else over_ratio <- under_ratio
    
    stop("SMOTE not currently implemented")
    # rec <- rec %>% 
    #   themis::step_smotenc(y, over_ratio = over_ratio, seed = 10) 
  }
  
  if (resample == "up") {
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
  
  if (algorithm == "random_forest") {
    rec <- rec  %>% 
      # drop columns with NA values after imputation (100% NA)
      step_select(where(~ !any(is.na(.))))
  } 
  
  if (algorithm == "xgboost") {
    rec <- rec  %>% 
      step_dummy(all_nominal(), -y) %>% 
      # drop columns with NA values after imputation (100% NA)
      step_select(where(~ !any(is.na(.))))
  } 
  
  
  return(rec)
}