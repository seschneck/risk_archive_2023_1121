# This is a study level script for defining the recipe and global attributes that are 
# the same across jobs.

# SET GLOBAL PARAMETERS --------
data_trn <- "features_1week.csv.xz" 
feature_set <- c("raw", "diff", "prop", "r_d", "r_p")
algorithm <- c("random_forest") 
resample <- c("up_1", "down_1") 
y_col_name <- "lapse" # outcome variable - will be changed to y in recipe for consistency across studies 
cv_type <- "group_kfold_1_x_10" # cv type - can be boot, group_kfold, or kfold
group <- "subid" # grouping variable for grouped k-fold - remove if not using group_kfold
remove_nzv <- TRUE # using as variable instead of in recipe to be able to calculate features before removing nzv

# CHANGE ALGORITHM-SPECIFIC HYPERPARAMETERS -------------------
hp1_rf <- c(5, 10, 20, 50) # mtry (p/3 for reg or square root of p for class)
hp2_rf <- c(2, 10, 20) # min_n
hp3_rf <- 1000 # trees (10 x's number of predictors)   UPDATE after CBIT

# CHANGE STUDY PATHS -------------------- 
name_job <- "train_1week_rf" # the name of the job to set folder names
path_jobs <- "P:/studydata/risk/chtc/ema" # location of where you want your jobs to be setup
path_data <- "P:/studydata/risk/data_processed/ema" # location of data set

# CHANGE CHTC SPECIFIC CONTROLS
tar <- c("train.tar.gz") # name of tar packages for submit file - does not transfer these anywhere 
max_idle <- 1000
request_cpus <- 1 
request_memory <- "16000MB"
request_disk <- "1500000KB"
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
    update_role(subid, dttm_label, new_role = "id variable") %>%
    step_rm(label_num) %>% 
    step_string2factor(y, levels = c("yes", "no")) %>% 
    # event level will be first level in factor
    step_string2factor(all_nominal()) %>% 
    step_zv(all_predictors()) %>% 
    step_impute_median(all_numeric()) %>% 
    step_impute_mode(all_nominal(),  -y) 
  
  # If statements for filtering features based on feature set
  if (feature_set == "raw") {
    rec <- rec   %>% 
      step_rm(contains("dratecount")) %>% 
      step_rm(contains("pratecount")) %>% 
      step_rm(contains("dmedian")) %>% 
      step_rm(contains("pmedian")) %>% 
      step_rm(contains("dmax")) %>% 
      step_rm(contains("pmax")) %>% 
      step_rm(contains("dmin")) %>% 
      step_rm(contains("pmin"))
  }
  if (feature_set == "diff") {
    rec <- rec   %>% 
      step_rm(contains("rratecount")) %>% 
      step_rm(contains("pratecount")) %>% 
      step_rm(contains("rmedian")) %>% 
      step_rm(contains("pmedian")) %>% 
      step_rm(contains("rmax")) %>% 
      step_rm(contains("pmax")) %>% 
      step_rm(contains("rmin")) %>% 
      step_rm(contains("pmin"))
  }
  if (feature_set == "prop") {
    rec <- rec   %>% 
      step_rm(contains("dratecount")) %>% 
      step_rm(contains("rratecount")) %>% 
      step_rm(contains("dmedian")) %>% 
      step_rm(contains("rmedian")) %>% 
      step_rm(contains("dmax")) %>% 
      step_rm(contains("rmax")) %>% 
      step_rm(contains("dmin")) %>% 
      step_rm(contains("rmin"))
  }
  if (feature_set == "r_d") {
    rec <- rec   %>% 
      step_rm(contains("pratecount")) %>% 
      step_rm(contains("pmedian")) %>% 
      step_rm(contains("pmax")) %>% 
      step_rm(contains("pmin"))
  }
  if (feature_set == "r_p") {
    rec <- rec   %>% 
      step_rm(contains("dratecount")) %>% 
      step_rm(contains("dmedian")) %>% 
      step_rm(contains("dmax")) %>% 
      step_rm(contains("dmin")) 
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
  

  
  # no additional steps for rf
  
  return(rec)
}


