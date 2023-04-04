suppressWarnings(suppressPackageStartupMessages({
  require(readr)
  require(dplyr)
  require(tidyr)
  library(purrr)
})) 
source("fun_chtc.R")
source("training_controls.R")


args <- commandArgs(trailingOnly = TRUE) 
job_num <- args[1] # zero-indexed



# Get model and data

(job*100) + 1    - (job+1)*100


model_fit <- readRDS("best_model_fit_all_1week_0_v4.rds")
x <- read_csv("x.csv.xz") %>% 
  slice(((job_num * 100) + 1):((job_num + 1) * 100))

y <- read_csv("y.csv.xz") %>% 
  mutate(y = factor(y, levels = c("yes", "no"))) %>% 
  slice(((job_num * 100) + 1):((job_num + 1) * 100)) %>% 
  pull(y)

predict_wrapper <- function(model, newdata) {
  predict(model, newdata, type = "prob") %>%
    dplyr::select(yes = .pred_yes, no = .pred_no)
}

predictor <- iml::Predictor$new(model = model_fit, 
                           data = x, 
                           y = y,
                           predict.fun = predict_wrapper)  

get_shapley_values <- function(df1, predictor){
  iml::Shapley$new(predictor, x.interest = df1)$results %>%
    separate_wider_delim(feature.value, delim = "=",
                         names = c("tmp", "feature_value")) %>%
    mutate(feature_value = as.numeric(feature_value)) %>%
    select(feature, feature_value, phi)
}
tictoc::tic()
x %>%
  dplyr::mutate(id = row_number()) %>%
  tidyr::nest(.by = id, .key = "dfs") %>%
  dplyr::mutate(shapleys = map(dfs, \(df1) get_shapley_values(df1, predictor))) %>%
  dplyr::select(-dfs) %>%
  tidyr::unnest(shapleys) %>% 
  saveRDS(stringr::str_c("shapleys_", job_num, ".rds"))
tictoc::toc()
