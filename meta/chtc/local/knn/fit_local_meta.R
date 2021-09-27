# fit model locally with grouped k-fold cv 

# libraries & source functions file ----------------
suppressMessages(suppressWarnings({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(doParallel)
  library(ggplot2)
  
  source("../fun_local_meta.R")
}))

# set up parallel processing ----------------
n_core <- detectCores(logical = FALSE)
cl <- makePSOCKcluster(n_core - 1)
registerDoParallel(cl)

# read in data train --------------- 
d <- read_csv("../data_trn.csv", col_types = cols())

# create splits object ---------------
set.seed(102030)
folds <- make_splits(d = d, cv_type = "group_kfold_1_x_10")


# GLMNET ----------------
grid_penalty <- expand_grid(neighbors = seq(5, 75, length.out = 15))


# downsample ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "knn", resample = "down")

# fit model
# fits_knn_down <- readRDS("fits_knn_down.rds")
fits_knn_down <- nearest_neighbor(neighbors = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")  %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_knn_down <- read_csv("results_knn_down.csv", col_types = cols())
results_knn_down <- collect_metrics(fits_knn_down) %>% 
  # summarise across repeats
  group_by(neighbors, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "knn",
         resample = "down") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_knn_down %>%
  ggplot(mapping = aes(x = neighbors, 
                       y = bal_accuracy)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("neighbors") +
  ylab("balanced accuracy") +
  theme_classic()

# save results
results_knn_down %>% 
  write_csv(., "results_knn_down.csv")

# save model fits
saveRDS(fits_knn_down, "fits_knn_down.rds")

# no resample ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "knn", resample = "none")

# fit model
# fits_knn_none <- readRDS("fits_knn_none.rds")
fits_knn_none <- nearest_neighbor(neighbors = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")  %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_knn_none <- read_csv("results_knn_none.csv", col_types = cols())
results_knn_none <- collect_metrics(fits_knn_none) %>% 
  # summarise across repeats
  group_by(neighbors, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "knn",
         resample = "none") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_knn_none %>%
  ggplot(mapping = aes(x = neighbors, 
                       y = bal_accuracy)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("neighbors") +
  ylab("balanced accuracy") +
  theme_classic()

# save results
results_knn_none %>% 
  write_csv(., "results_knn_none.csv")

# save model fits
saveRDS(fits_knn_none, "fits_knn_none.rds")

# smote ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "knn", resample = "smote")

# fit model
# fits_knn_smote <- readRDS("fits_knn_smote.rds")
fits_knn_smote <- nearest_neighbor(neighbors = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")  %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_knn_smote <- read_csv("results_knn_smote.csv", col_types = cols())
results_knn_smote <- collect_metrics(fits_knn_smote) %>% 
  # summarise across repeats
  group_by(neighbors, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "knn",
         resample = "smote") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_knn_smote %>%
  ggplot(mapping = aes(x = neighbors, 
                       y = bal_accuracy)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("neighbors") +
  ylab("balanced accuracy") +
  theme_classic()

# save results
results_knn_smote %>% 
  write_csv(., "results_knn_smote.csv")

# save model fits
saveRDS(fits_knn_smote, "fits_knn_smote.rds")

# upsample ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "knn", resample = "up")

# fit model
# fits_knn_up <- readRDS("fits_knn_up.rds")
fits_knn_up <- nearest_neighbor(neighbors = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")  %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_knn_up <- read_csv("results_knn_up.csv", col_types = cols())
results_knn_up <- collect_metrics(fits_knn_up) %>% 
  # summarise across repeats
  group_by(neighbors, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "knn",
         resample = "up") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_knn_up %>%
  ggplot(mapping = aes(x = neighbors, 
                       y = bal_accuracy)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("neighbors") +
  ylab("balanced accuracy") +
  theme_classic()

# save results
results_knn_up %>% 
  write_csv(., "results_knn_up.csv")

# save model fits
saveRDS(fits_knn_up, "fits_knn_up.rds")

# reproduce ggplot ---------------- 
results_knn_down %>% 
  bind_rows(results_knn_none, results_knn_smote, results_knn_up) %>%
  ggplot(mapping = aes(x = neighbors, 
                       y = bal_accuracy)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("neighbors") +
  ylab("balanced accuracy") +
  theme_classic()

