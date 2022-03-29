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

# RANDOM FOREST ----------------
# 285 features, 22,740 observations
grid_penalty <- expand_grid(mtry =  c(5, 10, 20, 50),
                            min_n = c(2, 10, 20))
grid_penalty_extended <- expand_grid(mtry = seq(2, 50, length.out = 10),
                                     min_n = seq(2, 20, length.out = 5))

# downsample ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "random_forest", resample = "down")

# fit model
# fits_rf_down <- readRDS("fits_rf_down.rds")
fits_rf_down <- rand_forest(mtry = tune(),
                            min_n = tune(),
                            trees = 2000) %>%
  set_engine("ranger",
             importance = "impurity",
             respect.unordered.factors = "order",
             oob.error = FALSE,
             seed = 102030) %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_rf_down <- read_csv("results_rf_down.csv", col_types = cols())
results_rf_down <- collect_metrics(fits_rf_down) %>% 
  # summarise across repeats
  group_by(mtry, min_n, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "random_forest",
         resample = "down") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_rf_down %>%
  mutate(min_n = factor(min_n, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = mtry, 
                       y = bal_accuracy, 
                       group = min_n, 
                       color = min_n)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("mtry") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "min n") +
  theme_classic()


# save results
results_rf_down %>% 
  write_csv(., "results_rf_down.csv")

# save model fits
saveRDS(fits_rf_down, "fits_rf_down.rds")

# extended penalty grid
# fit model
# fits_rf_down_extended <- readRDS("fits_rf_down_extended.rds")
fits_rf_down_extended <- rand_forest(mtry = tune(),
                                     min_n = tune(),
                                     trees = 2850) %>%
  set_engine("ranger",
             importance = "impurity",
             respect.unordered.factors = "order",
             oob.error = FALSE,
             seed = 102030) %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty_extended,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_rf_down_extended <- read_csv("results_rf_down_extended.csv", col_types = cols())
results_rf_down_extended <- collect_metrics(fits_rf_down_extended) %>% 
  # summarise across repeats
  group_by(mtry, min_n, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "random_forest",
         resample = "down") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)


# plot hyperparameters
results_rf_down_extended %>%
  mutate(min_n = factor(min_n, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = mtry, 
                       y = bal_accuracy, 
                       group = min_n, 
                       color = min_n)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("mtry") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "min n") +
  theme_classic()


# save results
results_rf_down_extended %>% 
  write_csv(., "results_rf_down_extended.csv")

# save model fits
saveRDS(fits_rf_down_extended, "fits_rf_down_extended.rds")

# none ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "random_forest", resample = "none")

# fit model
# fits_rf_none <- readRDS("fits_rf_none.rds")
fits_rf_none <- rand_forest(mtry = tune(),
                            min_n = tune(),
                            trees = 2000) %>%
  set_engine("ranger",
             importance = "impurity",
             respect.unordered.factors = "order",
             oob.error = FALSE,
             seed = 102030) %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_rf_none <- read_csv("results_rf_none.csv", col_types = cols())
results_rf_none <- collect_metrics(fits_rf_none) %>% 
  # summarise across repeats
  group_by(mtry, min_n, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "random_forest",
         resample = "none") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_rf_none %>%
  mutate(min_n = factor(min_n, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = mtry, 
                       y = bal_accuracy, 
                       group = min_n, 
                       color = min_n)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("mtry") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "min n") +
  theme_classic()


# save results
results_rf_none %>% 
  write_csv(., "results_rf_none.csv")

# save model fits
saveRDS(fits_rf_none, "fits_rf_none.rds")

# extended penalty grid
# fit model
# fits_rf_none_extended <- readRDS("fits_rf_none_extended.rds")
fits_rf_none_extended <- rand_forest(mtry = tune(),
                                     min_n = tune(),
                                     trees = 2850) %>%
  set_engine("ranger",
             importance = "impurity",
             respect.unordered.factors = "order",
             oob.error = FALSE,
             seed = 102030) %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty_extended,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_rf_none_extended <- read_csv("results_rf_none_extended.csv", col_types = cols())
results_rf_none_extended <- collect_metrics(fits_rf_none_extended) %>% 
  # summarise across repeats
  group_by(mtry, min_n, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "random_forest",
         resample = "none") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_rf_none_extended %>%
  mutate(min_n = factor(min_n, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = mtry, 
                       y = bal_accuracy, 
                       group = min_n, 
                       color = min_n)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("mtry") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "min n") +
  theme_classic()


# save results
results_rf_none_extended %>% 
  write_csv(., "results_rf_none_extended.csv")

# save model fits
saveRDS(fits_rf_none_extended, "fits_rf_none_extended.rds")

# up ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "random_forest", resample = "up")

# fit model
# fits_rf_up <- readRDS("fits_rf_up.rds")
fits_rf_up <- rand_forest(mtry = tune(),
                            min_n = tune(),
                            trees = 2000) %>%
  set_engine("ranger",
             importance = "impurity",
             respect.unordered.factors = "order",
             oob.error = FALSE,
             seed = 102030) %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_rf_up <- read_csv("results_rf_up.csv", col_types = cols())
results_rf_up <- collect_metrics(fits_rf_up) %>% 
  # summarise across repeats
  group_by(mtry, min_n, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "random_forest",
         resample = "up") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_rf_up %>%
  mutate(min_n = factor(min_n, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = mtry, 
                       y = bal_accuracy, 
                       group = min_n, 
                       color = min_n)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("mtry") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "min n") +
  theme_classic()


# save results
results_rf_up %>% 
  write_csv(., "results_rf_up.csv")

# save model fits
saveRDS(fits_rf_up, "fits_rf_up.rds")

# extended penalty grid
# fit model
# fits_rf_up_extended <- readRDS("fits_rf_up_extended.rds")
fits_rf_up_extended <- rand_forest(mtry = tune(),
                                     min_n = tune(),
                                     trees = 2850) %>%
  set_engine("ranger",
             importance = "impurity",
             respect.unordered.factors = "order",
             oob.error = FALSE,
             seed = 102030) %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty_extended,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_rf_up_extended <- read_csv("results_rf_up_extended.csv", col_types = cols())
results_rf_up_extended <- collect_metrics(fits_rf_up_extended) %>% 
  # summarise across repeats
  group_by(mtry, min_n, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "random_forest",
         resample = "up") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_rf_up_extended %>%
  mutate(min_n = factor(min_n, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = mtry, 
                       y = bal_accuracy, 
                       group = min_n, 
                       color = min_n)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("mtry") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "min n") +
  theme_classic()


# save results
results_rf_up_extended %>% 
  write_csv(., "results_rf_up_extended.csv")

# save model fits
saveRDS(fits_rf_up_extended, "fits_rf_up_extended.rds")

# smote ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "random_forest", resample = "smote")

# fit model
# fits_rf_smote <- readRDS("fits_rf_smote.rds")
fits_rf_smote <- rand_forest(mtry = tune(),
                            min_n = tune(),
                            trees = 2000) %>%
  set_engine("ranger",
             importance = "impurity",
             respect.unordered.factors = "order",
             oob.error = FALSE,
             seed = 102030) %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_rf_smote <- read_csv("results_rf_smote.csv", col_types = cols())
results_rf_smote <- collect_metrics(fits_rf_smote) %>% 
  # summarise across repeats
  group_by(mtry, min_n, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "random_forest",
         resample = "smote") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_rf_smote %>%
  mutate(min_n = factor(min_n, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = mtry, 
                       y = bal_accuracy, 
                       group = min_n, 
                       color = min_n)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("mtry") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "min n") +
  theme_classic()


# save results
results_rf_smote %>% 
  write_csv(., "results_rf_smote.csv")

# save model fits
saveRDS(fits_rf_smote, "fits_rf_smote.rds")

# extended penalty grid
# fit model
# fits_rf_smote_extended <- readRDS("fits_rf_smote_extended.rds")
fits_rf_smote_extended <- rand_forest(mtry = tune(),
                                     min_n = tune(),
                                     trees = 2850) %>%
  set_engine("ranger",
             importance = "impurity",
             respect.unordered.factors = "order",
             oob.error = FALSE,
             seed = 102030) %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty_extended,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_rf_smote_extended <- read_csv("results_rf_smote_extended.csv", col_types = cols())
results_rf_smote_extended <- collect_metrics(fits_rf_smote_extended) %>% 
  # summarise across repeats
  group_by(mtry, min_n, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "random_forest",
         resample = "smote") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_rf_smote_extended %>%
  mutate(min_n = factor(min_n, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = mtry, 
                       y = bal_accuracy, 
                       group = min_n, 
                       color = min_n)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("mtry") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "min n") +
  theme_classic()


# save results
results_rf_smote_extended %>% 
  write_csv(., "results_rf_smote_extended.csv")

# save model fits
saveRDS(fits_rf_smote_extended, "fits_rf_smote_extended.rds")


# reproduce ggplot ---------------- 
# original hyperparameters
results_rf_down %>% 
  bind_rows(results_rf_none, results_rf_smote, results_rf_up) %>%
  mutate(min_n = factor(min_n, ordered = TRUE)) %>%
  ggplot(mapping = aes(x = mtry, 
                       y = bal_accuracy, 
                       group = min_n, 
                       color = min_n)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("mtry") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "min n") +
  theme_classic()

# extended range of hyperparameters
results_rf_down_extended %>% 
  bind_rows(results_rf_none_extended, results_rf_smote_extended, results_rf_up_extended) %>%
  mutate(min_n = factor(min_n, ordered = TRUE)) %>%
  ggplot(mapping = aes(x = mtry, 
                       y = bal_accuracy, 
                       group = min_n, 
                       color = min_n)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("mtry") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "min n") +
  theme_classic()

