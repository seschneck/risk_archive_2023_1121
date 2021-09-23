# fit model locally with grouped k-fold cv 

# libraries & source functions file ----------------
suppressMessages(suppressWarnings({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(doParallel)
  library(ggplot2)
  
  source("fun_local_meta.R")
}))

# set up parallel processing ----------------
n_core <- detectCores(logical = FALSE)
cl <- makePSOCKcluster(n_core - 1)
registerDoParallel(cl)

# read in data train --------------- 
d <- read_csv("data_trn.csv", col_types = cols())

# create splits object ---------------
set.seed(102030)
folds <- make_folds(d = d, cv_type = "1_x_10")


# GLMNET ----------------
grid_penalty <- expand_grid(penalty = exp(seq(-5, 5, length.out = 100)),
                            mixture = c(0, .05, .1, .2, .3, .4, .5, .6, .7,
                                        .8, .9, 1))


# downsample ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "glmnet", resample = "down")

# fit model
# fits_glmnet_down <- readRDS("fits_glmnet_down.rds")
fits_glmnet_down <- logistic_reg(penalty = tune(),
                                 mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_glmnet_down <- read_csv("results_glmnet_down.csv", col_types = cols())
results_glmnet_down <- collect_metrics(fits_glmnet_down) %>% 
  # summarise across repeats
  group_by(penalty, mixture, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            std_err = mean(std_err),
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "glmnet",
         resample = "down") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config, std_err)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_glmnet_down %>%
  mutate(mixture = factor(mixture, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = log(penalty), 
                       y = bal_accuracy, 
                       group = mixture, 
                       color = mixture)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("penalty (lambda)") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "mixture (alpha)") +
  theme_classic()

# view values in plot for single mixture
results_glmnet_down %>% 
  filter(mixture == 1) %>% 
  mutate(penalty = log(penalty)) %>% 
  arrange(penalty) %>% 
  select(penalty, bal_accuracy) %>% 
  print(n = Inf)

# save results
results_glmnet %>% 
  write_csv(., "results_glmnet_down.csv")

# save model fits
saveRDS(fits_glmnet, "fits_glmnet_down.rds")

# no resample ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "glmnet", resample = "none")

# fit model
# fits_glmnet_none <- readRDS("fits_glmnet_none.rds")
fits_glmnet_none <- logistic_reg(penalty = tune(),
                                 mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_glmnet_none <- read_csv("results_glmnet_none.csv", col_types = cols())
results_glmnet_none <- collect_metrics(fits_glmnet_none) %>% 
  # summarise across repeats
  group_by(penalty, mixture, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            std_err = mean(std_err),
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "glmnet",
         resample = "none") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config, std_err)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_glmnet_none %>%
  mutate(mixture = factor(mixture, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = log(penalty), 
                       y = bal_accuracy, 
                       group = mixture, 
                       color = mixture)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("penalty (lambda)") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "mixture (alpha)") +
  theme_classic()

# save results
results_glmnet_none %>% 
  write_csv(., "results_glmnet_none.csv")

# save model fits
saveRDS(fits_glmnet_none, "fits_glmnet_none.rds")

# smote ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "glmnet", resample = "smote")

# fit model
# fits_glmnet_smote <- readRDS("fits_glmnet_smote.rds")
fits_glmnet_smote <- logistic_reg(penalty = tune(),
                                  mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_glmnet_smote <- read_csv("results_glmnet_smote.csv", col_types = cols())
results_glmnet_smote <- collect_metrics(fits_glmnet_smote) %>% 
  # summarise across repeats
  group_by(penalty, mixture, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            std_err = mean(std_err),
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "glmnet",
         resample = "smote") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config, std_err)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_glmnet_smote %>%
  mutate(mixture = factor(mixture, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = log(penalty), 
                       y = bal_accuracy, 
                       group = mixture, 
                       color = mixture)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("penalty (lambda)") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "mixture (alpha)") +
  theme_classic()

# save results
results_glmnet_smote %>% 
  write_csv(., "results_glmnet_smote.csv")

# save model fits
saveRDS(fits_glmnet_smote, "fits_glmnet_smote.rds")

# upsample ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "glmnet", resample = "up")

# fit model
# fits_glmnet_up <- readRDS("fits_glmnet_up.rds")
fits_glmnet_up <- logistic_reg(penalty = tune(),
                               mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  tune_grid(preprocessor = rec,
            resamples = folds,
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_glmnet_up <- read_csv("results_glmnet_up.csv", col_types = cols())
results_glmnet_up <- collect_metrics(fits_glmnet_up) %>% 
  # summarise across repeats
  group_by(penalty, mixture, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            std_err = mean(std_err),
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "glmnet",
         resample = "up") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config, std_err)) %>% 
  pivot_wider(., names_from = ".metric",
              values_from = "mean") %>%  
  relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
  relocate(spec, .after = sens)

# plot hyperparameters
results_glmnet_up %>%
  mutate(mixture = factor(mixture, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = log(penalty), 
                       y = bal_accuracy, 
                       group = mixture, 
                       color = mixture)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("penalty (lambda)") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "mixture (alpha)") +
  theme_classic()

# save results
results_glmnet_up %>% 
  write_csv(., "results_glmnet_up.csv")

# save model fits
saveRDS(fits_glmnet_up, "fits_glmnet_up.rds")

# reproduce ggplot ---------------- 
results_glmnet_down %>% 
  bind_rows(results_glmnet_none, results_glmnet_smote, results_glmnet_up) %>%
  mutate(mixture = factor(mixture, ordered = TRUE)) %>% 
  ggplot(mapping = aes(x = log(penalty), 
                       y = bal_accuracy, 
                       group = mixture, 
                       color = mixture)) +
  geom_line() +
  facet_grid(algorithm ~ resample) +
  xlab("penalty (lambda)") +
  ylab("balanced accuracy") +
  scale_color_discrete(name = "mixture (alpha)") +
  theme_classic()


# RANDOM FOREST ----------------
# 285 features, 22,740 observations
grid_penalty <- expand_grid(mtry = seq(10, 250, length.out = 100),
                            min_n = seq(2, 100, length.out = 50))

# downsample ---------------- 
# build recipe 
rec <- build_recipe(d = d, algorithm = "random_forest", resample = "down")

# fit model
# fits_rf_down <- readRDS("fits_rf_down.rds")
fits_rf_down <- rand_forest(mtry = tune(),
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
            grid = grid_penalty,
            metrics = metric_set(accuracy, bal_accuracy,
                                 sens, spec, roc_auc))

# get predictions and metrics
# results_rf_down <- read_csv("results_rf_down.csv", col_types = cols())
results_rf_down <- collect_metrics(fits_rf_down) %>% 
  # summarise across repeats
  group_by(mtry, min_n, trees, .metric, .estimator, .config) %>% 
  summarise(mean = mean(mean), 
            std_err = mean(std_err),
            n = sum(n),
            .groups = "drop") %>% 
  mutate(algorithm = "random_forest",
         resample = "down") %>% 
  select(algorithm, resample, everything(), -c(.estimator, .config, std_err)) %>% 
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
  scale_color_discrete(name = "min n")


# save results
results_rf_down %>% 
  write_csv(., "results_rf_down.csv")

# save model fits
saveRDS(fits_rf_down, "fits_rf_down.rds")