library(tidymodels)
library(xgboost)

set.seed(102030)

d <- tidyr::tibble(x1 = stats::runif(100, min = 0, max = 10),
                   x2 = stats::runif(100, min = 0, max = 10),
                   y = sample(c("yes", "no"), size = 100, replace = TRUE)) %>% 
  dplyr::mutate(y = factor(y))

head(d)

# fit model using both predictor variables
model_both <- boost_tree(learn_rate = 0.1,
                         tree_depth = 2,
                         mtry = 1,
                         trees = 100,
                         stop_iter = 20) %>% 
  set_engine("xgboost",
             validation = 0.2) %>% 
  set_mode("classification") %>%
  fit(y ~ ., data = d)

summary(model_both)

# fit model using only 1 predictor variable
model_x1 <- boost_tree(learn_rate = 0.1,
                       tree_depth = 2,
                       mtry = 1,
                       trees = 100,
                       stop_iter = 20) %>% 
  set_engine("xgboost",
             validation = 0.2) %>% 
  set_mode("classification") %>%
  fit(y ~ x1, data = d)

reprex::reprex(venue = "gh")
