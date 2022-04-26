library(tidymodels)
library(tidyverse)

source("../lab_support/chtc/static_files/input/fun_chtc.R")

# read in data
data <- vroom::vroom(here::here("shared/demos/dem_rset/data.csv"), col_types = vroom::cols())

# create splits object using our function
# first set cv_type and grouping variable (global parameters defined in training controls)
cv_type <- "group_kfold_1_x_10"
group <- "subid"

# create splits object
set.seed(102030)
folds <- make_splits(d = data, cv_type = cv_type, group = group)

# set fold index
fold_index <- 1

# pull out fold
# d_in <- analysis(folds$splits[[fold_index]])
# d_out <- assessment(folds$splits[[fold_index]])

# turn single fold into rset
fold <- folds[fold_index, ]
manual_rset(fold$splits, fold$id)
