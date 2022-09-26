library(here)
library(stringr)

study <- "ema"
data_type <- "all"
# windows <- c("1hour", "1day", "1week")
windows <- c("1day")
lead <- 0
version <- "v4"
cv <- "nested"
algorithms <- "xgboost"  # "all" or algorithm name

for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/mak_training_metrics.Rmd"), 
                    output_file = str_c("mak_training_metrics_", data_type, "_", 
                                        window, "_", lead, "_", version, ".html"), 
                    output_dir = str_c("P:/studydata/risk/knits/", study),
                    params = list(window = window, study = study, data_type = data_type, 
                                  lead = lead, version = version, cv = cv),
                    envir = new.env())
}
