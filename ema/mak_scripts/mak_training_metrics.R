library(here)
library(stringr)

data_type <- "ema"
windows <- c("1hour", "1day", "1week")
# windows <- c("1day")
lead <- 0
version <- "v1"

for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/mak_training_metrics.Rmd"), 
                    output_file = str_c("mak_training_metrics_", data_type, "_", 
                                        window, "_", lead, "_", version, ".html"), 
                    output_dir = str_c("P:/studydata/risk/knits/", data_type),
                    params = list(window = window, data_type = data_type, lead = lead, version = version),
                    envir = new.env())
}
