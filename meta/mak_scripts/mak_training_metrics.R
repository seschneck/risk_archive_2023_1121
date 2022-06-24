library(here)
library(stringr)

data_type <- "meta"
# windows <- c("1hour", "1day", "1week")
windows <- c("1day")
lead <- 0
version <- "v2"

for (window in windows) {
  rmarkdown::render(input = here("meta/mak_training_metrics_temp.Rmd"), 
                    output_file = str_c("mak_training_metrics_", window, "_", 
                                        version, ".html"), 
                    output_dir = str_c("P:/studydata/risk/knits/", data_type),
                    params = list(window = window, data_type = data_type, lead = lead, version = version),
                    envir = new.env())
}
