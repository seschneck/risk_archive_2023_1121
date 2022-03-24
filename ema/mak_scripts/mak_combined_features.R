library(here)
library(stringr)

data_type <- "ema"
lead <- 0  # NOT YET USED.  WILL NEED TO UPDATE FILE NAMES WHEN USED
# windows <- c("1hour", "1day", "1week")
windows <- c("1day")

for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/mak_combined_features.Rmd"), 
                    output_file = str_c("mak_combined_features_", data_type, "_", window, ".html"), 
                    output_dir = str_c("P:/studydata/risk/knits/ema/", data_type),
                    params = list(window = window, data_type = data_type, lead = lead),
                    envir = new.env())
}
 