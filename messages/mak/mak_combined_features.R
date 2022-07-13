library(here)
library(stringr)

study <- "messages"
data_type <- "gps"
# windows <- c("1hour", "1day", "1week")
windows <- c("1week")
lead <- 0   # constant for now
version <- "v1"   # update based on version of features

for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/mak_combined_features.Rmd"), 
                    output_file = str_c("mak_combined_features_", data_type, "_", 
                                        window, "_", lead, "_", version, ".html"), 
                    output_dir = str_c("P:/studydata/risk/knits/", study),
                    params = list(study = study, window = window, data_type = data_type, lead = lead, version = version),
                    envir = new.env())
}
 