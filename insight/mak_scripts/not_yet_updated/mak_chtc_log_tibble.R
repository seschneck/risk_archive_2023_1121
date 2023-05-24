library(here)
library(stringr)

study <- "ema"
data_type <- "all"
# windows <- c("1hour", "1day", "1week")
windows <- c("1week")
lead <- 0
version <- "v4"
algorithm <- "xgboost_nested"  

for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/mak_chtc_log_tibble.Rmd"), 
                    output_file = str_c("mak_chtc_log_tibble_", data_type, "_", 
                                        window, "_", lead, "_", version, "_", algorithm, ".html"), 
                    output_dir = str_c("P:/studydata/risk/knits/", study),
                    params = list(window = window, study = study, data_type = data_type, 
                                  lead = lead, version = version, algorithm = algorithm),
                    envir = new.env())
}
