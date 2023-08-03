library(stringr)

study <- "insight"
data_type <- "all"
# windows <- c("1hour", "1day", "1week")
windows <- c("1week")
lead <- 0
version <- "v1"
cv <- "nested"
algorithms <- "xgboost"  # "all" or algorithm name

for (window in windows) {
  rmarkdown::render(input = file.path("shared/scripts_parameterized/nested/1_mak_metrics_inner.Rmd"), 
                    output_file = str_c("1_mak_metrics_inner_", data_type, "_", 
                                        window, "_", lead, "_", version, "_", cv, ".html"), 
                    output_dir = str_c("P:/studydata/risk/knits/", study),
                    params = list(window = window, study = study, data_type = data_type, 
                                  lead = lead, version = version, cv = cv),
                    envir = new.env())
}
