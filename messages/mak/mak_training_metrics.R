library(here)
library(stringr)

study <- "messages"
data_type <- "all"
cv <- "kfold"
# windows <- c("1hour", "1day", "1week")
windows <- c("1day")
lead <- 0
version <- "v1"

for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/mak_training_metrics.Rmd"), 
                    output_file = str_c("mak_training_metrics_", data_type, "_", 
                                        window, "_", lead, "_", version,"_", cv, ".html"), 
                    output_dir = str_c("P:/studydata/risk/knits/", study),
                    params = list(study = study,
                                  window = window, 
                                  data_type = data_type, 
                                  lead = lead, 
                                  version = version,
                                  cv = cv),
                    envir = new.env())
}
