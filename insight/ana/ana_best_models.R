library(here)
library(stringr)

study <- "ema"
data_type <- "all"
cv <- "kfold"
# windows <- c("1hour", "1day", "1week")
windows <- c("1week", "1day", "1hour")
lead <- 0
version <- "v4"

for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/3_ana_best_model.Rmd"), 
                    output_file = str_c("3_ana_best_model_", data_type, "_", 
                                        window, "_", lead, "_", version, "_", cv, ".html"),  
                    output_dir = str_c("P:/studydata/risk/knits/", study),
                    params = list(study = study, 
                                  window = window, 
                                  data_type = data_type, 
                                  cv = cv,
                                  lead = lead, 
                                  version = version),
                    envir = new.env())
}
