library(here)
library(stringr)

study <- "meta" 
data_type <- "meta"
# windows <- c("1hour", "1day", "1week")
windows <- c("1day")
lead <- 0
version <- "v2"

for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/ana_best_model.Rmd"), 
                    output_file = str_c("ana_best_model_", data_type, "_", 
                                        window, "_", lead, "_", version, ".html"),  
                    output_dir = str_c("P:/studydata/risk/knits/", data_type),
                    params = list(window = window, data_type = data_type, 
                                  lead = lead, version = version, study = study),
                    envir = new.env())
}
