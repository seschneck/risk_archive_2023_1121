library(here)
library(stringr)
 
study <- "messages"
data_type <- "all"
# windows <- c("1hour", "1day", "1week")
windows <- c("1week")
lead <- 0
version <- "v1"

for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/ana_best_model.Rmd"), 
                    output_file = str_c("ana_best_model_", data_type, "_", 
                                        window, "_", lead, "_", version, ".html"),  
                    output_dir = str_c("P:/studydata/risk/knits/", data_type),
                    params = list(study = "messages", 
                                  window = window, 
                                  data_type = data_type, 
                                  lead = lead, 
                                  version = version),
                    envir = new.env())
}
