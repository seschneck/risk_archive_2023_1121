library(here)
library(stringr)

data_type <- "ema"
windows <- c("1hour", "1day", "1week")
lead <- 0

for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/ana_best_model.Rmd"), 
                    output_file = str_c("ana_best_model_", data_type, "_", window, ".html"), 
                    output_dir = str_c("P:/studydata/risk/knits/", data_type),
                    params = list(window = window, data_type = data_type, lead = lead),
                    envir = new.env())
}
