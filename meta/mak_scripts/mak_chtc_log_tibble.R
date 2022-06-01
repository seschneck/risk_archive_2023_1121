library(here)
library(stringr)
 
# set parameters to pass in
job_path <- "P:/studydata/risk/chtc/meta/train_1day_0_v2_glmnet/"

# set other parameters for rendering
data_type <- "meta"
# windows <- c("1hour", "1day", "1week")
windows <- c("1day")
lead <- 0
version <- "v2"
algorithm <- "glmnet"

# render parameterized report
for (window in windows) {
  rmarkdown::render(input = here("shared/scripts_parameterized/mak_chtc_log_tibble.Rmd"), 
                    output_file = str_c("chtc_log_", data_type, "_", 
                                        window, "_", lead, "_", version, "_", algorithm, ".html"),  
                    output_dir = str_c("P:/studydata/risk/knits/", data_type),
                    params = list(job_path = job_path),
                    envir = new.env())
}
