library(tidyverse)

path_files <- "P:/studydata/risk/chtc/meta/jobs/features/features_1_day/output/features_part_3"

file_names <- enframe(list.files(path_files, pattern = "features_"), name = NULL)

write_csv(file_names, "file_names.txt")
# may need to open txt file in notepad++ to change EOL conversions to Unix

# to remove select files at chtc
# xargs --arg-file="file_names.txt" rm
