library(stringr)

####  Recursive search script
#     Purpose is to find filenames that contain reference to participant logs that will be
#     compiled in the participant_log_locations document in the RISK administration folder

log_filenames <- list.files(path = "..", 
                            pattern = regex("notes|log|participant"),
                            full.names = TRUE,
                            recursive = TRUE) 

####  Results
#     Using a relative path to the StudyData/RISK folder (from administration), a recursive 
#     search of the entire RISK directory returned a list of 57 filenames.

capture.output(log_filenames, file = "participant_log_search_results.txt")
