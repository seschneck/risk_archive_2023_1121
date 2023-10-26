# clean_qualtrics_by_log(d, log)--------------------------------------------------------------
# Used by mak_ scripts to clean Qualtrics files based on entries in a data log

clean_qualtrics_by_log <- function(d, log){
  
  # recode_one log entries-----------------------------
  log_change <- filter(log, log_action == "recode_one")
  
  #uncoment for debugging
  #message("Changing ", nrow(log_change), " entries listed in data log\n")
  
  for (i_log in seq_along(log_change$index)){
    
    # uncoment for debugging
    # message("Processing index: ", log_change$index[i_log])
    
    log_reponse_id <- log_change$response_id[i_log]
    
    # check that only find one matching entry
    i_data <- which(log_reponse_id == d$response_id)
    if (length(i_data) != 1) {
      stop("Problem with  ", log_change$response_id[i_log], " in data log.  Detected ", 
           length(i_data), " matched entries in data")
    }
    
    # check subid
    log_var_name <- log_change$var_name[i_log]
    if (log_var_name == "subid") {
      subid <- log_change$old_value[i_log]
      if (subid != d$subid[i_data] & suppressWarnings(as.numeric(subid) != as.numeric(d$subid[i_data]))) {  #compare both in raw class and as numeric
        stop("subid in log: ", subid, " does not match subid in data: ", d$subid[i_data])
      }
    } else{
      subid <- log_change$subid[i_log]
      if (as.numeric(subid) != as.numeric(d$subid[i_data])) {
        stop("subid in log: ", subid, " does not match subid in data: ", d$subid[i_data])
      }
    }
    
    # check that variable  name is present
    if (!(log_var_name %in% names(d))) {
      stop("var_name: ", log_var_name, " from data log not found in data")
    }
    
    # check that raw and old values match (next two if statements)
    # if no NA, then compare values to check for mismatch
    if (sum(is.na(c(log_change$old_value[i_log], d[[i_data, log_var_name]]))) == 0) {
      if (as.character(log_change$old_value[i_log]) != as.character(d[[i_data, log_var_name]]) & 
          suppressWarnings(as.numeric(log_change$old_value[i_log]) != as.numeric(d[[i_data, log_var_name]]))) {
        stop("Data log old_value: ", log_change$old_value[i_log], " does not match data old_value: ", d[[i_data, log_var_name]])
      }
    } 
    # if one NA, flag mismatch
    if (sum(is.na(c(log_change$old_value[i_log], d[[i_data, log_var_name]]))) == 1) {
      stop("Data log old_value: ", log_change$old_value[i_log], " does not match data old_value: ", d[[i_data, log_var_name]])
    } 
    
    # uncomment for debugging
    # message("LOG;  response_id: ", log_change$response_id[i_log], 
    #         " subid: ",  log_change$subid[i_log],
    #         " old_value: ",  log_change$old_value[i_log],
    #         " new_value: ",  log_change$new_value[i_log])
    # message("DATA; response_id: ", d$response_id[i_data], 
    #         " subid: ",  d$subid[i_data],
    #         " old_value: ",  d[[i_data, log_var_name]], "\n")
    
    # assign new value with correct class
    if (is_double(d[[i_data, log_var_name]])) d[[i_data, log_var_name]] <- as.double(log_change$new_value[i_log])
    if (is_character(d[[i_data, log_var_name]])) d[[i_data, log_var_name]] <- as.character(log_change$new_value[i_log])
    if (is.POSIXt(d[[i_data, log_var_name]])) d[[i_data, log_var_name]] <- as_datetime(log_change$new_value[i_log])
  }
  
  
  # remove log entries ----------------------------------
  log_remove <- filter(log, log_action == "remove")
  if (nrow(log_remove) > 0) {
    
    # uncoment for debugging
    # message("Removing ", nrow(log_remove), " entries listed in data log\n")
    
    d <- filter(d, !(response_id %in% log_remove$response_id))
  }
  
  
  
  return(d)  
}