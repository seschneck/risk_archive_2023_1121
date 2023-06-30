library(tidyverse)

var_score <- function(d, forward_items, reverse_items = NULL, item_range = NULL, prorate = TRUE, max_miss = .20)
{
  library(dplyr)
  
  # select relevant items
  d <- d %>% 
    select(all_of(forward_items), all_of(reverse_items))
  
  # check for out of range
  if (!is.null(item_range)) {
    if (!((min(d, na.rm = TRUE) %in% item_range || max(d, na.rm = TRUE) %in% item_range))) {
      stop("Item score(s) out of range")
    }
  }
  
  # check that length of range == 2 if reverse is not null
  if (!is.null(reverse_items) && length(item_range) != 2) {
    stop("Must specify item range (range) to reverse score items")
  }
  
  # reverse score relevant items
  if (!is.null(reverse_items)) {
    for (v in reverse_items) {                  
      d <- d %>% mutate({{v}} := (sum(item_range) - .data[[v]]))
    }   
  }
  
  max_missing_cols <- ncol(d) * max_miss
  d <- d %>% 
    rowwise() %>% 
    mutate(total = if_else(prorate,
                           mean(c_across(), na.rm = TRUE) * ncol(.), # if true
                           sum(c_across(), na.rm = TRUE))) %>%       # if false
    mutate(missing_cols = sum(is.na(c_across(!contains("total"))))) %>% # count miss cols excluding total
    mutate(total = if_else(missing_cols > max_missing_cols,  # set total to NA if too many missing
                           NA_real_,
                           total)) %>% 
    ungroup()
  return(d$total)
}

intake <- read_csv(file.path("P:/studydata/risk/data_processed/shared/intake.csv"), 
                   show_col_types = F) %>% 
  select(subid, starts_with("aase")) %>% 
  mutate(across(starts_with("aase"),
                ~ case_when(
                  . == "Not at all" ~ 0,
                  . == "Not very" ~ 1,
                  . == "Moderately" ~ 2,
                  . == "Very" ~ 3,
                  . == "Extremely" ~ 4,
                  TRUE ~ NA_integer_
                )))
labels <- read_csv(file.path("P:/studydata/risk/data_processed/insight/labels_1week.csv"),
                   show_col_types = F)

intake <- intake %>% 
  mutate(aase_na = var_score(., forward_items = c("aase_18",
                                                  "aase_16",
                                                  "aase_3",
                                                  "aase_14",
                                                  "aase_6"),
                             item_range = c(0, 4)),
         aase_soc = var_score(., forward_items = c("aase_15",
                                                   "aase_20",
                                                   "aase_4",
                                                   "aase_17",
                                                   "aase_8"),
                              item_range = c(0, 4)),
         aase_phys = var_score(., forward_items = c("aase_2",
                                                    "aase_12",
                                                    "aase_5",
                                                    "aase_13",
                                                    "aase_9"),
                               item_range = c(0, 4)),
         aase_crav = var_score(., forward_items = c("aase_1",
                                                    "aase_7",
                                                    "aase_11",
                                                    "aase_10",
                                                    "aase_19"),
                               item_range = c(0, 4))) 

labels <- labels %>% 
  mutate(lapse_num = if_else(lapse == "yes", 1, 0, NA_integer_)) %>% 
  group_by(subid) %>% 
  summarize(lapse_prop = mean(lapse_num),
            .groups = "keep")

d <- labels %>% 
  left_join(., intake, by = "subid")

cor(d$lapse_prop, d$aase_na) # r = 0.21
cor(d$lapse_prop, d$aase_crav) # r = 0.13
cor(d$lapse_prop, d$aase_phys) # r = 0.07
cor(d$lapse_prop, d$aase_soc) # r = 0.22
