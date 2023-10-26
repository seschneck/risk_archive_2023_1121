#This script adds geolocations (lat/lon) to a specific subjects context/places 
#file.  This should have been done during data collection but was missed for
#a few subjects.   This is a quick, cludge solution
#JJC, 2021-12-02

library(conflicted) # detect and warn about function conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

library(here)  # establish project directory consistently as working directory
library(ggmap)
library(readxl)
library(writexl)
library(stringr)

path_admin <- "P:/administration"
path_raw <- "P:/studydata/risk/data_raw"

# register google API key
source(here("../lab_support", "get_credentials.R"))
creds <- get_credentials("google_api", here(path_admin, "credentials.csv"))
register_google(key = creds$key)

subid <- "067"
read_xlsx(here(path_raw, subid, str_c(subid, "_Locations.xlsx"))) %>% 
  mutate(FullAddress = str_c(.$StreetAddress, .$City, .$State, sep = ", ")) %>% 
  mutate_geocode(FullAddress) %>% 
  rename(Lat = lat, Long = lon) %>%  #matching name and case from original files
  relocate(Long, .after = last_col()) %>% 
  write_xlsx(here(path_raw, subid, str_c(subid, "_Locations.xlsx")))
