---
title: "Review and resolution of each participants work/school locations"
author: "Hannah Moshontz"
date: '2021-12-27'
output: 
  html_document:
    toc: true 
    toc_depth: 4
editor_options: 
  chunk_output_type: console
knit: (function(input, ...) {
    rmarkdown::render(
      input,
      output_dir = "P:/studydata/risk/knits/context")
  })
---


### Notes

Purpose: This file reviews each person's GPS data to resolve issues related to multiple work and school locations.

Inputs:  user_locations_check.csv, which is a copy of a sheet in "workbook_sent_from_xinyi.xlsx" with cleaned variable names. Many of the columns we will not use (the binary description and decision columns Xinyi created), but many contain information about activity zones that we do not otherwise have.


### Setup

Packages and Source

```r
library(tidyverse)
library(janitor)
library(purrr)
library(kableExtra)
library(tidylog)
library(ggmap)  
register_google(key = "AIzaSyBBwIG1tC4QZi5QrTnhXXLktIPD8NlVVI0") #API key
```

### Import file


```r
location_descriptions <- read_csv("./user_locations_check.csv",
                            col_names = TRUE) %>% 
  clean_names("snake") %>% 
  glimpse()
```

```
## 
## -- Column specification -------------------------------------------------------------------
## cols(
##   subid = col_double(),
##   type = col_double(),
##   n_home_locations = col_double(),
##   n_work_locations = col_double(),
##   n_home_zone = col_double(),
##   n_work_zones = col_double(),
##   time_at_all_homes = col_character(),
##   time_at_all_workplaces = col_character(),
##   more_than_two_work = col_character(),
##   distant_work = col_character(),
##   eliminate_observations_temporal = col_character(),
##   combine_workplaces = col_character(),
##   n_school_locations = col_character(),
##   home_workplace = col_character(),
##   missing_location = col_character(),
##   notes = col_character()
## )
```

```
## Rows: 169
## Columns: 16
## $ subid                           [3m[38;5;246m<dbl>[39m[23m 1, 2, 3, 5, 6, 7, 9, 10, 11, 15, 16, 17, 18, 19, ~
## $ type                            [3m[38;5;246m<dbl>[39m[23m 1, 2, 1, 1, 0, 1, 2, 0, 0, 2, 1, 0, 1, 0, 0, 2, 1~
## $ n_home_locations                [3m[38;5;246m<dbl>[39m[23m 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
## $ n_work_locations                [3m[38;5;246m<dbl>[39m[23m 7, 1, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 7, 6, 7, 4~
## $ n_home_zone                     [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1~
## $ n_work_zones                    [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 2, 0, 1, 4, 4, 4, 2~
## $ time_at_all_homes               [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, "YES", NA, NA, NA, NA, "YES", "YE~
## $ time_at_all_workplaces          [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, "YES", NA, NA, NA, NA, NA, NA, NA~
## $ more_than_two_work              [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
## $ distant_work                    [3m[38;5;246m<chr>[39m[23m "NO", NA, NA, NA, "YES", "NO", NA, NA, NA, "YES",~
## $ eliminate_observations_temporal [3m[38;5;246m<chr>[39m[23m NA, "YES", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ combine_workplaces              [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
## $ n_school_locations              [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, NA, NA, NA, "YES", "YES", NA, NA,~
## $ home_workplace                  [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, "YES", NA, NA, NA~
## $ missing_location                [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "YES"~
## $ notes                           [3m[38;5;246m<chr>[39m[23m NA, "only one date for work activity", NA, NA, "t~
```

```r
locations <- read_csv("P:/studydata/risk/data_processed/gps/to_share/locations.csv",
                      col_names = TRUE) %>% 
  mutate(subid = as.numeric(subid)) %>% 
  clean_names("snake") %>% 
  mutate(time = lubridate::as_datetime(utc, tz = "America/Chicago")) %>% 
  glimpse()
```

```
## 
## -- Column specification -------------------------------------------------------------------
## cols(
##   subid = col_character(),
##   utc = col_double(),
##   street_address = col_character(),
##   city = col_character(),
##   state = col_character(),
##   type = col_character(),
##   drank = col_character(),
##   alcohol = col_character(),
##   risk = col_character(),
##   avoid = col_character(),
##   vacation = col_character(),
##   lat = col_double(),
##   long = col_double()
## )
```

```
## mutate: converted 'subid' from character to double (0 new NA)
```

```
## mutate: new variable 'time' (double) with 547 unique values and <1% NA
```

```
## Rows: 5,424
## Columns: 14
## $ subid          [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
## $ utc            [3m[38;5;246m<dbl>[39m[23m 1487871000, 1487871000, 1487871000, 1487871000, 1487871000, 148787~
## $ street_address [3m[38;5;246m<chr>[39m[23m "610 Merryturn Road", "4521 Cottage Grove Road", "5309 Portsmouth ~
## $ city           [3m[38;5;246m<chr>[39m[23m "Madison", "Madison", "Madison", "Madison", "Madison", "Madison", ~
## $ state          [3m[38;5;246m<chr>[39m[23m "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", "WI", ~
## $ type           [3m[38;5;246m<chr>[39m[23m "HOME OF FAMILY MEMBER", "LIQUOR STORE", "HOME", "LIQUOR STORE", "~
## $ drank          [3m[38;5;246m<chr>[39m[23m "YES", "NO", "YES", "NO", "NO", "NO", "NO", "NO", "NO", "NO", "NO"~
## $ alcohol        [3m[38;5;246m<chr>[39m[23m "YES", "YES", "YES", "YES", "YES", "NO", "NO", "NO", "NO", "NO", "~
## $ risk           [3m[38;5;246m<chr>[39m[23m "LOW", "HIGH", "LOW", "HIGH", "HIGH", "NO", "NO", "NO", "NO", "NO"~
## $ avoid          [3m[38;5;246m<chr>[39m[23m "NO", "YES", "NO", "YES", "YES", "NO", "NO", "NO", "NO", "NO", "NO~
## $ vacation       [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ lat            [3m[38;5;246m<dbl>[39m[23m 43.09011, 43.08377, 43.09328, 43.08460, 43.09681, 43.11457, 43.098~
## $ long           [3m[38;5;246m<dbl>[39m[23m -89.29272, -89.30367, -89.29017, -89.32479, -89.31896, -89.32980, ~
## $ time           [3m[38;5;246m<dttm>[39m[23m 2017-02-23 11:30:00, 2017-02-23 11:30:00, 2017-02-23 11:30:00, 20~
```

```r
gps <- read_csv("P:/studydata/risk/data_processed/gps/to_share/gps.csv") %>% 
  glimpse() #using the gps data we shared with xinyi
```

```
## 
## -- Column specification -------------------------------------------------------------------
## cols(
##   lat = col_double(),
##   long = col_double(),
##   time = col_datetime(format = ""),
##   accuracy = col_logical(),
##   sgmnt_type = col_character(),
##   trckpnt_type = col_character(),
##   app_source = col_character(),
##   data_type = col_character(),
##   speed_kmh = col_logical(),
##   altitude_meters = col_logical(),
##   direction = col_logical(),
##   subid = col_double()
## )
```

```
## Warning: 1967586 parsing failures.
##    row             col           expected actual                                                    file
## 575677 accuracy        1/0/T/F/TRUE/FALSE    19  'P:/studydata/risk/data_processed/gps/to_share/gps.csv'
## 575677 altitude_meters 1/0/T/F/TRUE/FALSE    233 'P:/studydata/risk/data_processed/gps/to_share/gps.csv'
## 575678 accuracy        1/0/T/F/TRUE/FALSE    18  'P:/studydata/risk/data_processed/gps/to_share/gps.csv'
## 575678 altitude_meters 1/0/T/F/TRUE/FALSE    233 'P:/studydata/risk/data_processed/gps/to_share/gps.csv'
## 575679 accuracy        1/0/T/F/TRUE/FALSE    3   'P:/studydata/risk/data_processed/gps/to_share/gps.csv'
## ...... ............... .................. ...... .......................................................
## See problems(...) for more details.
```

```
## Rows: 1,240,944
## Columns: 12
## $ lat             [3m[38;5;246m<dbl>[39m[23m 43.07284, 43.07284, 43.07284, 43.07284, 43.07284, 43.07284, 43.07~
## $ long            [3m[38;5;246m<dbl>[39m[23m -89.40627, -89.40627, -89.40627, -89.40627, -89.40627, -89.40627,~
## $ time            [3m[38;5;246m<dttm>[39m[23m 2017-03-01 15:47:24, 2017-03-01 18:03:11, 2017-03-01 19:02:19, 2~
## $ accuracy        [3m[38;5;246m<lgl>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
## $ sgmnt_type      [3m[38;5;246m<chr>[39m[23m "place", "place", "place", "place", "place", "move", "move", "mov~
## $ trckpnt_type    [3m[38;5;246m<chr>[39m[23m "unknown", "walking", "walking", "walking", "unknown", "transport~
## $ app_source      [3m[38;5;246m<chr>[39m[23m "moves", "moves", "moves", "moves", "moves", "moves", "moves", "m~
## $ data_type       [3m[38;5;246m<chr>[39m[23m "gps", "gps", "gps", "gps", "gps", "gps", "gps", "gps", "gps", "g~
## $ speed_kmh       [3m[38;5;246m<lgl>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
## $ altitude_meters [3m[38;5;246m<lgl>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
## $ direction       [3m[38;5;246m<lgl>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
## $ subid           [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~
```


### Merge


```r
location_check <- gps %>% 
  mutate(location_source = "gps_data") %>% 
  select(subid, lat, long, time, location_source) %>% 
  bind_rows(
    locations %>% 
      mutate(location_source = "labeled_home") %>% 
      filter(type == "HOME") %>% 
      select(subid, lat, long, time, location_source)) %>% 
  group_by(subid) %>% 
  nest() %>% 
  left_join(
    select(
      location_descriptions,
      subid, type, n_home_locations, n_work_locations, 
      n_home_zones = n_home_zone, n_work_zones), 
    by = "subid") %>% 
  rename(gps_data = data)
```

```
## mutate: new variable 'location_source' (character) with one unique value and 0% NA
```

```
## select: dropped 8 variables (accuracy, sgmnt_type, trckpnt_type, app_source, data_type, …)
```

```
## mutate: new variable 'location_source' (character) with one unique value and 0% NA
```

```
## filter: removed 5,238 rows (97%), 186 rows remaining
```

```
## select: dropped 10 variables (utc, street_address, city, state, type, …)
```

```
## group_by: one grouping variable (subid)
```

```
## select: renamed one variable (n_home_zones) and dropped 10 variables
```

```
## left_join: added 5 columns (type, n_home_locations, n_work_locations, n_home_zones, n_work_zones)
```

```
##            > rows only in x     0
```

```
##            > rows only in y  (  2)
```

```
##            > matched rows     167
```

```
##            >                 =====
```

```
##            > rows total       167
```

```
## rename: renamed one variable (gps_data)
```

#### Check for issues


```r
#check for subids only in location_description
missing_ids <- unique(location_descriptions$subid)[!unique(location_descriptions$subid) %in% unique(gps$subid)]

#clean up
rm(gps)
```

There are 2 subjects that are in location descriptions and have locations, but are not in the processed gps dataset. Their subids are: 24, 67.

TODO: Get more information about where their removal is documented. "studydata/risk/knits/gps/mak_gps_enriched.html" does not note their removal. Do not see another mak_gps html but the gps file was created in the last month. The notes file does not have any entry for 24 or 67. Clearly these people had data at some point, because they have locations.

### Examine participants

#### Set up variables


```r
location_check <- location_check %>% 
  mutate(action = case_when(type == 1 ~ "use as is",
                                type == 2 ~ "use with modification",
                                type == 0 ~ "remove all data")) 
```

```
## mutate (grouped): new variable 'action' (character) with 4 unique values and 87% NA
```


#### Categorize particpants with unique labeled and observed home and work


```r
location_check <- location_check %>% 
  mutate(action = case_when(n_home_locations == 1 & 
                     n_work_locations == 1 &
                     n_home_zones == 1 &
                     n_work_zones == 1 ~ "use as is",
                     TRUE ~ action))
```

```
## mutate (grouped): changed 30 values (18%) of 'action' (29 fewer NA)
```

#### Check participants with multiple home locations


```r
multiple_homes_ids <- locations %>% 
  filter(type == "HOME") %>% 
  group_by(subid) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(subid)
```

```
## filter: removed 5,238 rows (97%), 186 rows remaining
```

```
## group_by: one grouping variable (subid)
```

```
## count: now 163 rows and 2 columns, one group variable remaining (subid)
```

```
## filter (grouped): removed 142 rows (87%), 21 rows remaining
```

```r
#function that takes subid and creates maps around each home
#this will iterate through multiple_homes_ids list

check_labeled_locations <- function(subid){
  
subject <- as.numeric(subid) #renaming to prevent issues  
  
gps <- location_check %>% 
  filter(subid == subject) %>% 
  pull(gps_data) %>% 
  pluck(1) %>% 
  mutate(location_source = case_when(location_source == "labeled_home" ~ "home",
                                     location_source == "gps_data" ~ "observed location"))

homes <- gps %>% 
  filter(location_source == "home") %>% 
  select(long, lat) 

if (nrow(homes) > 1) {
  
  map2(homes$long, homes$lat, 
       ~ggmap(
         get_map(
           c(.x, .y), zoom = 18)) +
         geom_point(aes(x = long, 
                        y = lat,
                        color = location_source),
                    data = gps)) 
}

if (nrow(homes) < 2) message("multiple homes not detected")

}

check_labeled_locations("43")
```

```
## filter (grouped): removed 166 rows (99%), one row remaining
```

```
## mutate: changed 27,940 values (100%) of 'location_source' (0 new NA)
```

```
## filter: removed 27,937 rows (>99%), 3 rows remaining
```

```
## select: dropped 2 variables (time, location_source)
```

```
## Source : https://maps.googleapis.com/maps/api/staticmap?center=43.027239,-89.418402&zoom=18&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxx
```

```
## Source : https://maps.googleapis.com/maps/api/staticmap?center=43.102937,-89.352835&zoom=18&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxx
```

```
## Source : https://maps.googleapis.com/maps/api/staticmap?center=42.837672,-88.733222&zoom=18&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxx
```
