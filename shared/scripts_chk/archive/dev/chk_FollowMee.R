#specify a subID to check processed FollowMee rds file for missing dates
#dates are obtained from the sub's VisitDates file

#load libraries
library(readxl) 
library(tidyverse) 
library(lubridate) 

#set up variables
SubID = '241' #edit with the subID desired to check                     NOTE! Run their mak_database first
InPath = 'P:/StudyData/RISK/RawData'

#get start and end dates
dV = read_excel(file.path(InPath, '/', SubID,'/', SubID,'_VisitDates.xlsx', fsep =''),sheet=1)
StartDate = as_date(dV$StartStudy)
EndDate = as_date(dV$EndStudy)

if (EndDate > today()){
  EndDate = today()
}

#get RDS file
d = readRDS(file.path(InPath, '/', SubID,'/', SubID,'_GPSFollowRaw.rds', fsep =''))

#pull out date column
d = d %>% select(Time) %>%
  separate(Time, into=c('Date','Time'), sep=' ') 

#get uniques
d = as_date(unique(d$Date))

#get the date range
date_range = seq(StartDate, EndDate, by = 1)


dMissing =  date_range[!date_range %in% d]

#count of missing dates
length(dMissing)

#View the missing dates
dMissing


