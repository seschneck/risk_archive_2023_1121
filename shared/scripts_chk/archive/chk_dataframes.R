#PULL OUT THIS CODE INTO INDIVIDUAL SCRIPTS TO CHECK EACH DATA FILE


#Setup--------
library(lmSupport)
library(lubridate)
library(stringr)
library(tidyverse)

data_path <- 'P:/StudyData/RISK/Analysis/RISK/Data'
count_na <- function(x) sum(is.na(x))

# List of valid subjects--------------
subids <- read_rds(file.path(data_path,'Visits.rds')) %>% 
  filter(Followup1< today())
  pull(SubID)
  
subids
length(subids)

# EMA-------------
#check validity of EMA
ema <- read_rds(file.path(data_path,'EMA.rds'))
length(unique(ema$SubID)) #number of subjects
table(ema$SubID) #EMA per subject
table(ema$EMA_1) #number of lapses
table(ema$Type)  #table of morning vs. later EMA

str(ema)
tNA <- sapply(ema, count_na)
tNA[tNA>0]

sapply(ema,min, na.rm=TRUE)
sapply(ema,max, na.rm=TRUE)
rm(ema)
rm(tNA)

# ID----------------------
id <- read_rds(file.path(data_path,'ID.rds'))
length(unique(id$SubID)) #number of subjects
table(id$SubID)

str(id[, 1:74])
str(id[, 75:ncol(id)])
tNA <- sapply(id, count_na)
tNA[tNA > 0]
#sapply(id,min, na.rm=TRUE)
#sapply(id,max, na.rm=TRUE)
rm(id)
rm(tNA)

# SR------------------------
#check validity of sr
sr <- read_rds(file.path(data_path,'SR.rds'))
length(unique(sr$SubID)) #number of subjects
table(sr$SubID)

str(sr)
tNA <- sapply(sr, count_na)
tNA[tNA > 0]
#sapply(sr,min, na.rm=TRUE)
#sapply(sr,max, na.rm=TRUE)
rm(sr)
rm(tNA)

# Locations------------------------
#check validity of locs
locs <- read_rds(file.path(data_path,'LocationsReport.rds'))
View(locs)
length(unique(locs$SubID))
table(locs$SubID)

str(locs)
tNA <- sapply(locs, count_na)
tNA[tNA > 0]   

table(locs$Type)  
table(locs$Drank)
table(locs$Alcohol)
table(locs$Emotion)
table(locs$Risk)
table(locs$Avoid)

rm(locs)
rm(tNA)

# Contacts------------------------
#check validity of contacts
contacts <- read_rds(file.path(data_path,'ContactsReport.rds'))
View(contacts)
length(unique(contacts$SubID)) 
table(contacts$SubID)

str(contacts)
tNA <- sapply(contacts, count_na)
tNA[tNA > 0]  

table(contacts$Type) 
table(contacts$DrankPast)
table(contacts$DrinkerStatus)
table(contacts$DrinkFuture)
table(contacts$Recovery)
table(contacts$Emotion)
rm(contacts)
rm(tNA)

#Visits------------------------
visits <- read_rds(file.path(data_path,'Visits.rds'))
View(visits)
length(unique(visits$SubID)) 
any(table(visits$SubID) > 1) #should be false

str(visits)
tNA <- sapply(visits, count_na)
tNA[tNA > 0]  
rm(visits)
rm(tNA)

#Dates Report------------------------
dates <- read_rds(file.path(data_path,'DatesReport.rds'))
View(dates)
length(unique(dates$SubID)) 
table(dates$SubID)

str(dates)
tNA <- sapply(dates, count_na)
tNA[tNA > 0]  

rm(dates)
rm(tNA)

#GPS------------------------
gps <- read_rds(file.path(data_path,'GPS.rds'))
View(gps)
length(unique(gps$SubID)) 
table(gps$SubID)

str(gps)
tNA <- sapply(gps, count_na)
tNA[tNA > 0]  

table(gps$Type)
table(gps$PlaceType)
table(gps$Drank)
table(gps$Alcohol)
table(gps$Emotion)
table(gps$Risk)
table(gps$Avoid)

rm(gps)
rm(tNA)

#Voice------------------------
voice <- read_rds(file.path(data_path,'Voice.rds'))
View(voice)
length(unique(voice$SubID)) 
table(voice$SubID)

str(voice)
tNA <- sapply(voice, count_na)
tNA[tNA > 0]  

table(voice$Type)
table(voice$ContactType)
table(voice$DrankPast)
table(voice$DrinkerStatus)
table(voice$DrinkFuture)
table(voice$Recovery)
table(voice$Support)
table(voice$Emotion)

#Check phone numbers
min(voice$Phone)  #NEED TO HANDLE BAD PHONE NUMBERS
max(voice$Phone)

#Check durations (in seconds)
min(voice$Duration)  #NEED TO HANDLE NEGATIVE MIN
max(voice$Duration)
mean(voice$Duration)
windows()
hist(voice$Duration)

rm(voice)
rm(tNA)

#Audio Text------------------------
audio <- read_rds(file.path(data_path,'Audio.rds'))
#View(audio)
dim(audio)
names(audio)
length(unique(audio$SubID)) 
table(audio$SubID)

str(audio)
tNA <- sapply(audio, count_na)
tNA[tNA > 0]  

rm(audio)
rm(tNA)
