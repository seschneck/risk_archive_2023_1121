# Working script for EMA data from RISK

#Updates
# there are a few instances of people taking hours to complete surveys

# Setting up -------------------------------------------------------------------
library(tidyverse)
library(lmSupport)
library(mice)

# Loading data and date wrangling --------- ------------------------------------

EMA <- read_rds('./RISK/Data/EMA.rds') #loading data
Y <- read_rds('./RISK/Data/YDay.rds') 
X <- read_rds('./RISK/Data/XDay.rds')
ID <- read_rds('./RISK/Data/ID.rds')
#converting the unix time to actual time
EMA$Date <- EMA$UTC %>% #converting into human readable form
  as.numeric %>% 
  as.POSIXct(tz='America/Chicago', origin="1970-01-01")
EMA$UTC_Date <- EMA$Date #saving this to compare to StartDate
#converting Start and End dates to chicago
attributes(EMA$StartDate)$tzone <- "America/Chicago"
attributes(EMA$EndDate)$tzone <- "America/Chicago"

EMA <- EMA %>% #making columns for day and time
  separate(Date, into = c("Day", "Time"), sep = " ")

#Checking basic descriptives and properties ------------------------------------

#basic properties
nrow(EMA) #28,762 rows

#Observations per person
EMA %>% group_by(SubID) %>% 
  summarize(N = n()) %>% 
  varDescribe()  %>% 
  filter(vars == 2) 
#120 subjects provide an avg of ~240 obs each (range 2-376)

#Observations per person by type
#type = 1 (morning)
EMA %>% group_by(SubID, Type) %>% 
  summarize(N = n()) %>% 
  filter(Type == 1) %>% 
  varDescribe() %>% 
  filter(vars == 3) 
#120 subjects provide an avg of ~60 morning observations (range 1 - 131)

#number of morning surveys per day
EMA %>% group_by(SubID, Day) %>%
  filter(Type == 1) %>% 
  summarize(N = n()) %>% 
  varDescribe() %>% 
  filter(vars ==3)
#mean is 1.04 morning surveys per day
#someone has 5 morning surveys for one day!

#Morning surveys
#digging deeper...
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 1) %>% 
  summarize(N = n()) %>% 
  filter(N > 1) %>% 
  .$N %>% table()
#240 instances of two per day, 20 of 3 per day, 1 of 4 and 1 of 5

#type = 2 (non-morning)
EMA %>% group_by(SubID, Type) %>% 
  summarize(N = n()) %>% 
  filter(Type == 2) %>% 
  varDescribe() %>% 
  filter(vars == 3) 
#120 subjects provide an avg of ~180 non-morning surveys (range 1 - 286)

#number of non-morning surveys per day
EMA %>% group_by(SubID, Day) %>%
  filter(Type == 2) %>% 
  summarize(N = n()) %>% 
  varDescribe() %>% 
  filter(vars ==3)
#Average of 2.67 per day, but a max of 10!

#Getting counts of number of day surveys by day
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 2) %>% 
  summarize(N = n()) %>% 
  filter(N > 1) %>% 
  .$N %>% table()
#the mode is 2, but there are 85 instances of 5 per day 
#instances of 7, 8, 9, 10...

#Sources of duplicate surveys per day ---------------------------------------

#Ruled out sources of duplicates are:
# 1. Exact identical observations (all are unique)
EMA %>% duplicated() %>% table()
# 2. People triggering the survey but not completing them 
#(no - these are removed unless there is a lapse)
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 1) %>% 
  mutate(AMcount = cumsum(Type)) %>% 
  filter(AMcount > 1) %>% 
  mutate(missing = is.na(EMA_10)) %>% 
  filter(missing > 0)
# 3. People not finishing one survey, then restarting?
#(no - these are removed unless there is a lapse)
#(also incomplete surveys don't account for all cases)
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 1) %>% 
  mutate(AMcount = cumsum(Type)) %>% #making a temp AM count variable
  filter(AMcount == 1) %>%  #looking at missingness in 1st survey
  mutate(missing = is.na(EMA_10)) %>%  #chose the last Q
  filter(missing > 0)
#4. UTC coding lapse date rather than survey date
#No -- this never happens

#Sources of duplicates are:
# 1. People taking multiple surveys in the 6 hour window
# e.g., taking many in a row
EMA %>% filter(SubID == 79, Day == "2018-08-16", Type == 1) %>% 
  View()
# 2. People taking multiple surveys in a window after a reminder was sent
# Not important to directly show / same as #1

# 3. Early participants taking multiple surveys
# Not important to establish this directly, as survey signal has same problems

# 4. A few cases of people with a lot of duplicate surveys for known reasons
# LOOK AT DATA LOG IN RAW DATA

# 5. Partial finishes that were recoded as complete if they answered lapse Qs
# yes (see patterns of missingness)

#Checking the people with lots of duplicates -----------------------------------

#ordering dates first
EMA <- select(EMA, UTC_Date, StartDate, EndDate, everything())

#Morning surveys ####

#who had 5 per day?
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 1) %>% 
  summarize(N = n()) %>% 
  filter(N == 5)
#subj 79 on 2018-08-16
#what were those observations?
EMA %>% filter(SubID == 79, Day == "2018-08-16", Type == 1) %>% 
  View()
#took many in a row, giving similar responses

#who had 4?
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 1) %>% 
  summarize(N = n()) %>% 
  filter(N == 4)
#subj 30 on 2017-12-24
#what were those observations?
EMA %>% filter(SubID == 30, Day == "2017-12-24", Type ==1) %>% 
  View()
#took many in a row giving varied answers but mostly similar

#how many people had 3?
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 1) %>% 
  summarize(N = n()) %>% 
  filter(N == 3) %>% arrange(SubID) 
#20+ instances by several people
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 1) %>% 
  summarize(N = n()) %>% 
  filter(N == 3) %>% 
  .$SubID %>% table()
#10 people -- with subjects 78 and 85 having lots of instances

#Looking at these people

#subj 52 on 2018-04-15
EMA %>% filter(SubID == 52, Day == "2018-04-15", Type ==1) %>% 
  View()
# spaced out responses throughout the day

#subj 65 on 2018-06-27
EMA %>% filter(SubID == 65, Day == "2018-06-27", Type ==1) %>% 
  View()
#similar duplicates all in a row

#subj 74 on 2018-06-08
EMA %>% filter(SubID == 74, Day == "2018-06-08", Type ==1) %>% 
  View()
#similar duplicates all in a row

# subj 85 on 2018-07-28, 2018-08-08, 2018-08-15, 2018-08-21, 
# 2018-09-03, 2018-09-07, 2018-09-13
EMA %>% filter(SubID == 85, 
               Day == "2018-07-28" | 
                 Day == "2018-08-08"| 
                 Day == "2018-08-15"|
                 Day == "2018-08-21"|
                 Day == "2018-09-03"|
                 Day == "2018-09-07"| 
                 Day == "2018-09-13",
               Type ==1) %>% View()
#none of these are lapses
#giving the same responses to 8, 9, and 10
#looking at all data from this person
EMA %>%  filter(SubID == 85) %>%  .$EMA_8 %>% table()
#I guess this is plausible? almost always said 7 but sometimes 6 and twice 4
EMA %>%  filter(SubID == 85) %>%  .$EMA_9 %>% table()
#less plausible, said 11 every time and 7 once
EMA %>%  filter(SubID == 85) %>%  .$EMA_10 %>% table()
#said 1 every time. I guess plausible.

# subj 78 on seveal days
EMA %>% filter(SubID == 78, 
               Day == "2018-08-17" |
                 Day == "2018-08-19" | 
                 Day == "2018-08-24" | 
                 Day == "2018-08-30",
               Type ==1) %>% View()
#seems fine

# subj 190 on 2019-03-03  
EMA %>% filter(SubID == 190, Day == "2019-03-03", Type ==1) %>% 
  View()
#several in a row, seems fine

#subj 185 on several days
EMA %>% filter(SubID == 185, 
               Day == "2019-03-04" |   
                 Day == "2019-03-05" |   
                 Day == "2019-03-22" |   
                 Day == "2019-03-31" ,
               Type ==1) %>% View()
#Lots of duplicates on the same day in a row

# subj 138 on 2018-12-23  
EMA %>% filter(SubID == 138 , Day == "2018-12-23", Type ==1) %>% 
  View()
#ditto

# subj 117 on 2018-10-19   
EMA %>% filter(SubID == 117 , Day == "2018-10-19", Type ==1) %>% 
  View()
#same

# subj 98 on 2018-08-27    
EMA %>% filter(SubID == 98, Day == "2018-08-27", Type ==1) %>% 
  View()
#seems fine also

# Day surveys ####

#Multiple day surveys
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 2) %>% 
  summarize(N = n()) %>% 
  filter(N > 5) %>% arrange(SubID)

# subj   7 on 2017-07-02 completed  6 surveys
# subj   7 on 2017-08-16 completed  6 surveys
EMA %>% filter(SubID == 7, Day == "2017-07-02" | Day == "2017-08-16", Type == 2) %>% 
  View()
#a lot spaced out throughout the day

# subj  25 on 2018-02-12 completed  6 surveys
EMA %>% filter(SubID == 25, Day == "2018-02-12", Type == 2) %>% 
  View()
#many in a row but seems okay

# subj  53 on 2018-03-30 completed  6 surveys
# subj  53 on 2018-03-31 completed  6 surveys
# subj  53 on 2018-04-06 completed  6 surveys
# subj  53 on 2018-05-12 completed  6 surveys
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 2) %>% 
  summarize(N = n()) %>% 
  filter(SubID == 53) %>% arrange(-N)
#they also completed 5 and 4 on other days 
EMA %>% filter(SubID == 53, Day == "2018-03-30", Type == 2) %>% 
  View()
#responses inconsistent but spaced 
EMA %>% filter(SubID == 53, Day == "2018-03-31", Type == 2) %>% 
  View()
#I think this person just likes reporting when things happen

# subj  65 on 2018-06-19 completed  6 surveys
EMA %>% filter(SubID == 65, Day == "2018-06-19", Type == 2) %>% 
  View()

#checking people with 10 in one day and those who have lots of high repeat days

# subj  66 on 2018-05-23 completed 10 surveys
EMA %>% filter(SubID == 66, Day == "2018-05-23", Type == 2) %>% 
  View()
#responses don't look totally normal... checking to see if they have lots of
#other high survey days
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 2) %>% 
  summarize(N = n()) %>% 
  filter(SubID == 66) %>% arrange(-N)
#not a lot,,, 2 with 5, 5 with four

# subj  79 on 2018-06-25 completed  7 surveys
# subj  79 on 2018-06-28 completed  6 surveys
# subj  79 on 2018-06-30 completed  6 surveys
# subj  79 on 2018-07-15 completed  6 surveys
EMA %>% group_by(SubID, Day) %>% 
  filter(Type == 2) %>% 
  summarize(N = n()) %>% 
  filter(SubID == 79) %>% arrange(-N)
#they had multiple surveys on most days
EMA %>% filter(SubID == 79, Day == "2018-06-30", Type == 2) %>% 
  View()
#not sure. seems okay.


# subj  84 on 2018-07-26 completed  7 surveys
# subj  84 on 2018-08-21 completed 10 surveys
# subj  84 on 2018-08-24 completed 10 surveys
# subj  84 on 2018-08-25 completed  6 surveys
# subj  84 on 2018-08-28 completed  7 surveys
# subj  84 on 2018-08-29 completed  8 surveys
# subj  84 on 2018-08-30 completed  8 surveys
# subj  84 on 2018-09-03 completed  8 surveys
# subj  84 on 2018-09-05 completed  7 surveys
# subj  84 on 2018-09-21 completed  6 surveys
# subj  84 on 2018-09-25 completed  6 surveys
# subj  84 on 2018-09-28 completed  6 surveys
EMA %>% filter(SubID == 84, Day == "2018-08-21", Type == 2) %>% 
  View()
#a bunch within a short window

#Asking project staff about subjects: 84, 79, 66, 85

## Sarah ! Check in on this just to confirm we know what was up
## with these subject

#Basic descriptives ------------------------------------------------------------
# Descriptives - mins and maxes, distributions
EMA %>% select(-Type, -SubID, -UTC, -EndDate, -Day, -Time) %>% 
  varDescribe(2)
#11 point scales, people using the full range of the scale
#some milk skewness and kurtosis

EMA %>% .$EMA_2 %>% hist(main = 'Greatest Urge Histogram') #greatest urge is really zero inf
#does that make sense?
#I suppose if people are filling out a lot of surveys per day there isn't time for urges.
EMA %>% .$EMA_3 %>% hist(main = 'Risks Histogram') #risks also really zero inflated
#are people not perceiving risks that are there?
EMA %>% .$EMA_4 %>% hist(main = 'Hassles Histogram') #hassles also zero inflated
#also this is surprising to me!
EMA %>% .$EMA_5 %>% hist(main = 'Positive Events') #same with positive events

#What about distributions within people?
#For example, greatest urge
EMA %>% group_by(SubID) %>% 
  summarize(N = n(),
            Urge_mean = mean(EMA_2)) %>%   
  .$Urge_mean %>% 
  hist(main = 'Urge Within-Person Mean')
#max urge
EMA %>% group_by(SubID) %>% 
  summarize(N = n(),
            Urge_max = max(EMA_2)) %>%   
  .$Urge_max %>% 
  hist(main = 'Urge Within-Person Max')
#for most people this was 12, but for a surprisingly large number, it was lower
#suggests that lapses might be more of a choice than failures to resist temptations
# (or they are impulses)

#and for min?
EMA %>% group_by(SubID) %>% 
  summarize(N = n(),
            Urge_min = min(EMA_2)) %>%   
  .$Urge_min %>% 
  hist(main = 'Urge Within-Person Min')
#almost everyone used the full range of the scale

#how about for risks?
EMA %>% group_by(SubID) %>% 
  summarize(N = n(),
            Risks_mean = mean(EMA_3)) %>%   
  .$Risks_mean %>% 
  hist(main = 'Risks Within-Person Mean')
#surprisingly skew
#max?
EMA %>% group_by(SubID) %>% 
  summarize(N = n(),
            Risks_max = max(EMA_3)) %>%   
  .$Risks_max %>% 
  hist(main = 'Risks Within-Person Mean')
#more people reporting max of 12
EMA %>% group_by(SubID) %>% 
  summarize(N = n(),
            Risks_min = min(EMA_3)) %>%   
  .$Risks_min %>% 
  hist(main = 'Risks Within-Person Min')
#people using the full lower range of the scale

##Missing data
# Estimate response rates within and between participants.
jpeg('./EMA/EMA_missing.jpg')
md.pattern(EMA)
dev.off
#missing data patterns... seem normal? 

#did conditional display of questions 1.1, 1.3, 1.5 work as it should?
EMA %>% filter(EMA_1 == 2 & is.na(EMA_1.1) & is.na(EMA_1.3) & is.na(EMA_1.5))
#nobody answered yes that they drank but has all three follow-ups NA
EMA %>% filter(EMA_1 == 1 & (!is.na(EMA_1.1) | !is.na(EMA_1.3) | !is.na(EMA_1.5)))
#nobody answered no that they didn't drink but has any response to a follow-up Q
#so, yes, conditional display of these questions worked

#some follow up questions are missing from this dataframe (EMA 1.2, 1.4)
#these were collapsed into other items

#did morning and evening surveys have what they should?
EMA %>% filter(Type == 1 & is.na(EMA_8) & is.na(EMA_9) & is.na(EMA_10))
#6 people have NAs for all these questions
#but they have NAs for all the other questions, too... so maybe they didn't mean 
#to fill out the surveys?

#did evening surveys not have what they shouldn't?
EMA %>% filter(Type == 2 & (!is.na(EMA_8) | !is.na(EMA_9) | !is.na(EMA_10)))
#yes

# Survey duration ---------------------------------------------------------------------------


#did anyone take a very long time to complete the survey (compare startdate and enddate?)
EMA %>% mutate(SurveyDuration = EndDate - StartDate) %>% 
  mutate(SurveyDuration_hours = as.numeric(SurveyDuration / 60 / 60)) %>% 
  arrange(-SurveyDuration) %>% 
  select(SurveyDuration, SurveyDuration_hours, SubID, everything()) %>% 
  filter(SurveyDuration_hours >1) %>% nrow()
#there are 84 observations of people taking more than an hour to complete the survey

EMA %>% mutate(SurveyDuration = EndDate - StartDate) %>% 
  mutate(SurveyDuration_hours = as.numeric(SurveyDuration / 60 / 60)) %>% 
  arrange(-SurveyDuration) %>% 
  select(SurveyDuration, SurveyDuration_hours, SubID, everything()) %>% 
  filter(SurveyDuration_hours >3) %>% nrow()
#there are 19 observations of people taking more than 3 hours

EMA %>% mutate(SurveyDuration = EndDate - StartDate) %>% 
  mutate(SurveyDuration_hours = as.numeric(SurveyDuration / 60 / 60)) %>% 
  arrange(-SurveyDuration) %>% 
  select(SurveyDuration, SurveyDuration_hours, SubID, everything()) %>% 
  filter(SurveyDuration_hours >5) %>% View()
#there are 12 instances of people taking more than 5 hours

#how about at the short end?
EMA %>% mutate(SurveyDuration = EndDate - StartDate) %>% 
  mutate(SurveyDuration_hours = as.numeric(SurveyDuration / 60 / 60)) %>% 
  arrange(-SurveyDuration) %>% 
  select(SurveyDuration, SurveyDuration_hours, SubID, everything()) %>% 
  filter(SurveyDuration < 30) %>% nrow() 
#there are 11930 instances of people taking less than 30 seconds

EMA %>% mutate(SurveyDuration = EndDate - StartDate) %>% 
  mutate(SurveyDuration_hours = as.numeric(SurveyDuration / 60 / 60)) %>% 
  arrange(-SurveyDuration) %>% 
  select(SurveyDuration, SurveyDuration_hours, SubID, everything()) %>% 
  filter(SurveyDuration < 15) %>% nrow()
#769 instances that were less than 15 seconds

#how many of these were from the same people?
EMA %>% mutate(SurveyDuration = EndDate - StartDate) %>% 
  mutate(SurveyDuration_hours = as.numeric(SurveyDuration / 60 / 60)) %>% 
  arrange(-SurveyDuration) %>% 
  select(SurveyDuration, SurveyDuration_hours, SubID, everything()) %>% 
  filter(SurveyDuration < 15) %>% group_by(SubID) %>% 
  summarize(N = n()) %>% arrange(-N)
#some participants regularly spent less than 15 seconds on surveys...

#what is the average survey duration?
EMA %>% mutate(SurveyDuration = EndDate - StartDate) %>% 
  summarize(DurationMean = mean(SurveyDuration),
            DuraitonSD = sd(SurveyDuration),
            DurationMin = min(SurveyDuration),
            DurationMax = max(SurveyDuration),
            N = n())
#92 seconds (min 9 max 251944)

###Looking at missing responses
# IN ORDER TO FIGURE OUT MISSING RESPONSES / HOW PEOPLE RESPONDED TO TRIGGERS, 
# NEED TO KNOW WHEN THEY WERE SUPPOSED TO RESPOND


# Looking at the Y dataset ------------------------------------------------------------------

#Note: all data included are from the day before the day they were recorded as drinking.
# (not much here)

# Comparing EMA lapses and the Y lapses

EMA %>% filter(EMA_1 == 2) %>% nrow()
#there are 1106 lapses recorded in the EMA dataset

Y %>% filter(Lapse == 1) %>% nrow()
#there are 793 in Y

#Looking at lapses per day

#in EMA 
EMA %>% group_by(SubID, Day) %>% 
  filter(EMA_1 == 2) %>% 
  summarize(N = n()) %>% .$N %>% table()
#The majority have 1 lapse per day
#Some people are listing more than 1 (up to 7!)

#in Y
Y$Day <- Y$UTC %>% #converting into human readable form
  as.numeric %>% 
  as.POSIXct(tz='America/Chicago', origin="1970-01-01")
#note, the time has been removed

Y %>% group_by(SubID, Day) %>% 
  filter(Lapse == 1) %>% 
  summarize(N = n()) %>% .$N %>% table()
#now everyone has 1 lapse per day

#how many lapses per person?
Y %>% group_by(SubID) %>% 
  filter(Lapse == 1) %>% 
  summarize(N = n()) %>% 
  summarize(mean_nlapses = mean(N),
            max_nlapses = max(N),
            min_nlapses = min(N))

#hist
Y %>% group_by(SubID) %>% 
  filter(Lapse == 1) %>% 
  summarize(N = n()) %>% 
  .$N %>% hist(breaks = 10)

#Table
Y %>% group_by(SubID) %>% 
  filter(Lapse == 1) %>% 
  summarize(N = n()) %>% 
  .$N %>% table()
#mode is 1 and 2


# Looking at the X dataset ------------------------------------------------------------------

#we should make a codebook, the features are not self-explanatory
#X and Y datasets have the same number of rows
nrow(X) == nrow(Y)

#subsetting just to EMA and General
X_EMA <- X %>% as_tibble() %>% 
  select(SubID, UTC, contains("EMA"), contains("Gen")) 

#converting dates
#these have alreay been standardized to just the day
X_EMA$Date <- X_EMA$UTC %>% #converting into human readable form
  as.numeric %>% 
  as.POSIXct(tz='America/Chicago', origin="1970-01-01") %>% 
  substr(1, 10)

#looks normal
nrow(X_EMA)
ncol(X_EMA)
#there are 206 features
#and 9108 observations

#checking the number that are exact duplicates ()
duplicated(X_EMA) %>% table()
#there are zero

#check that the values here are all possible
#EMA_2 as an example
X_EMA %>% select(contains("EMA_2")) %>% varDescribe()
#two are extremely kurtotic
#MeanWeekvB
X_EMA %>% select(contains("MeanWeekvB")) %>% varDescribe()
#Mean3DayvB
X_EMA %>% select(contains("Mean3DayvB")) %>% varDescribe()
#MeanDayvB
X_EMA %>% select(contains("MeanDayvB")) %>% varDescribe()
#these look correct

# Looking at the ID dataset ------------------------------------------------------------------

nrow(ID)
#127 rows

ID %>% group_by(SubID) %>% 
  summarize(N = n()) %>% nrow()
#127 people

#vs for EMA
EMA %>% group_by(SubID) %>% 
  summarize(N = n()) %>% nrow()
#also 127

#looking at demographics
ID %>% select(contains("dem")) %>% varDescribe()
#some missing values on Dem2_4, 6, 8
#also where are Dem 2_1, 3, 5, 7?
#dem_7 must be income
hist(ID$DEM_7)

#looking at alcohol use history
ID %>% select(contains("auh")) %>% varDescribe()
#no missing 

#looking at assist
ID %>% select(contains("assist")) %>% varDescribe()
#no missing 
#some insane kurtosis and skew for the self-efficacy

#DSM
ID %>% .$DSM5_Tot %>% hist()
#left skew but not too bad

#DTS
ID %>% select(contains("dts")) %>% varDescribe()
#looks good

#FAD
ID %>% select(contains("fad")) %>% varDescribe()
#why are the maxes here all over the place? what was the scale?
#otherwise no problems

#ASI
ID %>% select(contains("asi")) %>% varDescribe()
#looks good

#SCL
ID %>% select(contains("scl")) %>% varDescribe()
#mostly normal excelt Pho and Ang are kurtotic
#most people scored very low on both

#MPS
ID %>% select(contains("mps")) %>% varDescribe()
#looks normal

#else
ID %>% select(-contains("mps"), -contains("scl"), -contains("asi"),
              -contains("fad"), -contains("dts"), -contains("assist"),
              -contains("auh"), -contains("dsm"), -contains("dem"),
              -contains("sub"), -contains("utc")) %>% varDescribe()
#all good


#Other thoughts on features to add (to gen category)
# time spent on surveys just before the lapse
# OK to include in our model are those that could be derrived passively 

# age is ok
# gender is ok
# trait level time 0 is ok as just intercept
# trait level time 0 is a source of interaction, too
# ID is the id labels, includes things like the DSM-5 checklist, demographics
# SR is self-report varying

# Clinical story is to build a model to predict from self-report
# Theory EMA story is to expand on static person-level variables
# by adding time-varying components
# and then interactions

# Next ... we will do a nested CV just using EMA to get a starting point. Maybe use lasso to figure out 
# what features are best and most useful. To help us with the theory-driven questions.
# would like to interact features -- maybe not all features, but some subset (e.g.,
# holiday, person-level variables, ...)

#thinking about pre-registering this project
# there is 1 way that we can still be cherry-picking:
# specifically, any decisions made outside of CV
# is still subject to cherry picking
# specifically the DV and inclusion/exclusion
# and inclusion/exclusion of the EMA data (e.g., because of odd behavior)
# including for sparseness / missing data within the EMA
# goals of this would be:
# 1. to do analyses we are comfortable with
# 2. to do analyses that others are comfortable with / confident in?

#To - do:
# check for missingness in the EMA ...
# do a little lit search using risk 2 citations as starting point
