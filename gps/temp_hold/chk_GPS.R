library(tidyverse)
RiskDataPath = 'Risk/Data'

X = read_rds(file.path(RiskDataPath, 'GPS.rds'))
glimpse(X)

#check missing data
summarise_all(X, funs(sum(is.na(.))))

View(filter(X, is.na(NextTime)))  #check specific variable


#check mins & maxs
summarize_at(X, vars(NextTime, NextDist), funs(min), na.rm=TRUE)
summarize_at(X, vars(NextTime, NextDist), funs(max), na.rm=TRUE)

#Checking NextTime
dNT = X %>%
      select(NextTime) %>%
      filter(NextTime > 24*60)
qplot(dNT$NextTime, bins = 1000)

X %>% filter(NextTime> 24*60) %>%
      count(PlaceType)
Days = difftime(first(Time), last(Time), units=days)
#CONSIDER BRINGING TO FENCE (24*60) ALL BIG NEXTTIMEs


#Checking number of GPS points
dPTS =  X %>% 
        group_by(SubID) %>%
        arrange(Time) %>%
        summarise(N=n(), FirstDay=first(Time), LastDay=last(Time)) %>%
        mutate(Days= as.numeric(difftime(LastDay,FirstDay, units='days')), PPD = N/Days) %>%
        arrange(PPD)
qplot(dPTS$PPD)
