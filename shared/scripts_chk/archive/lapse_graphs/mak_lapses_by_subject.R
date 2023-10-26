#Plot lapses for each subject, colored by intent
#Red = not persuing abstinence, Yellow= uncertain, green = YES, black = no lable
#ema_1_5 only asked when they lapsed

library(tidyverse)
#match on UTCs to get abstinence labels
lapse_day <- read_rds("P:/StudyData/RISK/analysis/shared/data/ds_lapses.rds")
ema <- read_rds("P:/StudyData/RISK/analysis/shared/data/ds_ema.rds")

#Convert date of lapse to utc
lapse_day <- lapse_day %>%
  mutate(date = as.POSIXct(utc, tz='America/Chicago', origin="1970-01-01")) 
#Convert to date format so we can merge with ema file  
lapse_day <- lapse_day %>%
  mutate(date = as.Date(date, tz = "America/Chicago"))

#Times in lapse_day do not match reported lapse times in ema
#Pull out just the date of the reported lapse to match on
#Subset down to subid, date of lapse, and reponse to abstinece q (ema_1_5)

ema <- ema %>% 
mutate(date = as.Date(ema_1_1, tz = "America/Chicago")) %>% 
  select(c("subid","date","ema_1_5"))


lapses <- left_join(lapse_day, ema, by = c("subid","date"))

#Make abstince a factor for easy graphing. Include missing as a level so we can assign a color
lapses <- lapses %>% 
   mutate(ema_1_5 = as.factor(case_when(
    ema_1_5 == 1 ~ "No",
    ema_1_5 == 2 ~ "Uncertain",
    ema_1_5 == 3 ~ "Yes",
    is.na(ema_1_5) ~ "Missing"))) 

#If someone reported multiple lapese, but they didn't change their abstinence rating, we don't care and can remove dulplicated rows
lapses <- distinct(lapses)

#Lets look at all cases where someone reported multiple lapses but their abstince rating was not the same
mult_lapses <- lapses %>% 
  group_by(subid) %>% 
  filter(duplicated(date, fromLast = TRUE)==TRUE| 
           duplicated(date)==TRUE)

problem_ids <- unique(mult_lapses$subid)

#In these cases, lets be safe and take their lowest reported abstinence rating
#Loop through problem ids, select lowest abstinence rating, and replace value in lapses

#if someone reported multiple lapses on the same day, take their lowest abstinence (No < Uncertain < yes)
for(i in problem_ids){
  sub <- lapses %>% 
    filter(subid==i)
  
    dup_dates <- unique(sub$date[duplicated(sub$date)==TRUE])
    
    for(a_date in dup_dates){
      dup_at_date <- sub %>% 
        filter(date == a_date)
      if(any(dup_at_date$ema_1_5=="No")){
        lowest_label="No"
      }else{lowest_label="Uncertain"}
      lapses$ema_1_5[lapses$subid==i & lapses$date==a_date] = lowest_label
    }
}

#Remove duplicates again
lapses <- distinct(lapses)

#Check to make sure we have the same number of rows as the original lapses file
nrow(lapses) == nrow(lapse_day)

#Yay! Now we can finally make the graphs
#get a list of all subids to loop through
subids <- unique(lapses$subid)

#save to pdf in loop
pdf("P:/StudyData/RISK/analysis/shared/archive/lapse_graphs/lapses_by_subject.pdf")
for (i in subids){
  sub <- lapses %>% 
    filter(subid==i)
  
  plot = ggplot(sub, aes(x = date, y = lapse, color = ema_1_5)) +
    geom_point() +
   labs(color = "Pursuing abstinence?")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_color_manual(values = c("No" = "red2","Uncertain" = "gold2",
                                  "Yes" = "forestgreen", "Missing" = "black"))+
    ggtitle(str_c("Sub ID: ",i))+
    theme(plot.title = element_text(face = "bold",hjust = 0.5, vjust = .3))+
    scale_y_discrete(limits=c(0, 1))
 
 print(plot)
}
dev.off()
