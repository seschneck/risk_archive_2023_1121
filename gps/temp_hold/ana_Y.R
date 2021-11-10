#Script to explore the Y labels of participants
library(stringr)
library(lmSupport)
library(gdata)
library(tidyverse)

DataPath = 'P:/StudyData/RISK/Analysis/GPS/Data'

#Load Y data
#dYWeek = readNo33GDS(file.path(DataPath, 'YWeek.rds'))
dYDay = read_rds(file.path(DataPath, 'YDay.rds'))

#Add color column for future intent
for (l in 1:length(dYDay$SubID)){
  if (is.na(dYDay$FutureIntent[l])){dYDay$Color[l] = "black"
  }else if (dYDay$FutureIntent[l] == 1){dYDay$Color[l] = "red"
  }else if (dYDay$FutureIntent[l] == 2){dYDay$Color[l] = "cornflowerblue"
  }else if (dYDay$FutureIntent[l] == 3) {dYDay$Color[l] = 'chartreuse3'}
}


#get some quick summary stats
sum(dYDay$Lapse) #793
nrow(dYDay) #9286
sum(dYDay$Lapse)/nrow(dYDay) #.085

#Subs are ordered by time as part of mak_Y

#get mean of lapse day labels by subject------------------------
dYLapse = tibble(SubID=unique(dYDay$SubID),TotalL = NA, Mean=NA, TotalObs=NA)
for(i in unique(dYDay$SubID)){
  dYi = dYDay[dYDay$SubID==i,]
  dYLapse$TotalL[dYLapse$SubID==i] = sum(dYi$Lapse)
  dYLapse$Mean[dYLapse$SubID==i] = sum(dYi$Lapse)/nrow(dYi)
  dYLapse$TotalObs[dYLapse$SubID==i] = nrow(dYi)
}

#histogram of the mean lapses day (lapse rate?) across subjects------------------
windows()
hist(dYLapse$Mean)
varDescribe(dYLapse$Mean)
#mean lapse rate of .09, max .95

#How many people never lapsed?
length(dYLapse$SubID[dYLapse$TotalL==0])
#40 out of 108 = .37

#People lapsing above 33% of the time
SubIDs = dYLapse$SubID[dYLapse$Mean>.33] 
#N=7; Sub IDs 37, 48, 58, 86, 98, 104, 128
#Everyone except 37 is above 40%

#Ends in lapse (more at the end): 48, 58, 
#Even lapses throughout: 86, 104(!), 128, 37
#More lapses at beginning: 98

#Remove if above 33% LR--------------------------
dNo33 = dYLapse
for (aSub in SubIDs){
    print(aSub) #Who is removed
    dNo33 = dNo33[!(dNo33$SubID==aSub),]}
  
#How many lapse cases would we lose if we eliminated these people
sum(dNo33$TotalL)
sum(dNo33$TotalObs)
varDescribe(dNo33$Mean)
View(dNo33)
#We would drop to 486 lapses

#Remove if >33% LR UNLESS >90 green lapses--------------------------------
dNo33G = dYLapse
for (aSub in SubIDs){
  lapses = dYDay[dYDay$SubID==aSub & dYDay$Lapse==1,]
  #3 = green, certain abstinence
  if(length(lapses$FutureIntent[lapses$FutureIntent==3])/length(lapses$FutureIntent)<.9){
    print(str_c(aSub,': %Green = ',(round(length(lapses$FutureIntent[lapses$FutureIntent==3])/length(lapses$FutureIntent), digits = 2))))
    dNo33G = dNo33G[!(dNo33G$SubID==aSub),]}
  
}
#37: LR = .40, %green = .06
#48: LR = .45, %green = .88
#58: LR = .47, %green = 0
#86: LR = .43, %green = 0
#98: LR = .43, %green = .87
#104: LR = .95, %green = .05

#128 is kept: LR = .59, %green = .96

#How many lapse cases would we lose if we eliminated these people
sum(dNo33G$TotalL)
sum(dNo33G$TotalObs)
varDescribe(dNo33G$Mean)
View(dNo33G)
#We would drop to 512

#Remove if >25% LR UNLESS >90 green lapses--------------------------------
dNo25G = dYLapse

#People lapsing above 25% of the time
SubIDs = dYLapse$SubID[dYLapse$Mean>.25] 
#N=14; Sub IDs 34, 37, 43, 47, 48, 54, 56, 58, 86, 98, 104, 121, 128, 131

for (aSub in SubIDs){
  lapses = dYDay[dYDay$SubID==aSub & dYDay$Lapse==1,]
  #3 = green, certain abstinence
  if(length(lapses$FutureIntent[lapses$FutureIntent==3])/length(lapses$FutureIntent)<.9){
    print(str_c(aSub,': %Green = ',(round(length(lapses$FutureIntent[lapses$FutureIntent==3])/length(lapses$FutureIntent), digits = 2))))
    #Who is removed
    dNo25G = dNo25G[!(dNo25G$SubID==aSub),]}
  
}
#34: LR = .27, %green = .46
#37: LR = .40, %green = .06
#43: LR = .27, %green = .04
#47: LR = .29, %green = .02
#48: LR = .45, %green = .88
#54: LR = .30, %green = .33
#56: LR = .28, %green = 0
#58: LR = .47, %green = 0
#86: LR = .43, %green = 0
#98: LR = .43, %green = .87
#104: LR = .95, %green = .05
#121: LR = .30, %green = .07

#128 is kept: LR = .59, %green = .96
#131 is kept: LR = .28, %green = .96

#How many lapse cases would we lose if we eliminated these people
sum(dNo25G$TotalL)
sum(dNo25G$TotalObs)
varDescribe(dNo25G$Mean)
View(dNo25G)
#We would dNo25Gop to 512

#Plot people removed
pdf(file = file.path(DataPath, 'LapseLabels_byDay_No25G.pdf')) #make this as pdf
for(i in SubIDs){
  dYi = dYDay[dYDay$SubID==i,]
  plot(dYi$Lapse, main= str_c("SubID = ", i), col=dYi$Color,cex=.7, pch = 19)
}
dev.off()



#Remove lapses that are chains-----------------------------------------
dYDay$NewLapse3 = NA
for (aSub in unique(dYDay$SubID)){
  dSub = dYDay[dYDay$SubID == aSub,]
  dSub$NLapse3 = NA
  #First, if they don't have any lapses, we don't do anything
  if (any(dSub$Lapse==1) > 0){
    for (i in 1:length(dSub$Lapse)){
    #If its the first event
    #Keep label as is
    if (i == 1){dSub$NLapse3[i] = dSub$Lapse[i]
    
    }else{
      if (dSub$Lapse[(i-1)]==1){
        if(dSub$Lapse[i]==1){dSub$NLapse3[i] = NA}
        else{dSub$NLapse3[i] = dSub$Lapse[i]}
      }else{dSub$NLapse3[i] = dSub$Lapse[i]}
    }
    }
    
  }else{dSub$NLapse3[1:length(dSub$Lapse)] = dSub$Lapse[1:length(dSub$Lapse)]}
  
  #Add NewLapse to orig dataframe
  dYDay$NewLapse3[dYDay$SubID == aSub] = dSub$NLapse3 
}

#make plots for each subject of time series of lapse labels by day if you remove lapsechains-----------------------
#Add color column
for (l in 1:length(dYDay$SubID)){
  if (is.na(dYDay$FutureIntent[l])){dYDay$Color[l] = "black"
  }else if (dYDay$FutureIntent[l] == 1){dYDay$Color[l] = "red"
  }else if (dYDay$FutureIntent[l] == 2){dYDay$Color[l] = "cornflowerblue"
  }else if (dYDay$FutureIntent[l] == 3) {dYDay$Color[l] = 'chartreuse3'}
}

dYDay$NewLapse3[is.na(dYDay$NewLapse3)]= -1 #make NAs show on the plot
pdf(file = file.path(DataPath, 'LapseLabels_byDay_NoChains.pdf')) #make this as pdf
for(i in unique(dYDay$SubID)){
  dYi = dYDay[dYDay$SubID==i,]
  plot(dYi$NewLapse3, main= str_c("SubID = ", i), col=dYi$Color,cex=.7, pch = 19)
}
dev.off()
dYDay$NewLapse3[dYDay$NewLapse3==-1]= NA

#How many lapses are left
table(dYDay$NewLapse3)
#450 out of 766 -- 58.7%

#7249 observations left, from 8015 -- (90.4% of cases remain)

#Mean lapse rates with 7 Day Abstinece only lapses-----------------------------------

#get mean of lapse day labels by subject, add to prev dYLapse
for(i in unique(dYDay$SubID)){
  dYLapse$TotalLCh[dYLapse$SubID==i] = sum(dYDay$NewLapse3[dYDay$SubID==i], na.rm = TRUE)
  dYnoNA = dYDay[!is.na(dYDay$NewLapse),]
  dYLapse$MeanCh[dYLapse$SubID==i] = dYLapse$TotalLCh[dYLapse$SubID==i]/length(dYnoNA$NewLapse[dYnoNA$SubID==i])
}


#histogram of the mean lapses day (lapse rate?) across subjects
windows()
hist(dYLapse$MeanCh)
varDescribe(dYLapse$MeanCh) #mean lapse rate = .07%
table(dYLapse$MeanCh)




#What if we required them to be 7 days abstinent-------------------------------------------------
#Make 7 days after lsapse NA

#Now lets make their data NA after a lapse until they are 7 days  abstinent
dYDay$NewLapse=NA
for (aSub in unique(dYDay$SubID)){
  dSub = dYDay[dYDay$SubID == aSub,]
  dSub$NLapse = NA
  #First, if they don't have any lapses, we don't do anything
  if (any(dSub$Lapse==1) > 0){
    for (i in 1:length(dSub$Lapse)){
      #Need to check multiple time periods
      #If its the first event
      #Keep label as is
      if (i == 1){dSub$NLapse[i] = dSub$Lapse[i]
      
      #If its between day 1 and 8  
      }else if (i < 8){
        #Remove lapse label if lapsed between 1 and now
        if (1 %in% dSub$Lapse[1:(i-1)]){
          dSub$NLapse[i] = NA
          }else{dSub$NLapse[i] = dSub$Lapse[i]}
        
        #If its past day 8 (we can count back 7 days)
      }else{
        #Remove lapse label if lapsed in past 7 days, only resume if 7 days abstinent
        if (1 %in% dSub$Lapse[(i-7):(i-1)]){
        dSub$NLapse[i] = NA
        }else{dSub$NLapse[i] = dSub$Lapse[i]}
      }
    }
    #If they never lapsed, keep the whole the same 
  }else{dSub$NLapse[1:length(dSub$Lapse)] = dSub$Lapse[1:length(dSub$Lapse)]}
  
  #Add NewLapse to orig dataframe
  dYDay$NewLapse[dYDay$SubID == aSub] = dSub$NLapse 
}

#make plots for each subject of time series of lapse labels by day if make 7 days post lapse NA-----------------------
#Add color column
for (l in 1:length(dYDay$SubID)){
  if (is.na(dYDay$FutureIntent[l])){dYDay$Color[l] = "black"
  }else if (dYDay$FutureIntent[l] == 1){dYDay$Color[l] = "red"
  }else if (dYDay$FutureIntent[l] == 2){dYDay$Color[l] = "cornflowerblue"
  }else if (dYDay$FutureIntent[l] == 3) {dYDay$Color[l] = 'chartreuse3'}
}

dYDay$NewLapse[is.na(dYDay$NewLapse)]= -1 #make NAs show on the plot
pdf(file = file.path(DataPath, 'LapseLabels_byDay_7DayAb.pdf')) #make this as pdf
for(i in unique(dYDay$SubID)){
  dYi = dYDay[dYDay$SubID==i,]
  plot(dYi$NewLapse, main= str_c("SubID = ", i), col=dYi$Color,cex=.7, pch = 19)
}
dev.off()
dYDay$NewLapse[dYDay$NewLapse==-1]= NA


#NOTE, subID 25 has a black lapse around 20..thats why there are more than 7 blacks in a row


#How many lapses are left
table(dYDay$NewLapse)
#144 out of 766 -- 18.7%

#5714 observations left, from 8015 -- (71% of cases remain)

#Mean lapse rates with 7 Day Abstinece only lapses-----------------------------------

#get mean of lapse day labels by subject, add to prev dYLapse
for(i in unique(dYDay$SubID)){
  dYLapse$TotalL7A[dYLapse$SubID==i] = sum(dYDay$NewLapse[dYDay$SubID==i], na.rm = TRUE)
  dYnoNA = dYDay[!is.na(dYDay$NewLapse),]
  dYLapse$Mean7A[dYLapse$SubID==i] = dYLapse$TotalL7A[dYLapse$SubID==i]/length(dYnoNA$NewLapse[dYnoNA$SubID==i])
}


#histogram of the mean lapses day (lapse rate?) across subjects
windows()
hist(dYLapse$Mean7A)
varDescribe(dYLapse$Mean7A) #mean lapse rate = 11%
table(dYLapse$Mean7A)
#a few people their only valid observation is a lapse.

#Lapses and EMA-------------------------------------------------------------------------------------------------
#EMA graphs with lapses colored by Future Intent

#check how many lapses we lose if we require a green or 7 days abstinence after red lapse
#Add color column
for (l in 1:length(dYDay$SubID)){
if (is.na(dYDay$FutureIntent[l])){dYDay$Color[l] = "black"
}else if (dYDay$FutureIntent[l] == 1){dYDay$Color[l] = "red"
}else if (dYDay$FutureIntent[l] == 2){dYDay$Color[l] = "cornflowerblue"
}else if (dYDay$FutureIntent[l] == 3) {dYDay$Color[l] = 'chartreuse3'}
}

pdf(file = file.path(DataPath, 'LapseLabels_byDay_FutureIntent.pdf')) #make this as pdf
for(i in unique(dYDay$SubID)){
  dYi = dYDay[dYDay$SubID==i,]
  plot(dYi$Lapse, main= str_c("SubID = ", i), col=dYi$Color,cex=.7, pch = 19)
}
dev.off()
table(dYDay$FutureIntent)
#35 NO, 345 uncertain, 384 YES

#Who said NO ever
unique(dYDay$SubID[which((dYDay$FutureIntent==1))])
#11: Has 4 Nos, 5 uncertains, longest period of abstinence was after a No..all nos at beginning
#16, has 2 NOs, 1 U, 8 yes. Nos followed by 2nd longest period of abstinence (about 15 days)
#43 has a lot of Nos and uncertains, only 1 green. Nos are clustered at the end with highest lapse rate. This person, NO was a good indicator
#47 2 nos, 6 U, 2 yes. Nos are spread out at the end, highest lapse period at beginning with Us
#56 4 nos, many us. Nos seem evenly spaced and do not predict more lapses
#58 1 No in the middle of a string of Us, second half of study
#86 9 Nos, many Us. First lapse was a no. Nos seem to be connected to multiple lapses -- from prev description (not included rn)
#93 1 No, 4 U, 2 yes. Last point was a no followed by 7ish days of abstinence



#Now lets make their data NA after a red lapse until they are 7 days  abstinent or have a green lapse
dYDay$NewLapse2=NA
for (aSub in unique(dYDay$SubID)){
  dSub = dYDay[dYDay$SubID == aSub,]
  dSub$NLapse2 = NA
  #First, if they don't have any red lapses, we don't do anything
  if (any(dSub$Color == 'red') > 0){
    for (i in 1:length(dSub$Lapse)){
      #Need to check multiple time periods
      #If its the first event
      #Keep label as is
      if (i == 1){dSub$NLapse2[i] = dSub$Lapse[i]
        
        #If its between day 1 and 8  
      }else if (i < 8){
        #Remove lapse label if red lapsed between 1 and now
        if ('red' %in% dSub$Color[1:(i-1)]){
          if ('chartreuse3' %in% dSub$Color[1:(i-1)]){
            Space =  dSub[1:(i-1),]
            Reds = which(Space$Color=='red')
            Greens = which(Space$Color=='chartreuse3')
            if((last(Greens))>(last(Reds))){
              dSub$NLapse2[i] = dSub$Lapse[i]
            }else{dSub$NLapse2[i] = NA}
          }else {dSub$NLapse2[i] = NA}
        }else if (is.na(dSub$NLapse2[(i-1)])){
          Space = dSub[1:(i-1),]
          Lapses = which(Space$Lapse == 1)
            if (length(Lapses)==0){dSub$NLapse2[i]=dSub$Lapse[i]
            }else if (Space$Color[last(Lapses)] == "chartreuse3"){
              dSub$NLapse2[i]== dSub$Lapse[i]
            }else{dSub$NLapse2[i]=NA}
          }else{dSub$NLapse2[i] = dSub$Lapse[i]}
        
        #If its past day 8 (we can count back 7 days)
      }else{
        #Remove lapse label if lapsed in past 7 days, only resume if green is more recent than red or 7 days abstinent
        
        if ('red' %in% dSub$Color[(i-7):(i-1)]){
          if ('chartreuse3' %in% dSub$Color[(i-7):(i-1)]){
            Space =  dSub[(i-7):(i-1),]
            Reds = which(Space$Color=='red')
            Greens = which(Space$Color=='chartreuse3')
            if((last(Greens))>(last(Reds))){
              dSub$NLapse2[i] = dSub$Lapse[i]
              }else{dSub$NLapse2[i] = NA}
            }else {dSub$NLapse2[i] = NA}
        }else if (is.na(dSub$NLapse2[(i-1)])){
          Space = dSub[(i-7):(i-1),]
          Lapses = which(Space$Lapse == 1)
          if (length(Lapses)==0){dSub$NLapse2[i]=dSub$Lapse[i]
          }else if (Space$Color[last(Lapses)] == "chartreuse3"){
            dSub$NLapse2[i]== dSub$Lapse[i]
          }else{dSub$NLapse2[i]=NA}
        }else{dSub$NLapse2[i] = dSub$Lapse[i]}
        }
    }
    #If they never red lapsed, keep the whole the same 
  }else{dSub$NLapse2[1:length(dSub$Lapse)] = dSub$Lapse[1:length(dSub$Lapse)]}
  
  #Add NewLapse to orig dataframe
  dYDay$NewLapse2[dYDay$SubID == aSub] = dSub$NLapse2 
}
#Lapses left with removing after red lapses---------------------------------
#How many lapses are left
table(dYDay$NewLapse2)
#673 out of 766 -- 87%

#7758 observations left, from 8015 -- (96% of cases remain)

#Graph lapses after removing after red
dYDay$NewLapse2[is.na(dYDay$NewLapse2)]= -1 #make NAs show on the plot
pdf(file = file.path(DataPath, 'LapseLabels_byDay_FutureIntent_RemoveReds.pdf')) #make this as pdf
for(i in unique(dYDay$SubID)){
  dYi = dYDay[dYDay$SubID==i,]
  plot(dYi$NewLapse2, main= str_c("SubID = ", i), col=dYi$Color,cex=.7, pch = 19)
}
dev.off()
#bring back NAs
dYDay$NewLapse2[dYDay$NewLapse2==-1]= NA
#Mean lapse rates with removing after red lapses-----------------------------------

#get mean of lapse day labels by subject, add to prev dYLapse
for(i in unique(dYDay$SubID)){
  dYLapse$TotalLRA[dYLapse$SubID==i] = sum(dYDay$NewLapse2[dYDay$SubID==i], na.rm = TRUE)
  dYnoNA = dYDay[!is.na(dYDay$NewLapse2),]
  dYLapse$MeanRA[dYLapse$SubID==i] = dYLapse$TotalLRA[dYLapse$SubID==i]/length(dYnoNA$NewLapse[dYnoNA$SubID==i])
}


#histogram of the mean lapses day (lapse rate?) across subjects
windows()
hist(dYLapse$MeanRA)
varDescribe(dYLapse$MeanRA) #mean lapse rate = 10%
table(dYLapse$MeanRA)
#0 - 100 LR

#Remove after red or blue lapses--------------------------------
#Now lets make their data NA after a red or blue lapse until they are 7 days  abstinent or have a green lapse
dYDay$NewLapse3=NA
for (aSub in unique(dYDay$SubID)){
  dSub = dYDay[dYDay$SubID == aSub,]
  dSub$NLapse3 = NA
  #First, if they don't have any red or blue lapses, we don't do anything
  if (any(dSub$Color == 'red' | dSub$Color=='cornflowerblue') > 0){
    for (i in 1:length(dSub$Lapse)){
      #Need to check multiple time periods
      #If its the first event
      #Keep label as is
      if (i == 1){dSub$NLapse3[i] = dSub$Lapse[i]
      
      #If its between day 1 and 8  
      }else if (i < 8){
        #Remove lapse label if red lapsed between 1 and now
        if ('red' %in% dSub$Color[1:(i-1)] | 'cornflowerblue' %in% dSub$Color[1:(i-1)]){
          if ('chartreuse3' %in% dSub$Color[1:(i-1)]){
            Space =  dSub[1:(i-1),]
            Outs = which(Space$Color=='red'|Space$Color=='cornflowerblue')
            Greens = which(Space$Color=='chartreuse3')
            if((last(Greens))>(last(Outs))){
              dSub$NLapse3[i] = dSub$Lapse[i]
            }else{dSub$NLapse3[i] = NA}
          }else {dSub$NLapse3[i] = NA}
        }else if (is.na(dSub$NLapse3[(i-1)])){
          Space = dSub[1:(i-1),]
          Lapses = which(Space$Lapse == 1)
          if (length(Lapses)==0){dSub$NLapse3[i]=dSub$Lapse[i]
          }else if (Space$Color[last(Lapses)] == "chartreuse3"){
            dSub$NLapse3[i]== dSub$Lapse[i]
          }else{dSub$NLapse3[i]=NA}
        }else{dSub$NLapse3[i] = dSub$Lapse[i]}
        
        #If its past day 8 (we can count back 7 days)
      }else{
        #Remove lapse label if lapsed in past 7 days, only resume if green is more recent than red or 7 days abstinent
        
        if ('red' %in% dSub$Color[(i-7):(i-1)] |'cornflowerblue' %in% dSub$Color[(i-7):(i-1)]){
          if ('chartreuse3' %in% dSub$Color[(i-7):(i-1)]){
            Space =  dSub[(i-7):(i-1),]
            Outs = which(Space$Color=='red'|Space$Color=='cornflowerblue')
            Greens = which(Space$Color=='chartreuse3')
            if((last(Greens))>(last(Outs))){
              dSub$NLapse3[i] = dSub$Lapse[i]
            }else{dSub$NLapse3[i] = NA}
          }else {dSub$NLapse3[i] = NA}
        # }else if (is.na(dSub$NLapse3[(i-1)])){
        #   Space = dSub[(i-7):(i-1),]
        #   Lapses = which(Space$Lapse == 1)
        #   if (length(Lapses)==0){dSub$NLapse3[i]=dSub$Lapse[i]
        #   }else if (Space$Color[last(Lapses)] == 'chartreuse3'){
        #     dSub$NLapse3[i]== dSub$Lapse[i]
        #   }else{dSub$NLapse3[i]=NA}
        }else{dSub$NLapse3[i] = dSub$Lapse[i]}
      }
    }
    #If they never red or blue lapsed, keep the whole the same 
  }else{dSub$NLapse3[1:length(dSub$Lapse)] = dSub$Lapse[1:length(dSub$Lapse)]}
  
  #Add NewLapse to orig dataframe
  dYDay$NewLapse3[dYDay$SubID == aSub] = dSub$NLapse3 
}
#Lapses left with removing after red lapses---------------------------------
#How many lapses are left
table(dYDay$NewLapse3)
#432 out of 766 -- 56.4%

#7092 observations left, from 8015 -- (88% of cases remain)

#Graph lapses after removing after red and blue
dYDay$NewLapse3[is.na(dYDay$NewLapse3)]= -1 #make NAs show on the plot
pdf(file = file.path(DataPath, 'LapseLabels_byDay_FutureIntent_RemoveRedsBlues.pdf')) #make this as pdf
for(i in unique(dYDay$SubID)){
  dYi = dYDay[dYDay$SubID==i,]
  plot(dYi$NewLapse3, main= str_c("SubID = ", i), col=dYi$Color,cex=.7, pch = 19)
}
dev.off()

#People with a lot of green lapses (48, 98, 128, 131) -- are they different from people with many other lapses? 
#did a good job eliminating problem people (58, 104).

#bring back NAs
dYDay$NewLapse3[dYDay$NewLapse3==-1]= NA
#Mean lapse rates with removing after red lapses-----------------------------------

#get mean of lapse day labels by subject, add to prev dYLapse
for(i in unique(dYDay$SubID)){
  dYLapse$TotalRB[dYLapse$SubID==i] = sum(dYDay$NewLapse3[dYDay$SubID==i], na.rm = TRUE)
  dYnoNA = dYDay[!is.na(dYDay$NewLapse3),]
  dYLapse$MeanRB[dYLapse$SubID==i] = dYLapse$TotalRB[dYLapse$SubID==i]/length(dYnoNA$NewLapse[dYnoNA$SubID==i])
}


#histogram of the mean lapses day (lapse rate?) across subjects
windows()
hist(dYLapse$MeanRA)
varDescribe(dYLapse$MeanRA) #mean lapse rate = 10%
table(dYLapse$MeanRA)
#0 - 100% rates






#Histogram of total number of observations (just to make sure no one has too little data)------------
windows()
varDescribe(dYLapse$TotalObs)
hist(dYLapse$TotalObs) 

#lowest bracket
dYLapse$SubID[dYLapse$TotalObs<40]
#Only FU 1 - complete: 47, 93
#FU 2 - complete: 87
#FU 1 - ongoing: 158


dYLapse$SubID[dYLapse$TotalObs<71]
#Only FU 1 - complete: 47, 59,93, 82
#FU 2 - complete: 87, 76, 78, 87, 104, 105, 85
#FU 1 - ongoing: 158, 161, 162
#FU 2 - ongoing: 156

dYLapse$SubID[dYLapse$TotalObs<78]
#Only FU 1 - complete: 29, 47, 59,93, 82
#only FU 2 - complete: 87, 76, 78, 87, 104, 105, 85


#Complete with all??: 16 (only 75 obs?) -- quit early for holidays


#Look at their predicitions of their ability to stay abstinent-------------------------------
dP =  readNo33GDS(file.path(DataPath, 'YDay_withPreds.rds'))

#Change days that don't have predictions to the next non NA prediction
SubIDs = unique(dP$SubID)

for (aSub in SubIDs){
  dSub = dP[dP$SubID==aSub,]
  dSub$LikAlc = NA
  for (i in 1:length(dSub$SubID)){
    if (is.na(dSub$LikelyAlc[i])==TRUE){
      Space = dSub[i+1:length(dSub$SubID),]
        if (any(is.na(Space$LikelyAlc)==FALSE)){
        NextVal = first(which((is.na(Space$LikelyAlc)==FALSE)))
        dSub$LikAlc[i] = dSub$LikelyAlc[i + NextVal]
        }
    }else{dSub$LikAlc[i] = dSub$LikelyAlc[i]}
  }
  #shift all values so that the predictions are the first day after the lapse/non lapse
  shift =  c(NA, dSub$LikAlc[1:(length(dSub$LikAlc)-1)])
  dSub$LikAlc = shift
  dP$LikAlc[dP$SubID == aSub] = dSub$LikAlc
}



#mean preds for lapse/nolapse
#1=red, 2=blue, 3=green

dPreds = data.frame(SubID=unique(dP$SubID),NoLapse=NA, Lapse = NA,GreenL = NA, BlueL = NA, RedL = NA )
for(i in unique(dP$SubID)){
  dPi = dP[dP$SubID==i,]
  if (1 %in% dPi$Lapse){
  dPreds$Lapse[dPreds$SubID==i] = sum(dPi$LikelyAlc[dPi$Lapse==1 & (is.na(dPi$LikelyAlc)==FALSE)])/nrow(dPi[dPi$Lapse==1,])
  dPreds$NoLapse[dPreds$SubID==i] = sum(dPi$LikelyAlc[dPi$Lapse==0 & (is.na(dPi$LikelyAlc)==FALSE)])/nrow(dPi[dPi$Lapse==0,])
    if (3 %in% dPi$FutureIntent){
      dPreds$GreenL[dPreds$SubID==i] = sum(dPi$LikelyAlc[dPi$FutureIntent==3 & (is.na(dPi$FutureIntent)==FALSE) & (is.na(dPi$LikelyAlc)==FALSE)])/nrow(dPi[dPi$FutureIntent==3& (is.na(dPi$FutureIntent)==FALSE),])
      }else{dPreds$GreenL[dPreds$SubID==i] = NA}
    if (2 %in% dPi$FutureIntent){
      dPreds$BlueL[dPreds$SubID==i] = sum(dPi$LikelyAlc[dPi$FutureIntent==2 &  (is.na(dPi$FutureIntent)==FALSE) & (is.na(dPi$LikelyAlc)==FALSE) & (is.na(dPi$LikelyAlc)==FALSE)])/nrow(dPi[dPi$FutureIntent==2&  (is.na(dPi$FutureIntent)==FALSE),])
      }else{dPreds$BlueL[dPreds$SubID==i] = NA}
  if (1 %in% dPi$FutureIntent){
    dPreds$RedL[dPreds$SubID==i] = sum(dPi$LikelyAlc[dPi$FutureIntent==1  & (is.na(dPi$FutureIntent)==FALSE) & (is.na(dPi$LikelyAlc)==FALSE) & (is.na(dPi$LikelyAlc)==FALSE)])/nrow(dPi[dPi$FutureIntent==1 & (is.na(dPi$FutureIntent)==FALSE),])
    }else{dPreds$RedL[dPreds$SubID==i] = NA}
  
  }else{
    dPreds$Lapse[dPreds$SubID==i] = NA
    dPreds$GreenL[dPreds$SubID==i] = NA
    dPreds$BlueL[dPreds$SubID==i] = NA
    dPreds$RedL[dPreds$SubID==i] = NA
    dPreds$NoLapse[dPreds$SubID==i] = sum(dPi$LikelyAlc[dPi$Lapse==0 & (is.na(dPi$LikelyAlc)==FALSE)])/nrow(dPi[dPi$Lapse==0,])
  }

}

#Delete old var
dP$LikelyAlc = NULL
dP$LikelyRisky = NULL
dP$LikelyStress = NULL

#Overall means

#Non lapses
varDescribe(dPreds$NoLapse)
#mean 2.85, .38 - 10

#Lapses
varDescribe(dPreds$Lapse)
#Mean 3.43, 0 - 9.3

#Green Lapses
varDescribe(dPreds$GreenL)
#Mean 3.27, 0 - 8

#Blue Lapses
varDescribe(dPreds$BlueL)
#mean 4.01, 0 - 9.44

#Red Lapses
varDescribe(dPreds$RedL)
#mean 3.54 0 - 9.6


####### MAM measure of abstinence########
DatabasePath = 'P:/StudyData/RISK/Database'

dIntake = read_rds(file.path(DatabasePath,'Intake.rds'))
dFU= read_rds(file.path(DatabasePath,'Followup12.rds'))

#pad dYLapse SubIDs so we can match
dYLapse$SubID = varPadString(dYLapse$SubID, 3)
dYDay = as.tibble(dYDay)
dYDay$SubID = varPadString(dYDay$SubID, 3)

#Just need SubID, date, and score from intake
#Only want subIDs we have computed lapse rates for
dIntake = select(dIntake, c(SubID, UTC, MAM_22 ))
dIntake = filter(dIntake, SubID %in% dYLapse$SubID)

#Check to make sure dims line up
dim(dIntake)
dim(dYLapse)

#Check for duplicates in intake
dIntake$SubID[duplicated(dIntake$SubID)==TRUE]
#Two intake entries for 42
#First is 3/8/2018, Second is on 4/16/2018
View(filter(dYDay, SubID==42))
#First GPS is on 3/9/2018, last is 6/5/2018, so we will keep the first
dIntake = filter(dIntake, !(SubID=="042" & UTC==1523911892))

#Just need SubID, date, and score for FU visits
#Only want subIDs we have computed lapse rates for
dFU = select(dFU, c(SubID, UTC, MAM_22))
dFU = filter(dFU, SubID %in% dYLapse$SubID)

#Check to make sure dims line up
dim(dYLapse)
dim(dFU)
length(unique(dFU$SubID))
#which Subs dont we have FU for?
dYLapse$SubID[which(!dYLapse$SubID %in% dFU$SubID)]
#087 - discontinued
#105 discontinued but locations, vacations etc?
#173 discontinued but locations?
#177 - Should have completed FU1 on 3/21/19 --- did they miss? Or is database out of date?
#182 Should have completed FU1 on 3/27/19 --- did they miss? Or is database out of date?

#look at people with only 1 FU survey -- check to make sure it is for FU1
DupSubs = dFU$SubID[duplicated(dFU$SubID)==TRUE]
SingleSubs = (filter(dFU, !SubID %in% DupSubs))

for(aSub in SingleSubs$SubID) {
  message(str_c("Checking ", aSub))
  VisitDates = read_rds(file.path(DatabasePath, aSub,str_c(aSub,'_VisitDates.rds')))
  if(!is.na(VisitDates$Followup2)){
    if(VisitDates$Followup2<Sys.Date()){
      message(str_c(aSub, 'Missing FU Report: ',VisitDates$Followup2))
    }
  }
}
#All of these people either missed FU2 or it hasnt happened yet.

#Create a variable to note which are FU1 and which are FU2
dFU = mutate(dFU, Visit = 'FU1')

#for duplicates, change the later date to FU2
#Check if anyone is missing dates
dFU$SubID[which(is.na(dFU$UTC))]
#033 - not sure why this UTC is missing..it exists in qualtrics
#as 2/20/2018 9:28:00, UTC would be 1554486269
dFU$UTC[which(is.na(dFU$UTC)&dFU$SubID=='033')] = 1554486269

for (aSub in DupSubs){
  #compare UTC to visit dates for each person -- if its after FU 1 change to FU2
  message(str_c("Checking ", aSub))
  VisitDates = read_rds(file.path(DatabasePath, aSub,str_c(aSub,'_VisitDates.rds')))
  dFU$Visit[dFU$SubID==aSub & dFU$UTC >= as.numeric(as.POSIXct(VisitDates$Followup2))] = 'FU2'
}

#Check 54 has 2 FU2s --- 
dFU$Visit[dFU$UTC==1527790142 & dFU$SubID == '054'] = 'FU1'
#The UTC for FU1 is 5/31 1:09:02..the date for FU 2 is also 5/31..how?

#Add to dYLapse
dYLapse = mutate(dYLapse, IntakeMAM=NA,M1TotLapses = NA, M1GreenLapses=NA, M1BlueLapses=NA, M1RedLapses = NA, 
       FU1MAM=NA,M2TotLapses = NA, M2GreenLapses=NA, M2BlueLapses=NA, M2RedLapses = NA,  
       FU2MAM=NA, M3TotLapses = NA, M3GreenLapses=NA, M3BlueLapses=NA, M3RedLapses = NA )



for(aSub in unique(dFU$SubID)){
  dYLapse$IntakeMAM[dYLapse$SubID==aSub] = dIntake$MAM_22[dIntake$SubID==aSub]
  dYLapse$FU1MAM[dYLapse$SubID==aSub] = dFU$MAM_22[dFU$SubID==aSub & dFU$Visit == 'FU1']
  if ('FU2' %in% dFU$Visit[dFU$SubID==aSub]){
  dYLapse$FU2MAM[dYLapse$SubID==aSub] = dFU$MAM_22[dFU$SubID==aSub & dFU$Visit == 'FU2']}
}

for(i in unique(dYDay$SubID)){
  dYi = dYDay[dYDay$SubID==i,]
  VisitDates = read_rds(file.path(DatabasePath, i,str_c(i,'_VisitDates.rds')))
  month1 = filter(dYi, UTC < VisitDates$Followup1)
  month2 = filter(dYi, UTC < VisitDates$Followup2)
  month3 = filter(dYi, UTC < VisitDates$EndStudy)
  dYLapse$M1TotLapses[dYLapse$SubID==i] = sum(month1$Lapse)
  dYLapse$M2TotLapses[dYLapse$SubID==i] = sum(month2$Lapse)
  dYLapse$M3TotLapses[dYLapse$SubID==i] = sum(month3$Lapse)#Not working????

}
# Date = as_datetime(dYi$UTC)
# Date = with_tz(Date, 'America/Chicago')
# Date = as_date(Date)
# 1 = No, 2 = Uncertain, 3 = Yes
dim(dYLapse)
length(dYLapse$SubID)
dim(dIntake)
