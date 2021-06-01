# mak_iMazing.R
# iMazing file renaming code
# This file loops through all files in the /RawData folder and looks for newly-downloaded audio files, then renames them appropriately.
# Renames the audio file once in the participant's raw data folder.  
# Labels each individual file with SubID_year of recording_date of recording_time of recording (military time) 
# Ex: 002_2016_1128_0730 
# 2019_0219 SES

##INSTRUCTIONS:------------------------
# Download audio files per protocol
# Save in individual subjects' Audio folders
# Convert to .mp3 format, if necessary, per protocol
# Run this script
# If a file is not correctly renamed, please send a message to Susan 

##Libraries/Sources------------------------
#install.packages('tidyverse') #uncomment to install for the first time
library(tidyverse) #load for stringr functions

Subs = c(245, 255, 259, 262, 263, 264, 265, 268, 269, 270)

for (aSub in Subs)  {
  InPath =  file.path('P:/StudyData/RISK/RawData',aSub)
  ##Get list of un-renamed mp3 files in ANY subject folder under \RawData------------------------
  audioFiles = list.files(path = InPath, pattern='.*Voice.*.mp3|.*Record.*.mp3|.*Audio.*.mp3|.*recording.*.mp3', recursive = TRUE) #only finds un-renamed files
  #View(audioFiles) #uncomment if you wish to see a list of un-renamed files
  print(aSub)
  print(audioFiles)
  ##loop through files------------------------
  for (aFile in audioFiles) {
  
    #Get current SubID 
    SubID = aSub
    print(SubID)
    #delete the non-date parts of the current filename
    afName = str_replace(aFile,'Audio/','') #drop the subfolder names
    afName = str_trunc(afName, 16, side = 'right',ellipsis='') #drop all but the first 16 digits of raw filename (the date we want in the new filename)
    
    #convert to correctly formatted character vector
    afName = str_replace(afName,'-','_') #replace the first hypen
    afName = str_replace(afName,'-','')  #remove the second hyphen
    afName = str_replace(afName,' ','_') #replace the first space
    afName = str_replace_all(afName,' ','') #remove the second space
    afName = str_c(SubID, afName, sep='_') #add subID
    afName = str_c(afName, 'mp3', sep='.') #add file extension
    
    #rename the current file
    file.rename(file.path(InPath,aFile), file.path(InPath, 'Audio', afName))
    print(afName)
  }
}

