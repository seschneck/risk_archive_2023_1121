#This script reports on the status of all devices currently
#registered with FollowMee

source('P:/StudyData/RISK/Analysis/Dev/Scripts/fun_GPS.R')
dDevices = apiFollowMeeDevices()
dDevices$Group=NULL
View(dDevices)
