#ana_x
#Look at patterns of missing data in X
#What combo of features and time periods have msot missing data
#Use the whole dataset

#cool missing data viz tools https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html

library(stringr)
library(lmSupport)
library(ggplot2)
library(naniar)
library(tabplot)
data(diamonds)


DataPath = 'P:/StudyData/RISK/Analysis/GPS/Data'

dX = readRDS(file.path(DataPath, 'XWeek.rds'))
View(dX)

#parse data into smaller chunks for viz
dayCols = names(dX[str_detect(names(dX),'_Week')])
dXWeek = dX[,dayCols]
#nevermind...still too big

#make plots for each subject of time series of lapse labels by day-----------------------
pdf(file = file.path(DataPath, 'VisMissPlots.pdf')) #make this as pdf
for(i in seq(from = 5, to = length(dX$SubID),by=5)){
  j = i - 4
  x = vis_miss(dX[,j:i])
  print(x)
}
dev.off()

windows()
gg_miss_var(dX[,10:15])

gg_miss_case(dXWeek)

windows()
 gg_miss_case_cumsum(dX)
 
 windows()
 tableplot(dXWeek)
 tableplot(diamonds)
 