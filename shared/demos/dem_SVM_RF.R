#Load ML libraries and  functions------------------------------
library(caret)
library(stringr)
library(pROC)
library(dplyr)
library(MLmetrics)
library(kernlab)
library(randomForest)

#Load dataframes for X and Y for day/week
DataPath = 'P:/StudyData/RISK/Analysis/Dev/Data'
ModelPath = 'P:/StudyData/RISK/Analysis/Dev/ModelResults'
dX = readRDS(file.path(DataPath,'XWeek.rds'))
dY = readRDS(file.path(DataPath,'YWeek.rds'))

# #Global pre-processing and checks------------------------------
# InitCols = ncol(dX)
# tNA=sapply(dX, function(x) sum(is.na(x)))
# tNA[tNA>0]

#Find and remove near zero variance 
iNZV = nearZeroVar(dX)
# length(iNZV)
dX[,iNZV]=NULL

# #Percent of total features removed
# (InitCols-ncol(dX))/InitCols


#Drop class of features if desired-----------------------------
TargetCols = !str_detect(names(dX), 'AUDIO')
dX = dX[,TargetCols]

#Restrict to features from ONE class if desired-----------------------------
TargetCols = str_detect(names(dX), 'WV')   #WV   or BOW   from package called stringr
dXWV = dX[,TargetCols]


#ML Setup------------------------------
SubID = dY$SubID
Y = dY$Lapse
Y = factor(Y, levels=c(0,1),labels=c('NoLapse', 'Lapse'))

X = dX
X$UTC = NULL

# #Consider dropping Samsung Galaxy 7-----------------------------
# BadGPS = c(2,19,21,30,39,42,51)
# iG = !(SubID %in% BadGPS)
# Y = Y[iG]
# SubID = SubID[iG]
# X = X[iG,]


# #Create limited second order X2--------------------------------
# IDVars = formulaSet(names(X), 'ID_', TRUE)
# OtherVars = formulaSet(names(X), 'ID_', FALSE)
# fOrder2 = formula(str_c('Y ~ 0 + ', IDVars, ' * ', OtherVars))
# 
# naset = options('na.action')
# options(na.action='na.pass')
# X2 = model.matrix(fOrder2,X)
# options(na.action=naset)

#Control structure------------------------------
#get grouped indices for 10-fold 3x repeated CV
set.seed(01271969)
folds1x <- groupKFold(SubID, k = 10) 
names(folds1x) = str_c(names(folds1x), "_1x")
folds2x <- groupKFold(SubID, k = 10) 
names(folds2x) = str_c(names(folds2x), "_2x")
folds3x <- groupKFold(SubID, k = 10) 
names(folds3x) = str_c(names(folds3x), "_3x")
folds = c(folds1x,folds2x,folds3x)

# #f1 summary function
# #https://en.wikipedia.org/wiki/F1_score
# f1 = function(data, lev = NULL, model = NULL) {
#   f1_val = F1_Score(y_pred = data$pred, y_true = data$obs, positive = "Lapse")
#   c(F1 = f1_val)
# }
  
trainCTRL = trainControl(
  method = "repeatedcv", number = 10, repeats=3,
  index=folds,
  classProbs = TRUE,
  savePredictions = TRUE,
  summaryFunction = twoClassSummary
)

#Train random forest------------------------------
rfGrid <- expand.grid(mtry=(1:10)*10)
set.seed(05222011)
rf = train(x=X,
           y=Y,
           method = 'rf',
           preProcess = c("center","scale","medianImpute"),
           trControl = trainCTRL,
           tuneGrid=rfGrid,
           metric='Accuracy',
           ntree=80     #TUNE????
           )
print(rf)
plot(rf$results$mtry,rf$results$ROC, col="green")


rf = readRDS(file.path(ModelPath, 'rf_NoAudio_ROC_50.rds'))


windows()
#plot(log2(mGLM$results$lambda),mGLM$results$Accuracy, col="red") #lambda is scaled by log2
#plot(log2(mGLM$results$lambda),mGLM$results$Kappa, col="blue")
plot(rf$results$mtry,rf$results$ROC, col="green")
mGLM$finalModel$tuneValue


SI = rf$pred$mtry == rf$finalModel$mtry
obs= rf$pred$obs[SI]
pred= rf$pred$pred[SI]
PrLapse = rf$pred$Lapse[SI]
PrNoLapse = rf$pred$NoLapse[SI]
dCV = data.frame(obs=obs,pred=pred,PrNoLapse=PrNoLapse, PrLapse=PrLapse)

windows()
ROC_CV = roc(response=obs, predictor=PrLapse)
plot(ROC_CV, print.auc=TRUE)

#Plot of lapse probabilty by class
windows()
par( mfrow = c( 1, 2 ) )
hist(dCV$PrLapse[dCV$obs=="NoLapse"], breaks=seq(0,1,.1), xlab="Probabilty of Lapse", main ="No Lapse")
hist(dCV$PrLapse[dCV$obs=="Lapse"], breaks=seq(0,1,.1), xlab="Probabilty of Lapse", main ="Lapse")

#Confusion matrix
confusionMatrix(data=dCV$pred,reference=dCV$obs, positive="Lapse")

dCV$predOther = ifelse(dCV$PrLapse>.3,"Lapse","NoLapse") #30% PrLapse
dCV$predOther = factor(dCV$predOther,levels=c("NoLapse","Lapse"))
confusionMatrix(data=dCV$predOther,reference=dCV$obs, positive="Lapse") #default 50%



#Train kernel SVM------------------------------
kSVMGrid <- expand.grid(sigma = 10^(-5:-1),
                        C = 2^(-8:-4))
set.seed(05222011)
kSVM = train(x=X,
             y=Y,
             method = 'svmRadial',
             preProcess = c("center","scale","medianImpute"),
             trControl = trainCTRL,
             tuneGrid = kSVMGrid,
             metric='ROC',
             tuneLength = 5     #WHY NEEDED?
             )
print(kSVM)


saveRDS(kSVM,"/Users/host/Desktop/YuzheMa/Work/Research/NIHproject/Data/ModelResults/kSVM.rds")
kSVM = readRDS(file.path(ModelPath, 'kSVM_NoAudio_ROC.rds'))


SI = (kSVM$pred$sigma == kSVM$bestTune$sigma) & (kSVM$pred$C == kSVM$bestTune$C)
obs= kSVM$pred$obs[SI]
pred= kSVM$pred$pred[SI]
PrLapse = kSVM$pred$Lapse[SI]
PrNoLapse = kSVM$pred$NoLapse[SI]
dCV = data.frame(obs=obs,pred=pred,PrNoLapse=PrNoLapse, PrLapse=PrLapse)

#ROC and AUC
windows()
ROC_CV = roc(response=dCV$obs, predictor=dCV$PrLapse)
plot(ROC_CV, print.auc=TRUE)
dROC = data.frame(ROC_CV$sensitivities,ROC_CV$specificities,ROC_CV$thresholds)

#Plot of lapse probabilty by class
windows()
par( mfrow = c( 1, 2 ) )
hist(dCV$PrLapse[dCV$obs=="NoLapse"], breaks=seq(0,1,.1), xlab="Probabilty of Lapse", main ="No Lapse")
hist(dCV$PrLapse[dCV$obs=="Lapse"], breaks=seq(0,1,.1), xlab="Probabilty of Lapse", main ="Lapse")

#Confusion matrix
confusionMatrix(data=dCV$pred,reference=dCV$obs, positive="Lapse")

dCV$predOther = ifelse(dCV$PrLapse>.3,"Lapse","NoLapse") #30% PrLapse
dCV$predOther = factor(dCV$predOther,levels=c("NoLapse","Lapse"))
confusionMatrix(data=dCV$predOther,reference=dCV$obs, positive="Lapse") #default 50%

#Model coefficients vs. W----------------------------------------
W = as.matrix(coef(kSVM$finalModel,s=kSVM$finalModel$lambdaOpt))  #final model coefficients
iW = order(abs(W), decreasing=TRUE)
OrdW = matrix(W[iW], nrow=length(W), ncol=1, dimnames=list(c(rownames(W)[iW]), "W"))
OrdW[1:30,1]

#save/load model for later use--------------------
#saveRDS(mGLM,"P:/StudyData/RISK/Analysis/Dev/ModelResults/mGLMWeek.rds")
#saveRDS(mGLM,"P:/StudyData/RISK/Analysis/Dev/ModelResults/mGLMWeek_NoEMA.rds")
#saveRDS(mGLM,"P:/StudyData/RISK/Analysis/Dev/ModelResults/mGLMWeek_GoodGPS.rds")
#saveRDS(mGLM,"P:/StudyData/RISK/Analysis/Dev/ModelResults/mGLMWeek_BOW.rds")
#saveRDS(mGLM,"P:/StudyData/RISK/Analysis/Dev/ModelResults/mGLMWeek_W2V.rds")
#saveRDS(mGLM,"P:/StudyData/RISK/Analysis/Dev/ModelResults/mGLMWeek_NoAudio.rds")

#(load if needed) and view model-----------------------
# mGLM_All = readRDS("/Users/host/Desktop/YuzheMa/Work/Research/NIHproject/Data/ModelResults/mGLMWeek_SVD.rds")
#mGLM_NoEMA = readRDS("P:/StudyData/RISK/Analysis/Dev/ModelResults/mGLMWeek_NoEMA.rds")
#mGLM_GoodGPS = readRDS("P:/StudyData/RISK/Analysis/Dev/ModelResults/mGLMWeek_GPS.rds")
# mGLM_SVD = readRDS("P:/StudyData/RISK/Analysis/Dev/ModelResults/mGLMWeek_SVD.rds")
# mGLM_BOW = readRDS("P:/StudyData/RISK/Analysis/Dev/Mod
# mGLM_W2V = readRDS("P:/StudyData/RISK/Analysis/Dev/ModelResults/mGLMWeek_W2V.rds")
# 
# mGLM = mGLM_SVD

#Tuning Parameter results------------------------------
# windows()
#plot(log2(mGLM$results$lambda),mGLM$results$Accuracy, col="red") #lambda is scaled by log2
#plot(log2(mGLM$results$lambda),mGLM$results$Kappa, col="blue")



#Model evaluation:  CV ONLY samples------------------------------


#ROC and AUC
# windows()
