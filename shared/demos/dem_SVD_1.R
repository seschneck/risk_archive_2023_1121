#Libraries---------------------
library(tm)        #lots of text mining
library(qdap)      #replace_abbreviation, replace_contraction, replace_symbol
library(stringr)   #str_c

#Load data---------------------------
DataPath = 'P:/StudyData/RISK/Analysis/Dev/Data'
dY = readRDS(file.path(DataPath, 'YWeek.rds'))  #outcome labels
dX = readRDS(file.path(DataPath, 'Audio.rds'))  #raw data for feature extraction

#Make DT Matrix----------------------
Text = dX$Audio_Text
Text = iconv(Text, 'UTF-8', 'ASCII', sub='')
Text = removeNumbers(Text)
Text = stripWhitespace(Text)
Text = tolower(Text)
Text = removePunctuation(Text)
Text = replace_abbreviation(Text)
Text = replace_contraction(Text)
Text = replace_symbol(Text)

AllStops = c(stopwords(), 'candace', 'uh', 'um', 'oh', 'inaudible')
Text = removeWords(Text, AllStops)

#Text = stemDocument(Text)
#Text =  stemCompletion(Text)

vsText <- VectorSource(Text)
vcText <- VCorpus(vsText)

dtmText <- DocumentTermMatrix(vcText)
mText = as.matrix(dtmText)

#SVD-----------------------------
svdText <- svd(mText)
U <- svdText$u   #docs by signular vectors
D <- svdText$d   #singular values
V <- svdText$v   #terms by singular vectors

k=100  #choose 100 vectors
V_k = V[,1:k]  #include only first k SVDs
SVDs = mText %*% V_k
dX = cbind(dX,SVDs)
names(dX)[4:(ncol(dX))] = str_c('SVD', 1:k)