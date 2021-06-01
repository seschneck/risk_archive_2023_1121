

d = data.frame(Y = c(10,2,3,19,100,4,5,6),X1 = c(1,2,3,4,5,6,5,6), X2=c(2,4,3,4,2,1,5,6), ID1=c(5,4,3,4,2,10,5,6))
m1 = lm(Y ~ .*., data=d)
modelSummary(m1)
m2 = lm(Y ~ .*., data=d)
modelSummary(m2)
m3 = lm(Y ~ ID1 * (X1+X2) + (X1+X2)^2, data=d)
mmX = model.matrix(formula('~ 0 + ID1 * (X1+X2) + (X1+X2)^2'),d)
modelSummary(m3)


NAMES = c('ID_1','ID_2','ID_3','O1','O2')
IDVars = formulaSet(NAMES, 'ID_', TRUE)
OtherVars = formulaSet(NAMES, 'ID_', FALSE)

IDVars = formulaSet(names(X), 'ID_', TRUE)
OtherVars = formulaSet(names(X), 'ID_', FALSE)
IDVars = names(X)[str_detect(names(X), 'ID_')]
IDVars=str_c('(', str_c(IDVars[1:(length(IDVars)-1)],' + ', collapse=TRUE), '+', IDVars[length(IDVars)], ')')
