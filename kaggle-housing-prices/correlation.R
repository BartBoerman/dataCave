###################################################################
#### correlation                                               ####
###################################################################
## porchTotalSF, totalSF are based on these variables, let's remove them
removeVariables <- c("OpenPorchSF","EnclosedPorch","ThreeSsnPorch","ScreenPorch","TotalBsmtSF","FirstFlrSF","SecondFlrSF")
full.dt[, (removeVariables):=NULL] 
variablesSquareFootage <- setdiff(c(variablesSquareFootage), c(removeVariables))
## corralation matrix
corMatrix = cor(full.dt[dataPartition == "train",..variablesSquareFootage])
highlyCorrelated = findCorrelation(corMatrix, cutoff=0.8, names = T, exact = T)
# Oeps, thats the just created feature.