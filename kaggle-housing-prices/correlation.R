###################################################################
#### correlation                                               ####
###################################################################
## https://stackoverflow.com/questions/46308308/find-the-pair-of-most-correlated-variables
#### corralation matrix
variablesNumeric <- sapply(full.dt,is.numeric) ## contains also integers
corrData <- full.dt[dataPartition == "train", ..variablesNumeric ]
corMatrix = cor(corrData)
#### table with highly correlated values
corTable <- setDT(melt(corMatrix))[order(-value)][value!=1] 
corTableSalePrice <- corTable[Var1=="SalePrice",][order(-value)]
corTableHigh <- corTable[value > 0.8 | value < -0.08 ][order(Var1,-value)]

#variablesOrdered <- sapply(full.dt,is.ordered) ## ordered factors
#variablesFactor <- sapply(full.dt,is.factor)  ## contains also ordered factors

features <- setdiff(features, c(
  "GarageCond",     ## high correlation with GarageQual
  "GarageYrBlt",    ## high correlation with GarageQual
  "GarageArea",     ## high correlation GarageCars
  "TotRmsAbvGrd" ,  ## high correlation GrLivArea
  "YearRemodAdd",   ## correlated with qualities and age 
  "TotalBsmtSF",      ## correlated	with BsmtCond BsmtQual
  "BsmtFinSF1"       ## correlated with 	BsmtFinType1
))
