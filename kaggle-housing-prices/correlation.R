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
corTableHigh <- corTable[value > 0.7][order(Var1,-value)]

#variablesOrdered <- sapply(full.dt,is.ordered) ## ordered factors
#variablesFactor <- sapply(full.dt,is.factor)  ## contains also ordered factors

