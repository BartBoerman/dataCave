require(caret)
###############################################################################
#### One hot encoding of factor variables with dummyVars from caret        ####                                                   ####
###############################################################################
## formulala for one hot encoding
f <- paste('~', paste(variablesFactor, collapse = ' + '))
## one hot encode train data with dummyVars from caret package
encoder <- dummyVars(as.formula(f), train.dt, fullRank = T, drop2nd = T)
## apply encoding on full data set
full.dummyVars.dt <- as.data.table(predict(encoder, full.dt))
###############################################################################
######## Analyse dummy vars                                                ####
###############################################################################
## filter out test data set
full.dummyVars.dt <- cbind(full.dt[,dataPartition], full.dummyVars.dt)
test.dummyVars.dt <- full.dummyVars.dt[V1 == "test",]
## levels not available in test data set have range zero
dummyVars.range.max <- sapply(test.dummyVars.dt[,!c("V1")], 
                              function(x) {range(x, na.rm = T, finite = F)})[2,]
dummyVars.df <- data.frame(dummyVars.range.max)
dummyVars.df <- data.frame(variableName = row.names(dummyVars.df), dummyVars.df,row.names = NULL)
dummyVars.df <- dummyVars.df[dummyVars.range.max == 0,]
missingLevels <- as.character(dummyVars.df$variableName)  
### what is the distribution of these levels in the training data?
train.dummyVars.missing.dt <- full.dummyVars.dt[V1 == "train", c(missingLevels), with=FALSE]
train.dummyVars.missing.sum <- sapply(train.dummyVars.missing.dt, 
                                      function(x) {sum(x, na.rm = T)})
print(train.dummyVars.missing.sum)
###############################################################################
##### Apply without levels missing in test data                            ####
###############################################################################
## formulala for one hot encoding
f <- paste('~', paste(variablesFactor, collapse = ' + '))
## one hot encode train data with dummyVars from caret package
encoder <- dummyVars(as.formula(f), train.dt, fullRank = T, drop2nd = T)
## apply encoding on full data set
full.dummyVars.dt <- as.data.table(predict(encoder, full.dt))
## Could have done a more thorough job, other missing levels will just be removed
full.dummyVars.dt <- full.dummyVars.dt[,!c(missingLevels), with=FALSE] ## gives error because we solved some
## Combine with full data sets
full.dt <- cbind(full.dt,full.dummyVars.dt)
###############################################################################
##### Remove original factors from data                                    ####
###############################################################################
#full.dt <- full.dt[,!c(variablesFactor), with=FALSE]

