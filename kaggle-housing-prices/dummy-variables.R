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
##### Solution for missing levels in test data                             ####
###############################################################################
## HouseStyle
full.dt[, hasUnfinishedLevel:= ifelse(HouseStyle %in% c("1.5Unf","2.5Unf"),1,0)]
require(plyr) ## easy mapping of factor levels
full.dt$HouseStyle <-mapvalues(full.dt$HouseStyle, 
                               from = c("1Story", "1.5Fin", "1.5Unf","2.5Unf","2.5Fin","2Story","SFoyer","SLvl"), 
                               to =   c("1Story", "1Story", "1Story","2Story","2Story","2Story","SFoyer","SLvl"))
full.dt[, hasUnfinishedLevel:= ifelse(HouseStyle %in% c("1.5Unf","2.5Unf"),1,0)]
## Condition1 and Condition2
full.dt[, ':=' (isConditionNearRail    = ifelse(Condition1 %in% c("RRAe","RRAn","RRNe","RRNn") | Condition2 %in% c("RRAe","RRAn","RRNe","RRNn"),1,0),
                isConditionNearArtery  = ifelse(Condition1 == "Artery" | Condition2 == "Artery",1,0),
                isConditionNearFeedr  = ifelse(Condition1 == "Feedr" | Condition2 == "Feedr",1,0),
                isConditionNearPosFeature  = ifelse(Condition1 %in% c("PosA"," PosN") | Condition2 %in% c("PosA"," PosN"),1,0),
                Condition1 = NULL, ## highly correlated
                Condition2 = NULL  ## highly correlated
)]
## Utilities
## one recordin train data, remove it as outlier (or drop column)
full.dt <- full.dt[Utilities != "NoSeWa",]
## RoofMatl
full.dt[, ':=' (hasRoofMatlWoodCly = ifelse(RoofMatl %in% c("WdShake","WdShngl","ClyTile"),1,0),
                hasRoofTar = ifelse(RoofMatl == "Tar&Grv",1,0), 
                hasRoofCompShg = ifelse(RoofMatl == "CompShg",1,0), 
                RoofMatl = NULL  ## highly correlated
)]
## removed dropped factors from list
variablesFactor <- setdiff(c(variablesFactor),c("Condition1","Condition2","RoofMatl"))
missingLevels   <- setdiff(c(missingLevels)  ,c("UtilitiesNoSeWa"
                                                ,"Condition2RRAe"
                                                ,"Condition2RRAn"
                                                ,"Condition2RRNn"
                                                ,"HouseStyle2.5Fin"
                                                ,"RoofMatlMembran"
                                                ,"RoofMatlMetal"
                                                ,"RoofMatlRoll"))
###############################################################################
##### Apply                                                                ####
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


full.dt <- full.dt[,!c(variablesFactor), with=FALSE]

