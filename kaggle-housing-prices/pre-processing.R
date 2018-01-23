###################################################################
#### Dependencies                                              ####
###################################################################
require(caret)      # (near) zero variance and dummyVars
require(robustHD)   # robust standardize 
###################################################################
#### Pre-processing                                            ####
###################################################################
#### remove unwanted variables
full.dt[, (variablesDrop):=NULL]
full.dt[, (variablesFactor):=NULL]
##### remove outliers
outliers.Id <-  train.dt[GrLivArea > 4000 | LotArea > 100000 | X1stFlrSF > 3000 | GarageArea > 1200,Id]
full.dt <- full.dt[!(Id %in% outliers.Id)]
##### ordinal factors
## convert ordinal factors to integers. h2o does not support ordered factors.
changeColType <- ordinalFactors
full.dt[,(changeColType):= lapply(.SD, as.integer), .SDcols = changeColType]
#### remove variables with zero variance
#### skewed variables
## log transform skewed variables (including response variable)
skewedVariables <- sapply(full.dt[, c(variablesSquareFootage,variablesValues), with = FALSE],function(x){skew(x,na.rm=TRUE)})
## keep only features that exceed a threshold for skewness
skewedVariables <- skewedVariables[skewedVariables > 0.75]
## transform excessively skewed features with log
skewedVariables <- names(skewedVariables)
full.dt[, (skewedVariables) := lapply(.SD, function(x) log(x)), .SDcols = skewedVariables]
#### scale 
## scale (exluding response)
varScale <- setdiff(c(variablesSquareFootage, variablesValues), c(response)) ## Do not scale response
full.dt[, (varScale) := lapply(.SD, function(x) robStandardize(x, centerFun = median, scaleFun = mad)), .SDcols = varScale]
###################################################################
#### Select features                                           ####
###################################################################
features <- setdiff(names(full.dt), c(response,variablesDrop, "Id","dataPartition")) 
###################################################################
#### Split data                                                ####
###################################################################
## split in train and test after engineering. Split by key is fasted method.
setkey(full.dt,"dataPartition") 
train.full.dt <- full.dt["train"]
test.dt <- full.dt["test"]
#Spliting training set into train and validate based on OverallQual
index <- createDataPartition(train.full.dt$OverallQual, p=0.80, list=FALSE)
train.dt <- train.full.dt[index,]
validate.dt <- train.full.dt[-index,]




