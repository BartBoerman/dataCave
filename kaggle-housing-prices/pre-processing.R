###################################################################
#### Dependencies                                              ####
###################################################################
require(caret)      # (near) zero variance and dummyVars
###################################################################
#### Pre-processing                                            ####
###################################################################
#### remove unwanted variables
full.dt[, (variablesDrop):=NULL]
##### remove outliers
full.dt <- full.dt[!(Id %in% outliers.Id) & dataPartition == "train",]
##### ordinal factors
## convert ordinal factors to integers. h2o does not support ordered factors.
changeColType <- variablesFactor
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
full.dt <- full.dt[ , (variablesSquareFootage) := lapply(.SD, scale), .SDcols = variablesSquareFootage]
###################################################################
#### Select features                                           ####
###################################################################
features <- setdiff(names(full.dt), c(response,variablesDrop, "Id","dataPartition")) 
###################################################################
#### Split data                                                ####
###################################################################
## split in train and test after engineering. Split by key is fasted method.
setkey(full.dt,"dataPartition") 
train.dt <- full.dt["train"]
test.dt <- full.dt["test"]