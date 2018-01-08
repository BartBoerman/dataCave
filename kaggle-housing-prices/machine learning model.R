###################################################################
#### References                                                ####
###################################################################
## https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/gbm-randomforest/GBM_RandomForest_Example.R
## https://blog.h2o.ai/2016/06/h2o-gbm-tuning-tutorial-for-r/
###################################################################
#### Dependencies                                              ####
###################################################################
require(h2o)        # machine learning algorithmes
h2o.connect(
  ip = "192.168.1.215",
  port = 54321
) 
###################################################################
#### Set response (target)                                     ####
###################################################################
response <- "SalePrice"
###################################################################
#### Pre-processing                                            ####
###################################################################
## remove variables with zero variance
zeroVarianceVariables <- nearZeroVar(full.dt, names = T, 
                                     freqCut = 10, uniqueCut = 2,
                                     foreach = T, allowParallel = T) ## Select variables with (near) zero veriance
full.dt <- full.dt[, -c(zeroVarianceVariables), with = FALSE]
variablesSquareFootage <- setdiff(c(variablesSquareFootage), c(zeroVarianceVariables))
variablesValues      <- setdiff(c(variablesValues ), c(zeroVarianceVariables))
## log transform skewed variables (including response variable)
# determine skew
skewedVariables <- sapply(full.dt[, c(variablesSquareFootage,variablesValues), with = FALSE],function(x){skew(x,na.rm=TRUE)})
# keep only features that exceed a threshold for skewness
skewedVariables <- skewedVariables[skewedVariables > 0.75]
## transform excessively skewed features with log
skewedVariables <- names(skewedVariables)
full.dt[, (skewedVariables) := lapply(.SD, function(x) log(x)), .SDcols = skewedVariables]
## scale (excluding response)
varScale <- setdiff(c(variablesSquareFootage, variablesValues), c(response)) ## Do not scale response
full.dt <- full.dt[ , (variablesSquareFootage) := lapply(.SD, scale), .SDcols = variablesSquareFootage]
###################################################################
#### Select features                                           ####
###################################################################
features <- setdiff(names(full.dt), c(response, "Id","SalePrice","dataPartition")) 
###################################################################
#### Split data                                                ####
###################################################################
## split in train and test after engineering. Split by key is fasted method.
setkey(full.dt,"dataPartition") 
train.dt <- full.dt["train"]
test.dt <- full.dt["test"]
## upload to h2o cloud which converts data to a h2o data frame (hex)
train.hex <- as.h2o(train.dt,"train.hex")
test.hex <- as.h2o(test.dt,"test.hex")
## split training data into training and validation data sets
splits <- h2o.splitFrame(
  train.hex,           ##  splitting the H2O frame we read above
  c(0.8),   
  seed=333)    ##  setting a seed will ensure reproducible results (not R's seed)

train.hex <- h2o.assign(splits[[1]], "train.hex")   
validate.hex <- h2o.assign(splits[[2]], "valid.hex")  


