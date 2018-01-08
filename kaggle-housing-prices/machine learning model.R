###################################################################
#### References                                                ####
###################################################################
## https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/gbm-randomforest/GBM_RandomForest_Example.R
## https://blog.h2o.ai/2016/06/h2o-gbm-tuning-tutorial-for-r/
###################################################################
#### Dependencies                                              ####
###################################################################
require(h2o)        # machine learning algorithmes
require(caret)      # (near) zero variance
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
## remove outliers
full.dt <- full.dt[!(Id %in% outliers.Id),]
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
###################################################################
#### Gradient Boosting Machine (GBM)                           ####
###################################################################
gbm <- h2o.gbm(
      training_frame = train.hex,          ## the H2O frame for training
      validation_frame = validate.hex,     ## the H2O frame for validation (not required)
      x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
      y=response,                          ## what we are predicting,alternativaly, e.g. 81
      nfolds = 3,
      ntrees = 40, # first do 1000, then plot, then adjust to 40
      learn_rate=0.1,
      #learn_rate_annealing = 0.99,         ## learning rate annealing: learning_rate shrinks by 1% after every tree
      sample_rate = 0.8,                   ## sample 80% of rows per tree
      col_sample_rate = 0.8,               ## sample 80% of columns per split
      ignore_const_cols = TRUE,
      stopping_rounds = 5, stopping_tolerance = 0.01, stopping_metric = "RMSLE", 
      score_tree_interval = 10,              ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)   
      model_id = "gbm_housing_v1",         ## name the model in H2O
      seed = 333)                          ## Set the random seed for reproducability








