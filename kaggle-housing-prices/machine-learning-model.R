###################################################################
#### References                                                ####
###################################################################
## https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/gbm-randomforest/GBM_RandomForest_Example.R
## https://blog.h2o.ai/2016/06/h2o-gbm-tuning-tutorial-for-r/
###################################################################
#### Dependencies                                              ####
###################################################################
require(h2o)        # machine learning algorithmes
require(caret)      # (near) zero variance and dummyVars
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
## convert hierarchical factors to integers
changeColType <- c("FireplaceQu","OverallQual","OverallCond","KitchenQual","GarageFinish","ExterQual","ExterCond","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","Electrical","Fence","PoolQC")
full.dt[,(changeColType):= lapply(.SD, as.integer), .SDcols = changeColType]
## remove variables with zero variance
zeroVarianceVariables <- nearZeroVar(full.dt, names = T, 
                                     freqCut = 10, uniqueCut = 2,
                                     foreach = T, allowParallel = T) ## Select variables with (near) zero veriance
full.dt <- full.dt[, (zeroVarianceVariables):=NULL] ## Drop variables
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
## disabled to check performance of glm with standardize TRUE
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
h2o.rm("train.hex")
train.hex <- as.h2o(train.dt,"train.hex")
h2o.rm("test.hex")
test.hex <- as.h2o(test.dt,"test.hex")
## split training data into training and validation data sets
splits <- h2o.splitFrame(
  train.hex,           ##  splitting the H2O frame we read above
  c(0.8),   
  seed=333)    ##  setting a seed will ensure reproducible results (not R's seed)

train.hex <- h2o.assign(splits[[1]], "train.hex")   
h2o.rm("validate.hex")
validate.hex <- h2o.assign(splits[[2]], "valid.hex")  
###################################################################
#### Gradient Boosting Machine (GBM)                           ####
###################################################################
gbm <- h2o.gbm(
      training_frame = train.hex,          ## the H2O frame for training
      validation_frame = validate.hex,     ## the H2O frame for validation (not required)
      x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
      y=response,                          ## what we are predicting,alternativaly, e.g. 81
      nfolds = 5,
      ntrees = 1000, # first do 1000, then plot, then adjust to 40
      learn_rate=0.1,
      #learn_rate_annealing = 0.99,         ## learning rate annealing: learning_rate shrinks by 1% after every tree
      sample_rate = 0.8,                   ## sample 80% of rows per tree
      col_sample_rate = 0.8,               ## sample 80% of columns per split
      ignore_const_cols = TRUE,
      stopping_rounds = 5, stopping_tolerance = 0.01, stopping_metric = "RMSLE", 
      score_tree_interval = 10,              ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)   
      model_id = "gbm_housing_v1",         ## name the model in H2O
      seed = 333)                          ## Set the random seed for reproducability
## performance of the model
h2o.performance(gbm, newdata = train.hex)
h2o.performance(gbm, newdata = validate.hex)
## Extract specific metric
h2o.rmsle(gbm, train = T)
h2o.rmsle(gbm, valid = T)
h2o.rmse(h2o.performance(gbm, xval = T))
## Show a detailed summary of the cross validation metrics
## This gives you an idea of the variance between the folds
gbm@model$cross_validation_metrics_summary
###################################################################
#### Variable importance                                       ####
###################################################################
## plot
h2o.varimp_plot(gbm)
## create overview for future reference
varImportance <- h2o.varimp(gbm)
varImportance.df <- as.data.frame(cbind(varImportance$variable, varImportance$percentage))
names(varImportance.df) <- c("variable","importance")
varImportance.df$importance <- round(as.numeric(as.character(varImportance.df$importance)),3)
featuresTopTen <-head(varImportance$variable)
write.csv(varImportance.df, file = "varImp.csv") ## For future reference
###################################################################
#### Generalized Linear Model (GLM)                            ####
###################################################################
glm <- h2o.glm(
          training_frame = train.hex,          ## the H2O frame for training
          validation_frame = validate.hex,     ## the H2O frame for validation (not required)
          x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
          y=response,                          ## what we are predicting,alternativaly, e.g. 81
          family = "gaussian",                      
          alpha =  c(0.0,0.5,1.0),
          lambda_search = TRUE,
          ## nlambdas = 30,
          standardize = FALSE,               ## Scaling already done.
          missing_values_handling = "Skip",
          remove_collinear_columns = TRUE,
          nfolds = 3,
          fold_assignment = "Modulo",
          ignore_const_cols = TRUE,
          solver = "COORDINATE_DESCENT", # "L_BFGS" "COORDINATE_DESCENT"
          early_stopping = TRUE,
          max_iterations = 300,
          max_runtime_secs = 300,
          model_id = "glm_housing_v1",
          seed = 333)
## performance of the model
h2o.performance(glm, newdata = train.hex)
h2o.performance(glm, newdata = validate.hex)
## Extract specific metric
h2o.rmsle(glm, train = T)
h2o.rmsle(glm, valid = T)
h2o.rmsle(h2o.performance(glm, xval = T))
## Show a detailed summary of the cross validation metrics
## This gives you an idea of the variance between the folds
glm@model$cross_validation_metrics_summary
###################################################################
#### GLM Predict and submit                                    ####
###################################################################
finalPredictions <- h2o.predict(
  object = glm
  ,newdata = test.hex)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- h2o.exp(finalPredictions$SalePrice) 
submission <- h2o.cbind(test.hex[, "Id"],finalPredictions)
h2o.exportFile(submission, path = "submission.h2o.glm.csv", force = T)
###################################################################
#### Automated machine learning                                ####
###################################################################
autoMl <- h2o.automl(
  training_frame = train.hex,        ## the H2O frame for training
  validation_frame = validate.hex,      ## the H2O frame for validation (not required)
  x=features,                        ## the predictor columns, by column index
  y=response,                          ## the target index (what we are predicting)
  stopping_metric = "RMSLE",
  nfolds = 3,
  seed = 333,
  max_runtime_secs = 3600,
  stopping_rounds = 2,
  stopping_tolerance = 0.001,
  project_name = "KaggleHousingPrices"
)
autoMl@leaderboard ## Models evaluated bu h2o
## Extract specific metric
h2o.rmsle(autoMl@leader, train = T)
h2o.rmsle(autoMl@leader, valid = T)
###################################################################
#### Predict and submit                                        ####
###################################################################
autoStack <- h2o.getModel("StackedEnsemble_BestOfFamily_0_AutoML_20180112_120335")
autoGLM <- h2o.getModel("GLM_grid_0_AutoML_20180112_120335_model_0")
autoGBM <- h2o.getModel("GBM_grid_0_AutoML_20180112_125300_model_53")   
finalPredictions <- h2o.predict(
  object =  autoGBM ##autoMl@leader
  ,newdata = test.hex)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- h2o.exp(finalPredictions$SalePrice) 
submission <- h2o.cbind(test.hex[, "Id"],finalPredictions)
h2o.exportFile(submission, path = "submission.h2o.autGBM.csv", force = T)



# remove data.frames before uploading solves data type error?
# Warning messages:
#   1: In doTryCatch(return(expr), name, parentenv, handler) :
#   Test/Validation dataset column 'YearBuilt' has levels not trained on: [1879, 1895, 1896, 1901, 1902, 1907]
# 2: In doTryCatch(return(expr), name, parentenv, handler) :
#   Test/Validation dataset column 'GarageYrBlt' has levels not trained on: [1895, 1896, 1917, 1919, 1943, 2207]
# 3: In doTryCatch(return(expr), name, parentenv, handler) :
#   Test/Validation dataset column 'MSSubClass' has levels not trained on: [150]
# 
