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
  ip = "192.168.1.219",
  strict_version_check = FALSE, ## watch out here.
  port = 54321
)
###################################################################
#### Upload data to h2o                                        ####
###################################################################
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
            ntrees = 100, # first do 1000, then plot, then adjust to 40
            max_depth = 3,
            distribution = "AUTO",
            min_rows = 4,
            learn_rate=0.3,
            min_split_improvement = 1e-3,
            #learn_rate_annealing = 0.99,         ## learning rate annealing: learning_rate shrinks by 1% after every tree
            #sample_rate = 0.8,                   ## sample 80% of rows per tree
            #col_sample_rate = 0.8,               ## sample 80% of columns per split
            ignore_const_cols = TRUE,
            stopping_rounds = 1, stopping_tolerance = 0.01, stopping_metric = "RMSLE", 
            model_id = "gbm_housing_v1",         ## name the model in H2O
            seed = 333)                          ## Set the random seed for reproducability
## performance of the model
h2o.performance(gbm, newdata = train.hex)
h2o.performance(gbm, newdata = validate.hex)
## Extract specific metric
h2o.rmsle(gbm, train = T)
h2o.rmsle(gbm, valid = T)
h2o.rmsle(h2o.performance(gbm, xval = T))
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
#### GBM Predict and submit                                    ####
###################################################################
finalPredictions <- h2o.predict(
  object = gbm
  ,newdata = test.hex)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- h2o.exp(finalPredictions$SalePrice) 
submission <- h2o.cbind(test.hex[, "Id"],finalPredictions)
h2o.exportFile(submission, path = "/home/h2o/h2o/output/submission.h2o.gbm.csv", force = T)
###################################################################
#### Generalized Linear Model (GLM)                            ####
###################################################################
glm <- h2o.glm(
            training_frame = train.hex,          ## the H2O frame for training
            validation_frame = validate.hex,     ## the H2O frame for validation (not required)
            x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
            y=response,                          ## what we are predicting,alternativaly, e.g. 81
            family = "gaussian",
            standardize = TRUE,             
            missing_values_handling = "Skip",
            remove_collinear_columns = TRUE,
            nfolds = 3,
            fold_assignment = "Modulo", 
            ignore_const_cols = TRUE,
            solver = "COORDINATE_DESCENT", # "L_BFGS" "COORDINATE_DESCENT"
            early_stopping = TRUE,
            max_iterations = 100,
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
h2o.exportFile(submission, path = "/home/h2o/h2o/output/submission.h2o.glm.csv", force = T)
###################################################################
#### XGBoost                                                   ####
###################################################################
xgb <- h2o.xgboost(training_frame = train.hex,          ## the H2O frame for training
                       validation_frame = validate.hex,     ## the H2O frame for validation (not required)
                       x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
                       y=response,                          ## what we are predicting,alternativaly, e.g. 81
                       distribution = "gaussian",
                       #categorical_encoding = "EnumLimited",
                       ntrees = 300,
                       max_depth = 3,                       ## Higher values will make the model more complex and can lead to overfitting.
                       min_rows = 4,
                       learn_rate = 0.03,
                       sample_rate = 1.0,                   ## Higher values may improve training accuracy. Test accuracy improves when either columns or rows are sampled.   
                       col_sample_rate = 1.0,
                       #max_abs_leafnode_pred = 0.2,          ## Reduce overfitting by limiting the absolute value of a leafe node prediction
                       #min_split_improvement = 1e-3,         ## The value of this option specifies the minimum relative improvement in squared error reduction in order for a split to happen. When properly tuned, this option can help reduce overfitting. Optimal values would be in the 1e-10…1e-3 range.  
                       nfolds =3,
                       #fold_assignment = "Modulo",
                       #keep_cross_validation_predictions = TRUE,
                       seed = 333)
## Extract specific metric
h2o.rmsle(xgb, train = T)
h2o.rmsle(xgb, valid = T)
h2o.rmsle(h2o.performance(xgb, xval = T))
###################################################################
#### Predict xgb                                               ####
###################################################################
finalPredictions <- h2o.predict(
  object = xgb
  ,newdata = test.hex)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- h2o.exp(finalPredictions$SalePrice) 
submission <- h2o.cbind(test.hex[, "Id"],finalPredictions)
h2o.exportFile(submission, path = "/home/h2o/h2o/output/submission.h2o.xgb.csv", force = T)
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
  max_runtime_secs = 600,
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
autoStack <- h2o.getModel("StackedEnsemble_AllModels_0_AutoML_20180119_220836")
autoGLM <- h2o.getModel("GLM_grid_0_AutoML_20180119_220836_model_0")
autoGBM <- h2o.getModel("GBM_grid_0_AutoML_20180119_220836_model_15")   
finalPredictions <- h2o.predict(
  object =  autoMl@leader
  ,newdata = test.hex)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- h2o.exp(finalPredictions$SalePrice) 
submission <- h2o.cbind(test.hex[, "Id"],finalPredictions)
h2o.exportFile(submission, path = "/home/h2o/h2o/output/submission.h2o.autML.csv", force = T)
