###################################################################
#### References                                                ####
###################################################################
## http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/drf.html
## http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/gbm.html
## https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/gbm-randomforest/GBM_RandomForest_Example.R
## https://blog.h2o.ai/2016/06/h2o-gbm-tuning-tutorial-for-r/
## http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html
## http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/xgboost.html
## http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/deep-learning.html
#  https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/deeplearning/deeplearning.R
## http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/drf.html
## http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/stacked-ensembles.html
###################################################################
#### Dependencies                                              ####
###################################################################
require(h2o)        ## machine learning algorithmes
h2o.connect(        ## connect to remote h2o cloud
  ip = "192.168.1.219",
  strict_version_check = FALSE, ## watch out here.
  port = 54321
)
###################################################################
#### Upload data to h2o                                        ####
###################################################################
## upload to h2o cloud which converts data to a h2o data frame (hex)
h2o.rm("train.full.hex")
train.full.hex <- as.h2o(train.full.dt,"train.full.hex") ## when not using a validation hold out set
h2o.rm("train.hex")
train.hex <- as.h2o(train.dt,"train.hex")
h2o.rm("test.hex")
test.hex <- as.h2o(test.dt,"test.hex")
h2o.rm("valid.hex")
validate.hex <- as.h2o(validate.dt,"valid.hex") ## when using a hold oud set for validation
###################################################################
#### Set defaults                                              ####
###################################################################
# Number of CV folds 
nFolds <- 5
ignoreConstCols = TRUE
###################################################################
#### Random forest                                             ####
###################################################################
rf <- h2o.randomForest(
            training_frame =  train.full.hex, 
            nfolds =nFolds,
            fold_assignment = "Modulo",
            keep_cross_validation_predictions = TRUE,
            ## train on subset and validate against hold out data set
            #training_frame =  train.hex,
            #validation_frame = validate.hex, 
            x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
            y=response,                          ## what we are predicting,alternativaly, e.g. 81
            ignore_const_cols = ignoreConstCols,
            stopping_rounds = 3, stopping_tolerance = 0.01, stopping_metric = "RMSLE", 
            model_id = "rf_housing_v1",         ## name the model in H2O
            seed = 333)                          ## Set the random seed for reproducability
## performance of the model
h2o.performance(rf, newdata = train.full.hex)
#h2o.performance(gbm, newdata = validate.hex)
## Extract specific metric
h2o.rmsle(rf, train = T)
#h2o.rmsle(gbm, valid = T) ## when training with a validation frame
h2o.rmsle(h2o.performance(rf, xval = T)) ## when training with cross validation
## Show a detailed summary of the cross validation metrics
## This gives you an idea of the variance between the folds
rf@model$cross_validation_metrics_summary
###################################################################
#### Gradient Boosting Machine (GBM)                           ####
###################################################################
gbm <- h2o.gbm(
            ## train on full data with cross validation
            training_frame =  train.full.hex, 
            nfolds =nFolds,
            fold_assignment = "Modulo",
            keep_cross_validation_predictions = TRUE,
            ## train on subset and validate against hold out data set
            #training_frame =  train.hex,
            #validation_frame = validate.hex,     
            x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
            y=response,                          ## what we are predicting,alternativaly, e.g. 81
            ntrees = 100, # first do 1000, then plot, then adjust to 40
            max_depth = 3,
            distribution = "AUTO",
            min_rows = 4,
            learn_rate=0.3,
            min_split_improvement = 1e-3,
            #learn_rate_annealing = 0.99,         ## learning rate annealing: learning_rate shrinks by 1% after every tree
            #sample_rate = 0.8,                   ## sample 80% of rows per tree
            #col_sample_rate = 0.8,               ## sample 80% of columns per split
            ignore_const_cols = ignoreConstCols,
            stopping_rounds = 3, stopping_tolerance = 0.01, stopping_metric = "RMSLE", 
            model_id = "gbm_housing_v1",         ## name the model in H2O
            seed = 333)                          ## Set the random seed for reproducability
## performance of the model
h2o.performance(gbm, newdata = train.hex)
#h2o.performance(gbm, newdata = validate.hex)
## Extract specific metric
h2o.rmsle(gbm, train = T)
#h2o.rmsle(gbm, valid = T) ## when training with a validation frame
h2o.rmsle(h2o.performance(gbm, xval = T)) ## when training with cross validation
## Show a detailed summary of the cross validation metrics
## This gives you an idea of the variance between the folds
gbm@model$cross_validation_metrics_summary
###################################################################
#### Generalized Linear Model (GLM)                            ####
###################################################################
glm <- h2o.glm(
            ## train on full data with cross validation
            training_frame =  train.full.hex, 
            nfolds = nFolds,
            fold_assignment = "Modulo", 
            keep_cross_validation_predictions = TRUE,
            ## train on subset and validate against hold out data set
            #training_frame =  train.hex,
            #validation_frame = validate.hex,     
            x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
            y=response,                          ## what we are predicting,alternativaly, e.g. 81
            family = "gaussian",
            link = "identity",
            standardize = TRUE,  
            remove_collinear_columns = TRUE,
            ignore_const_cols = ignoreConstCols,
            solver = "COORDINATE_DESCENT", # "L_BFGS" "COORDINATE_DESCENT"
            lambda_search = T,
            early_stopping = TRUE,
            max_iterations = 200,
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
#### XGBoost                                                   ####
###################################################################
xgb <- h2o.xgboost(
                       ## train on full data with cross validation
                       training_frame =  train.full.hex, 
                       nfolds = nFolds,
                       fold_assignment = "Modulo", 
                       keep_cross_validation_predictions = TRUE,
                       ## train on subset and validate against hold out data set
                       #training_frame =  train.hex,
                       #validation_frame = validate.hex,    
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
                       min_split_improvement = 1e-3,         ## The value of this option specifies the minimum relative improvement in squared error reduction in order for a split to happen. When properly tuned, this option can help reduce overfitting. Optimal values would be in the 1e-10…1e-3 range.  
                       ignore_const_cols = ignoreConstCols,
                       model_id = "xgb_housing_v1",         ## name the model in H2O
                       seed = 333)
## Extract specific metric
h2o.rmsle(xgb, train = T)
#h2o.rmsle(xgb, valid = T)
h2o.rmsle(h2o.performance(xgb, xval = T))
xgb@model$cross_validation_metrics_summary
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
#### Combine models in a stacked ensemble                      ####
###################################################################
## Note: All base models must have the same cross-validation folds, fold assignments and
## the cross-validated predicted values must be kept.
# Train a stacked ensemble using the GBM and GLM above
ensemble <- h2o.stackedEnsemble(
                                ## train on full data with cross validation
                                training_frame =  train.full.hex, 
                                #keep_cross_validation_predictions = TRUE,
                                ## train on subset and validate against hold out data set
                                #training_frame =  train.hex,
                                #validation_frame = validate.hex,    
                                x=features,                        ## the predictor columns, by column index
                                y=response,                        ## the target index (what we are predicting)
                                metalearner_algorithm = "glm",
                                metalearner_nfolds = nFolds,
                                model_id = "metalearnerGlm_GbmGlmRf",
                                keep_levelone_frame = T,
                                base_models = list(gbm, glm, rf))
###################################################################
#### Predict                                                   ####
###################################################################
finalPredictions <- h2o.predict(
                              object = ensemble
                              ,newdata = test.hex)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- h2o.exp(finalPredictions$SalePrice) 
submission <- h2o.cbind(test.hex[, "Id"],finalPredictions)
h2o.exportFile(submission, path = "/home/h2o/h2o/output/submission.h2o.ensembleGlm.csv", force = T)