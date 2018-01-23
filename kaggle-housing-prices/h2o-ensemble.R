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
train <- h2o.getFrame("train.hex")
test <- h2o.getFrame("valid.hex")
nFolds <- 5
ignoreConstCols = TRUE
###################################################################
#### Random forest                                             ####
###################################################################
rf <- h2o.randomForest(
            training_frame =  train, 
            nfolds =nFolds,
            fold_assignment = "Modulo",
            keep_cross_validation_predictions = TRUE,
            x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
            y=response,                          ## what we are predicting,alternativaly, e.g. 81
            ignore_const_cols = ignoreConstCols,
            stopping_rounds = 3, stopping_tolerance = 0.01, stopping_metric = "RMSLE", 
            model_id = "rf_housing_v1",         ## name the model in H2O
            seed = 333)                          ## Set the random seed for reproducability
#### performance of the model
h2o.performance(rf, newdata = test)
#### Extract specific metric
h2o.rmsle(rf, train = T)
#### Cross validation metrics
h2o.rmsle(h2o.performance(rf, xval = T)) 
rf@model$cross_validation_metrics_summary ## This gives you an idea of the variance between the folds
###################################################################
#### Gradient Boosting Machine (GBM)                           ####
###################################################################
gbm <- h2o.gbm(
            training_frame =  train, 
            nfolds =nFolds,
            fold_assignment = "Modulo",
            keep_cross_validation_predictions = TRUE,
            x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
            y=response,                          ## what we are predicting,alternativaly, e.g. 81
            ntrees = 100, 
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
#### performance of the model
h2o.performance(gbm, newdata = test)
#### Extract specific metric
h2o.rmsle(gbm, train = T)
#### Cross validation metrics
h2o.rmsle(h2o.performance(rf, xval = T)) 
rf@model$cross_validation_metrics_summary ## This gives you an idea of the variance between the folds
###################################################################
#### Generalized Linear Model (GLM)                            ####
###################################################################
glm <- h2o.glm(
            training_frame =  train, 
            nfolds = nFolds,
            fold_assignment = "Modulo", 
            keep_cross_validation_predictions = TRUE,
            x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
            y=response,                          ## what we are predicting,alternativaly, e.g. 81
            family = "gaussian",
            link = "identity",
            standardize = TRUE,  
            remove_collinear_columns = TRUE,
            ignore_const_cols = ignoreConstCols,
            solver = "L_BFGS", # "L_BFGS" "COORDINATE_DESCENT"
            lambda_search = T,
            early_stopping = TRUE,
            max_iterations = 200,
            model_id = "glm_housing_v1",
            seed = 333)
#### performance of the model
h2o.performance(glm, newdata = test)
#### Extract specific metric
h2o.rmsle(glm, train = T)
#### Cross validation metrics
h2o.rmsle(h2o.performance(glm, xval = T)) 
glm@model$cross_validation_metrics_summary ## This gives you an idea of the variance between the folds
###################################################################
#### Variable importance                                       ####
###################################################################
model <- h2o.getModel("rf_housing_v1")
## plot
h2o.varimp_plot(model)
## create overview for future reference
varImportance <- h2o.varimp(model)
varImportance.df <- as.data.frame(cbind(varImportance$variable, varImportance$percentage))
names(varImportance.df) <- c("variable","importance")
varImportance.df$importance <- round(as.numeric(as.character(varImportance.df$importance)),3)
featuresTop <-head(varImportance.df,50)
###################################################################
#### Combine models in a stacked ensemble                      ####
###################################################################
## Note: All base models must have the same cross-validation folds, fold assignments and
## the cross-validated predicted values must be kept.
# Train a stacked ensemble using the GBM and GLM above
ensemble <- h2o.stackedEnsemble(
                                training_frame =  train, 
                                x=features,                        ## the predictor columns, by column index
                                y=response,                        ## the target index (what we are predicting)
                                metalearner_algorithm = "glm",
                                metalearner_nfolds = nFolds,
                                model_id = "metalearnerGlm_GbmGlmRf_v2",
                                keep_levelone_frame = T,
                                base_models = list(gbm, glm, rf))
###################################################################
#### Compare performance of the model on the test data        #####
###################################################################
perfEnsemble <- h2o.performance(ensemble, newdata = test)
## Compare to base learner performance on the test set
perfRfTest <- h2o.performance(rf, newdata = test)
perfGbmTest <- h2o.performance(gbm, newdata = test)
perfGlmTest <- h2o.performance(glm, newdata = test)
baselearnerMaxRmsleTest <- max(h2o.rmsle(perfRfTest), h2o.rmsle(perfGbmTest), h2o.rmsle(perfGlmTest))
ensembleRsmleTest <- h2o.rmsle(perfEnsemble)
print(sprintf("Best base-learner RSMLE:  %s", baselearnerMaxRmsleTest))
print(sprintf("Ensemble test RSMLE:  %s", ensembleRsmleTest))
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
