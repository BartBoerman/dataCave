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
h2o.rm("validate.hex")
validate.hex <- as.h2o(validate.dt,"validate.hex") ## when using a hold oud set for validation
###################################################################
#### Set defaults                                              ####
###################################################################
# Number of CV folds
train <- h2o.getFrame("train.hex")
test <- h2o.getFrame("validate.hex")
nFolds <- 3
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
            stopping_rounds = 3, stopping_tolerance = 0.01, stopping_metric = "RMSE", 
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
            min_rows = 1,
            learn_rate=0.2,
            min_split_improvement = 1e-3,
            #learn_rate_annealing = 0.99,         ## learning rate annealing: learning_rate shrinks by 1% after every tree
            sample_rate = 1.0,                   ## sample 80% of rows per tree
            col_sample_rate = 1.0,               ## sample 80% of columns per split
            ignore_const_cols = ignoreConstCols,
            stopping_rounds = 3, stopping_tolerance = 0.01, stopping_metric = "RMSE", 
            model_id = "gbm_housing_v1",         ## name the model in H2O
            seed = 333)                          ## Set the random seed for reproducability
#### performance of the model
h2o.performance(gbm, newdata = test)
#### Extract specific metric
h2o.rmsle(gbm, train = T)
#### Cross validation metrics
h2o.rmsle(h2o.performance(gbm, xval = T)) 
gbm@model$cross_validation_metrics_summary ## This gives you an idea of the variance between the folds
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
#### Alternative I :  combine models by averaging predictions  ####
###################################################################
#### Validation
predictValidateMean          <- validate.hex[,c("Id","SalePrice")]
predictValidateMean[,"rf"]   <- h2o.predict(object = rf,newdata = validate.hex)
predictValidateMean[,"gbm"]  <- h2o.predict(object = gbm,newdata = validate.hex)
predictValidateMean[,"glm"]  <- h2o.predict(object = glm,newdata = validate.hex)
predictValidateMean[,"mean"] <- h2o.mean(x = predictValidateMean[,2:4], axis = 1, return_frame = TRUE)
predictValidateMean[,"residual"] <- predictValidateMean[,"SalePrice"] - predictValidateMean[,"mean"]
predictValidateMean[,"expMean"] <- h2o.exp(predictValidateMean[,"mean"]) 
predictValidateMean[,"expSalePrice"] <- h2o.exp(predictValidateMean[,"SalePrice"]) 
h2o.make_metrics(predicted = predictValidateMean[,"expMean"], actuals = predictValidateMean[,"expSalePrice"])
#### Submission
submissionMean               <- test.hex[,c("Id")]
submissionMean[,"rf"]        <- h2o.predict(object = rf,newdata = test.hex)
submissionMean[,"gbm"]       <- h2o.predict(object = gbm,newdata = test.hex)
submissionMean[,"glm"]       <- h2o.predict(object = glm,newdata = test.hex)
submissionMean[,"mean"]      <- h2o.mean(x = submissionMean[,2:4], axis = 1, return_frame = TRUE)
submissionMean[,"SalePrice"] <- h2o.exp(submissionMean[,"mean"]) 
submissionMean               <- submissionMean[,c("Id","SalePrice")]  
h2o.exportFile(submissionMean, path = "/home/bart/submission.h2o.average.csv", force = T)
###################################################################
#### Alternative II : combine models in a stacked ensemble     ####
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
                                metalearner_fold_assignment = "Modulo",
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
print(sprintf("Best base-learner test RSMLE:  %s", baselearnerMaxRmsleTest))
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



###################################################################
#### Parameter tuning with grid search                         ####
###################################################################
## http://docs.h2o.ai/h2o/latest-stable/h2o-docs/grid-search.html#grid-search-in-r
## https://blog.h2o.ai/2016/06/hyperparameter-optimization-in-h2o-grid-search-random-search-and-the-future/
#### Hyper parameters
ntrees_opt            <- c(10,20,50,70,100)
mtries_opt            <- c(50,60,70,80,90)         
max_depth_opt         <- c(5,6,7,8,9,10)
hyper_params <- list(ntrees = ntrees_opt,
                     mtries = mtries_opt,
                     max_depth = max_depth_opt)
#### Search criteria
search_criteria <- list(strategy = "RandomDiscrete",
                        stopping_rounds = 3, stopping_tolerance = 0.001, stopping_metric = "deviance",
                        max_models = 500, 
                        seed = 333)
#### Grid search
rf_grid <- h2o.grid(algorithm = "drf",
                    grid_id = "rf_grid_housing",
                    training_frame =  train, 
                    nfolds =nFolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,
                    x=features,
                    y=response,
                    min_rows = 1,
                    ignore_const_cols = TRUE,
                    min_split_improvement = 1e-3,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)
#### Get best model
rf <- h2o.getModel(rf_grid@summary_table$model_ids[[1]])

perfGbmTest <- h2o.performance(rf, newdata = test)

learn_rate_opt        <- c(0.01, 0.02, 0.03,0.04,0.05,0.06,0.08)
max_depth_opt         <- c(2, 3, 4, 5)
col_sample_rate_opt   <- c(1.0)
sample_rate_opt       <- c(1.0)
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt,
                     col_sample_rate = col_sample_rate_opt,
                     sample_rate = sample_rate_opt)

search_criteria <- list(strategy = "RandomDiscrete", max_models = 50, seed = 333)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "gbm_grid_housing",
                     x = features,
                     y = response,
                     training_frame = train,
                     distribution = "AUTO",
                     ntrees = 100,
                     min_rows = 1,
                     min_split_improvement = 1e-3,
                     nfolds = nFolds,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     stopping_rounds = 1, stopping_tolerance = 0.01, stopping_metric = "RMSLE",
                     ignore_const_cols = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

gbm <- h2o.getModel(gbm_grid@summary_table$model_ids[[1]])

perfGbmTest <- h2o.performance(gbm, newdata = test)



glm <- h2o.glm(
              training_frame =  train, 
              nfolds = nFolds,
              fold_assignment = "Modulo", 
              keep_cross_validation_predictions = TRUE,
              x=features,          
              y=response,          
              family = "gaussian",
              link = "identity",
              standardize = TRUE,  
              remove_collinear_columns = TRUE,
              ignore_const_cols = ignoreConstCols,
              solver = "L_BFGS", # "L_BFGS" "COORDINATE_DESCENT"
              lambda_search = T,
              early_stopping = TRUE,
              max_iterations = 200,)


