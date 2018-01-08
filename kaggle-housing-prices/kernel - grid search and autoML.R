
## https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/gbm-randomforest/GBM_RandomForest_Example.R
## https://blog.h2o.ai/2016/06/h2o-gbm-tuning-tutorial-for-r/
############################################### 
####  Data and dependencies                 ### 
###############################################
require(h2o)
## Create an H2O cloud 
h2o.connect(
  ip = "192.168.1.215",
  port = 54321
) 
## h2o.removeAll()        ## clean slate
## Load a file from disk
setwd("~/Data Science/Competitions/House prices")
filePathCent <- "C:/Users/Administrator/Documents/Data Science/Competitions/House prices/" 
train.hex <- h2o.importFile(path = "C:/Users/Administrator/Documents/Data Science/Competitions/House prices/train.csv",
                            destination_frame = "train.hex",
                            header = TRUE,
                            sep = ",",
                            na.strings = "NA")
test.hex <- h2o.importFile(path = "C:/Users/Administrator/Documents/Data Science/Competitions/House prices/test.csv",
                            destination_frame = "test.hex",
                            header = TRUE,
                            sep = ",",
                            na.strings = "NA")
test.hex$SalePrice      <- NA
test.hex$dataPartition  <- "test"
train.hex$dataPartition  <- "train"

full.hex <- as.h2o(h2o.rbind(train.hex,test.hex),destination_frame = "full.hex")
h2o.describe(full.hex)[, 1:8]
h2o.str(full.hex)

## Some engineering

full.hex$MSSubClass  <- h2o.asfactor(full.hex$MSSubClass)
full.hex$OverallQual <- h2o.asfactor(full.hex$OverallQual)
full.hex$OverallCond <- h2o.asfactor(full.hex$OverallCond)
full.hex$SalePrice   <- h2o.log(full.hex$SalePrice)

## Split data after engineering into original data sets

train.hex <- full.hex[(full.hex[,"dataPartition"] == "train"),]
test.hex <- full.hex[(full.hex[,"dataPartition"] == "test"),]

## split training data into training and validation 
splits <- h2o.splitFrame(
  train.hex,           ##  splitting the H2O frame we read above
  c(0.8),   
  seed=333)    ##  setting a seed will ensure reproducible results (not R's seed)

train.hex <- h2o.assign(splits[[1]], "train.hex")   
validate.hex <- h2o.assign(splits[[2]], "valid.hex")  

## set response (target) variable
response <- "SalePrice"
     
## use all other variables (except for the name) as features
features <- setdiff(names(full.hex), c(response, "Id","dataPartition")) 

## Random Forest
rf <- h2o.randomForest(
            training_frame = train.hex,          ## the H2O frame for training
            validation_frame = validate.hex,     ## the H2O frame for validation (not required)
            x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
            y=response,                          ## what we are predicting,alternativaly, e.g. 81
            model_id = "rf_housing_v1",          ## name the model in H2O
            seed = 333)                          ## Set the random seed for reproducability

## performance of the model
h2o.performance(rf, newdata = validate.hex)

## plot variable importance

h2o.varimp_plot(rf)

## Gradient Boosting Machine (GBM)
gbm <- h2o.gbm(
            training_frame = train.hex,          ## the H2O frame for training
            validation_frame = validate.hex,     ## the H2O frame for validation (not required)
            x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
            y=response,                          ## what we are predicting,alternativaly, e.g. 81
            #nfolds = 3,
            ntrees = 10000, 
            #learn_rate=0.01,
            learn_rate_annealing = 0.99,         ## learning rate annealing: learning_rate shrinks by 1% after every tree
            sample_rate = 0.8,                   ## sample 80% of rows per tree
            col_sample_rate = 0.8,               ## sample 80% of columns per split
            stopping_rounds = 5, stopping_tolerance = 0.01, stopping_metric = "RMSLE", 
            score_tree_interval = 10,              ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)   
            model_id = "gbm_housing_v1",         ## name the model in H2O
            seed = 333)                          ## Set the random seed for reproducability

## Parameter search for max_depth
hyper_params = list(max_depth = seq(1,14,1) )

grid <- h2o.grid(
            hyper_params = hyper_params,   ## hyper parameters
            search_criteria = list(strategy = "Cartesian"), ## full Cartesian hyper-parameter search
            algorithm="gbm",   ## which algorithm to run
            grid_id="gbm_depth_grid",  ## identifier for the grid, to later retrieve it
            ## standard model parameters
            training_frame = train.hex,          ## the H2O frame for training
            validation_frame = validate.hex,     ## the H2O frame for validation (not required)
            x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
            y=response,                          ## what we are predicting,alternativaly, e.g. 81
            #nfolds = 3,
            ntrees = 10000, 
            learn_rate=0.01,
            #learn_rate_annealing = 0.99,         ## learning rate annealing: learning_rate shrinks by 1% after every tree
            sample_rate = 0.8,                   ## sample 80% of rows per tree
            col_sample_rate = 0.8,               ## sample 80% of columns per split
            stopping_rounds = 5, stopping_tolerance = 0.01, stopping_metric = "RMSLE", 
            score_tree_interval = 10,              ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)   
            seed = 333)                          ## Set the random seed for reproducability

gbm <- h2o.getModel("gbm_depth_grid_model_3")

## performance of the model
h2o.performance(gbm, newdata = train.hex)
h2o.performance(gbm, newdata = validate.hex)

## find the range of max_depth for the top 5 models
topDepths = grid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))

hyper_params = list( 
            ## restrict the search to the range of max_depth established above
            max_depth = seq(minDepth,maxDepth,1),                                      
            ## search a large space of row sampling rates per tree
            sample_rate = seq(0.2,1,0.01),                                             
            ## search a large space of column sampling rates per split
            col_sample_rate = seq(0.2,1,0.01),                                         
            ## search a large space of column sampling rates per tree
            col_sample_rate_per_tree = seq(0.2,1,0.01),                                
            ## search a large space of how column sampling per split should change as a function of the depth of the split
            col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
            ## search a large space of the number of min rows in a terminal node
            min_rows = 2^seq(0,log2(nrow(train.hex))-1,1),                                 
            ## search a large space of the number of bins for split-finding for continuous and integer columns
            nbins = 2^seq(4,10,1),                                                     
            ## search a large space of the number of bins for split-finding for categorical columns
            nbins_cats = 2^seq(4,12,1),                                                
            ## search a few minimum required relative error improvement thresholds for a split to happen
            min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
            ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
            histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)
search_criteria = list(
            ## Random grid search
            strategy = "RandomDiscrete",      
            ## limit the runtime to 10 minutes
            max_runtime_secs = 600,         
            ## build no more than 100 models
            max_models = 100,                  
            ## random number generator seed to make sampling of parameter combinations reproducible
            seed = 333,                        
            ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
            stopping_rounds = 5,                
            stopping_metric = "RMSLE",
            stopping_tolerance = 1e-3
)
grid <- h2o.grid(
            ## hyper parameters
            hyper_params = hyper_params,
            ## hyper-parameter search configuration (see above)
            search_criteria = search_criteria,
            ## which algorithm to run
            algorithm = "gbm",
            ## identifier for the grid, to later retrieve it
            grid_id = "gbm_final_grid", 
            ## standard model parameters
            x = features, 
            y = response, 
            training_frame = train.hex, 
            validation_frame = validate.hex,
            ## more trees is better if the learning rate is small enough
            ## use "more than enough" trees - we have early stopping
            ntrees = 10000,                                                            
            ## smaller learning rate is better
            ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
            learn_rate = 0.05,                                                         
            ## learning rate annealing: learning_rate shrinks by 1% after every tree 
            ## (use 1.00 to disable, but then lower the learning_rate)
            learn_rate_annealing = 0.99,                                               
            ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
            max_runtime_secs = 600,                                                 
            ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
            stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "RMSLE", 
            ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
            score_tree_interval = 10,                                                
            ## base random number generator seed for each model (automatically gets incremented internally for each model)
            seed = 333                                                             
)
gbm <- h2o.getModel("final_grid_model_15")

## performance of the model
h2o.performance(gbm, newdata = validate.hex)

## Show a detailed summary of the cross validation metrics
## This gives you an idea of the variance between the folds
gbm@model$cross_validation_metrics_summary
h2o.rmse(h2o.performance(gbm, xval = T))


## make final predictions

finalPredictions <- h2o.predict(
  object = gbm
  ,newdata = test.hex)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- h2o.exp(finalPredictions$SalePrice) 
submission <- h2o.cbind(test.hex[, "Id"],finalPredictions)
h2o.exportFile(submission, path = "submission.h2o.gbm.csv", force = T)


autoMl <- h2o.automl(
  training_frame = train.hex,        ## the H2O frame for training
  validation_frame = validate.hex,      ## the H2O frame for validation (not required)
  x=2:80,                        ## the predictor columns, by column index
  y=81,                          ## the target index (what we are predicting)
  stopping_metric = "RMSE",
  nfolds = 3,
  seed = 333,
  max_runtime_secs = 3600,
  stopping_rounds = 2,
  stopping_tolerance = 0.01,
  project_name = "KaggleHousingPrices"
  )

finalPredictions <- h2o.predict(
  object = autoMl@leader
  ,newdata = test.hex)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- h2o.exp(finalPredictions$SalePrice) 
submission <- h2o.cbind(test.hex[, "Id"],finalPredictions)
h2o.exportFile(submission, path = "submission.h2o.autoMl.csv", force = T)

y <- h2o.getModel("GBM_grid_0_AutoML_20171229_121243_model_2")

## StackedEnsemble_AllModels_0_AutoML_20171229_122949


