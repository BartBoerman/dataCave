require(caret)
require(caretEnsemble)
require(foreach)
require(iterators)
require(parallel)
require(doParallel)
cluster <- makeCluster(detectCores())  # leave one for OS
registerDoParallel(cluster)
## https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html
my_control <- trainControl(
  method="cv",
  number=7,
  savePredictions="final",
#  index=createResample(train.dt$OverallQual, 7),  
  allowParallel =TRUE
)

features.back  <- features
#### Remove correlated 
features <- setdiff(features, c(
"GarageCond",    ## correlated with GarageQual
"GarageYrBlt",   ## correlated with GarageQual
"GarageArea",    ## correlated with GarageCars
"TotRmsAbvGrd",  ## correlated with GrLivArea
"YearRemodAdd", 
"FirstFlrSF"
))

formula.all <- as.formula(paste("SalePrice ~ ", paste(features, collapse= "+"))) 

###################################################################

require(randomForest)
#require(xgboost)
require(glmnet)
# require(gbm)
#library(kernlab)       # support vector machine 
#require(elasticnet)  # ridge regressing
glmnetGridElastic <- expand.grid(.alpha = 0.3, .lambda = 0.009) ## notice the . before the parameter
glmnetGridLasso <- expand.grid(.alpha = 1, .lambda = seq(0.001,0.1,by = 0.001))
glmnetGridRidge <- expand.grid(.alpha = 0, .lambda = seq(0.001,0.1,by = 0.001))
rfGrid <- expand.grid(.mtry = seq(35,65,by = 5))
set.seed(333)
model_list <- caretList(
                  formula.all, 
                  data=train.dt,
                  trControl=my_control,
                  metric="RMSE",
                  tuneList=list(
                          ## Do not use custom names in list. This will give prediction error with greedy ensemble. Bug in caret.
                          # rf=caretModelSpec(method="rf", .mtry = 57), ## , tuneGrid=data.frame(.mtry=2), preProcess="pca"
                          glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridElastic), ## Elastic  , tuneLength = 6
                          glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridLasso), ## Lasso
                          glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGridRidge), ## Lasso
                          rf = caretModelSpec(method="rf",  tuneGrid = rfGrid)
                          )
)
####
xyplot(resamples(model_list))
modelCor(resamples(model_list))
summary(resamples(model_list))
####
greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="RMSE",
  trControl=trainControl(
    number=7, method = "cv"
  ))

summary(greedy_ensemble)
#### 
require(Metrics)
rmse(log(expm1(validate.dt$SalePrice)), log(expm1(predict(greedy_ensemble, newdata=validate.dt))))

#### submit
finalPredictions <- predict(greedy_ensemble, newdata=test.dt)
finalPredictions <- data.frame(finalPredictions)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- expm1(finalPredictions$SalePrice) 
submission <- cbind(test.dt[, "Id"],finalPredictions)
write.csv(x = submission, file = "/home/bart/submission_greedy_ensemble.csv", row.names = F)

## leaderboard 0.12414 without feature engineering and dummies. And convert ordinal to integers.


## http://xgboost.readthedocs.io/en/latest/parameter.html
## https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
## verbose = 2, also print information about tree

## convert data table into a matrix
train.xgb.DMatrix <- xgb.DMatrix(data.matrix(train.dt[,1:80]), label=train.dt$SalePrice)
validate.xgb.DMatrix <- xgb.DMatrix(data.matrix(validate.dt[,1:80]), label=validate.dt$SalePrice)
test.xgb.DMatrix <- xgb.DMatrix(data.matrix(test.dt[,1:80]))

set.seed(333)
xgboostTreeBooster <- xgboost(data = train.xgb.DMatrix, 
                      eval_metric= "rmse", 
                      max.depth = 3, 
                      eta = 0.1, 
                      min_child_weight = 4, 
                      nround = 100, 
                      subsample = 1.0,
                      colsample_bytree = 1.0,
                      nthread = 8,  
                      verbose = 0, 
                      booster = "gbtree",
                      objective = "reg:linear")

require(Metrics)
rmse(log(expm1(validate.dt$SalePrice)), log(expm1(predict(xgboostFit, validate.xgb.DMatrix))))
rmsle(expm1(validate.dt$SalePrice), expm1(predict(xgboostFit, validate.xgb.DMatrix)))


importance_matrix <- xgb.importance(model = xgboostFit,
                                    feature_names = names(train.dt[,1:80]))
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,top_n = 20)

#### submit xgboost
finalPredictions <- predict(xgboostTreeBooster, test.xgb.DMatrix)
finalPredictions <- data.frame(finalPredictions)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- expm1(finalPredictions$SalePrice) 
submission <- cbind(test.dt[, "Id"],finalPredictions)
write.csv(x = submission, file = "/home/bart/submission_xgboostTreeBooster.csv", row.names = F)








################################################### 
####  return to single threaded processing     #### 
###################################################
stopCluster(cluster)
registerDoSEQ()






model_pred <- NULL
model_pred <- predict(model_list, newdata=validate.dt)
model_pred <- data.frame(model_pred)
ensemble_pred <- predict(greedy_ensemble, newdata=validate.dt)
model_pred$ensemble <- ensemble_pred
model_pred$actual <- validate.dt$SalePrice


### stack with xgboost

xgbTree_ensemble <- caretStack(
  model_list,
  method="xgbTree",
  metric="RMSE",
  tuneLength=10,
  trControl=trainControl(
    method="cv",
    number=7,
    savePredictions="final"
  ),
  nthread =8,
  objective = "reg:linear"
)




#### stack
glm_ensemble <- caretStack(
          model_list,
          method="glm",
          metric="RMSE",
          tuneLength=10,
          trControl=trainControl(
            method="cv",
            number=7,
            savePredictions="final"
  )
)


GLMensemble_pred <- predict(glm_ensemble, newdata=validate.dt)
model_pred$GLMensemble <- GLMensemble_pred

rmsle(model_pred$actual, model_pred$GLMensemble)
rmsle(model_pred$actual, model_pred$ensemble)
rmsle(model_pred$actual, model_pred$glm)
rmsle(model_pred$actual, model_pred$rf)

require("gbm")
gbm_ensemble <- caretStack(
          model_list,
          method="gbm",
          verbose=FALSE,
          tuneLength=10,
          metric="RMSE",
          trControl=trainControl(
            method="cv",
            number=7,
            savePredictions="final"
  )
)


GBMensemble_pred <- predict(gbm_ensemble, newdata=validate.dt)
model_pred$GBMensemble <- GBMensemble_pred

rmsle(model_pred$actual, model_pred$GBMensemble)
rmsle(model_pred$actual, model_pred$GLMensemble)
rmsle(model_pred$actual, model_pred$ensemble)
rmsle(model_pred$actual, model_pred$glm)
rmsle(model_pred$actual, model_pred$rf)




set.seed(333)
require(gbm)

gbmFit <- gbm(formula = formula.all,
              distribution = "gaussian",
              data = train.dt,
              n.trees = 1000,
              interaction.depth = 12,
              n.minobsinnode = 4, # 0.01
              shrinkage = 0.001, # 0.001
              bag.fraction = 0.9,
              train.fraction = 0.8, # 0.9??
              cv.folds=3) 



gbmPredict <- predict(gbmFit, newdata=validate.dt)
model_pred$gbmFit <- gbmPredict


rmsle(model_pred$actual, model_pred$GBMensemble)
rmsle(model_pred$actual, model_pred$gbmFit)
rmsle(model_pred$actual, model_pred$ensemble)
rmsle(model_pred$actual, model_pred$glm)
rmsle(model_pred$actual, model_pred$rf)
