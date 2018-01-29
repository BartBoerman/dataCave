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

formula.all <- as.formula(paste("SalePrice ~ ", paste(features, collapse= "+"))) 

require(randomForest)
require(xgboost)
require(glmnet)
#library(kernlab)       # support vector machine 
glmnetGrid <- expand.grid(alpha = 0.1, lambda = 0.0297056)
rfGrid <- expand.grid(mtry = 57)
model_list <- caretList(
                  formula.all, 
                  data=train.dt,
                  trControl=my_control,
                  metric="RMSE",
                  tuneList=list(
                          rf=caretModelSpec(method="rf", .mtry = 57), ## , tuneGrid=data.frame(.mtry=2), preProcess="pca"
                          glmnet=caretModelSpec(method="glmnet", tuneGrid = glmnetGrid)
            )
)

## http://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
## verbose = 2, also print information about tree
bst <- xgboost(data = as.matrix(train.dt), max.depth = 6, eta = 0.3, subsample = 0.8, nthread = 8, nround = 3, objective = "linear", verbose = 2)


################################################### 
####  return to single threaded processing     #### 
###################################################
stopCluster(cluster)
registerDoSEQ()


p <- as.data.frame(predict(model_list, newdata=head(validate.dt)))
print(p)


xyplot(resamples(model_list))

modelCor(resamples(model_list))

greedy_ensemble <- caretEnsemble(
                    model_list, 
                    metric="RMSE",
                    trControl=trainControl(
                      number=7, method = "cv"
  ))
summary(greedy_ensemble)

model_pred <- predict(model_list, newdata=validate.dt)
model_pred <- data.frame(model_pred)
ensemble_pred <- predict(greedy_ensemble, newdata=validate.dt)
model_pred$ensemble <- ensemble_pred
model_pred$actual <- validate.dt$SalePrice

require(Metrics)
rmsle(model_pred$actual, model_pred$ensemble)
rmsle(model_pred$actual, model_pred$glm)
rmsle(model_pred$actual, model_pred$rf)

#### submit
finalPredictions <- predict(greedy_ensemble, newdata=test.dt)
finalPredictions <- data.frame(finalPredictions)
names(finalPredictions) <- "SalePrice"
finalPredictions$SalePrice <- expm1(finalPredictions$SalePrice) 
submission <- cbind(test.dt[, "Id"],finalPredictions)
write.csv(x = submission, file = "/home/bart/submission_caret_ensemble.csv", row.names = F)

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


