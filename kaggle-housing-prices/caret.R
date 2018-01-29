require(caret)
require(caretEnsemble)
require(foreach)
require(iterators)
require(parallel)
require(doParallel)
cluster <- makeCluster(detectCores()-1)  # leave one for OS
registerDoParallel(cluster)
## https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  index=createResample(train.dt$OverallQual, 20),
  allowParallel =TRUE
)

formula.all <- as.formula(paste("SalePrice ~ ", paste(features, collapse= "+"))) 

model_list <- caretList(
  formula.all, data=train.dt,
  trControl=my_control,
  methodList=c("svmRadial", "glm","xgbTree")
)

model_list <- caretList(
            formula.all, 
            data=train.dt,
            trControl=my_control,
            metric="RSME",
            tuneList=list(
              rf=caretModelSpec(method="rf", tuneLength=6), ## tuneGrid=data.frame(.mtry=2), preProcess="pca"
              glm=caretModelSpec(method="glm", tuneLength=6)
            )
)



################################################### 
####  return to single threaded processing     #### 
###################################################
stopCluster(cluster)
registerDoSEQ()


p <- as.data.frame(predict(model_list, newdata=head(validate.dt)))
print(p)




