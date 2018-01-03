#### Reference
## https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/gbm-randomforest/GBM_RandomForest_Example.R
## https://blog.h2o.ai/2016/06/h2o-gbm-tuning-tutorial-for-r/
## http://www.listendata.com/2016/10/r-data-table.html
## http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/
## https://www.kaggle.com/jimthompson/regularized-linear-models-in-r ## skewness
## https://www.kaggle.com/nick898/descriptive-statistics-for-housing-prices ## statistics
## https://www.kaggle.com/sidraina89/regularized-regression-housing-pricing
## https://www.kaggle.com/michaldatberg/dataexploration-and-modeling-with-h2o
## http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm
## https://hulpbijonderzoek.nl/online-woordenboek/scheefheid/ ## skewness dutch
## https://alstatr.blogspot.nl/2013/06/measures-of-skewness-and-kurtosis.html ## Measures of Skewness and Kurtosis
## http://www.r-tutor.com/elementary-statistics/numerical-measures/skewness ## skewness

#### Dependencies
require(data.table) # fast data wrangling
require(h2o)        # machine learning algorithmes
require(psych)      # descriptive statistics, skewness and kurtosis
train.dt <- fread(input = "train.csv", 
                  sep = ",", 
                  nrows = -1,
                  header = T,
                  na.strings=c("NA","N/A","null"),
                  stringsAsFactors = F,
                  check.names = T,
                  strip.white = T,
                  blank.lines.skip = T,
                  data.table = T
) 
test.dt <- fread(input = "test.csv", 
                 sep = ",", 
                 nrows = -1,
                 header = T,
                 na.strings=c("NA","N/A","null"),
                 stringsAsFactors = F,
                 check.names = T,
                 strip.white = T,
                 blank.lines.skip = T,
                 data.table = T
) 
## Create one data set for feature engineering. 
train.dt[, dataPartition:="train"]
test.dt[, SalePrice:=NA] 
test.dt[, dataPartition:="test"]
full.dt <- rbindlist(list(train.dt, test.dt), use.names = F, fill = F)
#### Data dictionary
## Data types
variableTypes.df <- cbind(as.data.frame(names(full.dt)),as.data.frame(sapply(full.dt, class)))
names(variableTypes.df) <- c("variable","type")
## Numeric, square footage
variablesSquareFootage <- c(
  "LotFrontage", 		## Linear feet of street connected to property 
  "LotArea",    		## Lot size in square feet
  "MasVnrArea",  		## Masonry veneer area in square feet
  "BsmtFinSF1",		  ## Type 1 finished square feet	
  "BsmtFinSF2",		  ## Type 2 finished square feet
  "BsmtUnfSF",		  ## Unfinished square feet of basement area
  "TotalBsmtSF", 		## Total square feet of basement area
  "FirstFlrSF",		  ## First Floor square feet
  "SecondFlrSF",	  ## Second floor square feet
  "LowQualFinSF", 	## Low quality finished square feet (all floors)
  "GrLivArea", 		  ## Above grade (ground) living area square feet
  "GarageArea",     ## Size of garage in square feet
  "WoodDeckSF",     ## Wood deck area in square feet
  "OpenPorchSF",    ## Open porch area in square feet  
  "EnclosedPorch",  ## Enclosed porch area in square feet 
  "ThreeSsnPorch",  ## Three season porch area in square feet 
  "ScreenPorch",    ## Screen porch area in square feet
  "PoolArea" 		    ## Pool area in square feet
)
## Counts, a house has n of something
variablesCounts <- c(
  "BsmtFullBath",		## Basement full bathrooms
  "BsmtHalfBath",		## Basement half bathrooms
  "FullBath",			  ## Full bathrooms above grade
  "HalfBath",			  ## Half baths above grade
  "BedroomAbvGr",		## Bedrooms above grade (does NOT include basement bedrooms)
  "KitchenAbvGr",		## Kitchens above grade
  "TotRmsAbvGrd",		## Total rooms above grade (does not include bathrooms)
  "Fireplaces",		  ## Number of fireplaces
  "GarageCars"     	## Size of garage in car capacity
)
## Values
variablesValues <- c(
  "MiscVal",        ## $ Value of miscellaneous feature
  "SalePrice"       ## $ Price paid
)
## Factors
variablesFactor <- colnames(full.dt)[which(as.vector(full.dt[,sapply(full.dt, class)]) == "character")]
variablesFactor <- c(variablesFactor,
                     "MSSubClass",     ## Identifies the type of dwelling involved in the sale
                     "OverallQual",    ## Rates the overall material and finish of the house
                     "OverallCond"     ## Rates the overall condition of the house
)
#### Data engineering
## In R first character can not be a number in variable names
setnames(full.dt, c("X1stFlrSF","X2ndFlrSF","X3SsnPorch"), c("FirstFlrSF","SecondFlrSF","ThreeSsnPorch"))
## Set columns to numeric
changeColType <- c(variablesSquareFootage, variablesCounts, variablesValues)
full.dt[,(changeColType):= lapply(.SD, as.numeric), .SDcols = changeColType]
## Set columns to factor
changeColType <- variablesFactor
full.dt[,(changeColType):= lapply(.SD, as.factor), .SDcols = changeColType]
#### Descriptive statistics
## statisticts
describe(full.dt[, c(variablesSquareFootage,variablesValues), with = FALSE]) ## from psych package 
## na values
countIsNA <- sapply(full.dt,function(x)sum(is.na(x)))
countIsNA.df <- data.frame(countIsNA)
countIsNA.df <- data.frame(variableName = row.names(countIsNA.df), countIsNA.df,row.names = NULL)
## skewness of numerical variables
# for numeric feature with excessive skewness, perform log transformation
# first get data type for each feature
feature_classes <- sapply(names(full.dt),function(x){class(full.dt[[x]])})
numeric_feats <-names(feature_classes[feature_classes != "character"])
# determine skew
skewedVariables<- sapply(full.dt[, c(variablesSquareFootage,variablesValues), with = FALSE],function(x){skew(x,na.rm=TRUE)})
# keep only features that exceed a threshold for skewness
skewedVariables <- skewedVariables[skewedVariables > 0.75]

# transform excessively skewed features with log(x + 1)
#for(x in names(skewed_feats)) {
#  all_data[[x]] <- log(all_data[[x]] + 1)
#}




## Scale data
# full.dt <- full.dt[ , (variablesSquareFootage) := lapply(.SD, scale), .SDcols = variablesSquareFootage]

## log SalePrice
#full.dt <- full.dt[ , SalePriceLog := log(SalePrice)]

#### Select features
## Response (target) variable
response <- "SalePrice"
## All features
features <- setdiff(names(full.dt), c(response, "Id","SalePrice","dataPartition")) 
## Below grade?
## Below grade in real estate is a term that describes a space that is below ground level – usually referred to as a basement.
## You could have a basement floor that is only one foot under the ground and it would still be a below grade floor.


## split in train and test after engineering. Split by key is fasted method.
setkey(full.dt,"dataPartition") 
train.dt <- full.dt["train"]
test.dt <- full.dt["test"]

#### Create an H2O cloud 
h2o.connect(
  ip = "192.168.1.215",
  port = 54321
) 
## h2o.removeAll()        ## clean slate

train.hex <- as.h2o(train.dt,"train.hex")
test.hex <- as.h2o(test.dt,"test.hex")

h2o.describe(train.hex)[,1:4]

## split training data into training and validation 
splits <- h2o.splitFrame(
  train.hex,           ##  splitting the H2O frame we read above
  c(0.8),   
  seed=333)    ##  setting a seed will ensure reproducible results (not R's seed)

train.hex <- h2o.assign(splits[[1]], "train.hex")   
validate.hex <- h2o.assign(splits[[2]], "valid.hex")  


## Gradient Boosting Machine (GBM)
gbm <- h2o.gbm(
  training_frame = train.hex,          ## the H2O frame for training
  validation_frame = validate.hex,     ## the H2O frame for validation (not required)
  x=features,                          ## the predictor columns, alternativaly by column index, e.g. 2:80
  y=response,                          ## what we are predicting,alternativaly, e.g. 81
  nfolds = 3,
  ntrees = 50, 
  learn_rate=0.05,
  learn_rate_annealing = 0.99,         ## learning rate annealing: learning_rate shrinks by 1% after every tree
  sample_rate = 0.8,                   ## sample 80% of rows per tree
  col_sample_rate = 0.8,               ## sample 80% of columns per split
  stopping_rounds = 5, stopping_tolerance = 0.01, stopping_metric = "RMSLE", 
  score_tree_interval = 10,              ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)   
  model_id = "gbm_housing_v1",         ## name the model in H2O
  seed = 333)                          ## Set the random seed for reproducability

## performance of the model
h2o.performance(gbm, newdata = train.hex)
h2o.performance(gbm, newdata = validate.hex)

## Show a detailed summary of the cross validation metrics
## This gives you an idea of the variance between the folds
h2o.rmsle(gbm, train = T)
h2o.rmsle(gbm, valid = T)

## Variable importance
h2o.varimp_plot(gbm)
varImportance <- h2o.varimp(gbm)
varImportance.df <- as.data.frame(cbind(varImportance$variable, varImportance$percentage))
names(varImportance.df) <- c("variable","importance")
varImportance.df$importance <- round(as.numeric(as.character(varImportance.df$importance)),3)
write.csv(varImportance.df, file = "varImp.csv")

## make final predictions
# Test/Validation dataset column 'MSSubClass' has levels not trained on: [150]
finalPredictions <- h2o.predict(
  object = gbm
  ,newdata = test.hex)
names(finalPredictions) <- "SalePrice"
#finalPredictions$SalePrice <- h2o.exp(finalPredictions$SalePrice) 
submission <- h2o.cbind(test.hex[, "Id"],finalPredictions)
h2o.exportFile(submission, path = "submission.h2o.gbm.csv", force = T)
