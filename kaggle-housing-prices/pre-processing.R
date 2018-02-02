###################################################################
#### Dependencies                                              ####
###################################################################
require(caret)      # (near) zero variance and dummyVars
###################################################################
#### Pre-processing                                            ####
###################################################################
##### remove outliers
outliersHigh.Id <-  train.dt[GrLivArea > 4000 | LotArea > 100000 | X1stFlrSF > 3000 | GarageArea > 1200,Id]
#outliersLowPrice.Id <-  train.dt[(GarageArea > 1000 & SalePrice  < 200000) |
#                            (GrLivArea > 4000 & SalePrice   < 200000) | 
#                            (TotalBsmtSF > 6000 & SalePrice < 200000) |
#                            (TotRmsAbvGrd > 12 & SalePrice  < 300000), Id]
full.dt <- full.dt[!(Id %in% outliersHigh.Id)]
#### create index for splitting data 
setkey(full.dt,dataPartition)
train.full.dt <- full.dt["train"]
index <- createDataPartition(train.full.dt$YrSold, p=0.80, list=FALSE)
#### remove unwanted variables
#full.dt[, (variablesDrop):=NULL]
#full.dt[, (variablesFactor):=NULL]
##### ordinal factors
## convert ordinal factors to integers. h2o does not support ordered factors, in addition, it gives a small performance boost for glmnet and xgbTree.
changeColType <- setDT(data.frame(sapply(full.dt,is.ordered)), keep.rownames = TRUE)[sapply.full.dt..is.ordered.==TRUE]$rn
full.dt[,(changeColType):= lapply(.SD, as.integer), .SDcols = changeColType]
#### remove variables with zero variance
#### skewed variables
## log transform skewed variables (including response variable)
skewedVariables <- sapply(full.dt[, c(variablesSquareFootage,variablesValues), with = FALSE],function(x){skew(x,na.rm=TRUE)})
## keep only features that exceed a threshold for skewness
skewedVariables <- skewedVariables[skewedVariables > 0.50]
## transform excessively skewed features with log
skewedVariables <- names(skewedVariables)
full.dt[, (skewedVariables) := lapply(.SD, function(x) log1p(x)), .SDcols = skewedVariables]
#### scale (Done in h20 algorithm )
## scale (exluding response)
varScale <- setdiff(c(variablesSquareFootage, variablesValues), c(response)) ## Do not scale response
full.dt[, (varScale) := lapply(.SD, function(x) scale(x, center = T, scale = T)), .SDcols = varScale]
###################################################################
#### Select features                                           ####
###################################################################
features <- setdiff(names(full.dt), c(response, variablesDrop, "Id","dataPartition")) 
###################################################################
#### Split data                                                ####
###################################################################
## split in train and test after engineering. Split by key is fasted method.
train.full.dt <- full.dt["train"]
test.dt <- full.dt["test"]
#Spliting training set into train and validate based on distribution
#train.dt <- train.full.dt[index,]
#validate.dt <- train.full.dt[-index,]
## random split training into train and validate
set.seed(333)
n <- nrow(train.full.dt)
shuffled.dt <- train.full.dt[sample(n), ]
train_indices <- 1:round(0.8 * n)
train.dt <- shuffled.dt[train_indices, ]
validate_indices <- (round(0.8 * n) + 1):n
validate.dt <- shuffled.dt[validate_indices, ]

