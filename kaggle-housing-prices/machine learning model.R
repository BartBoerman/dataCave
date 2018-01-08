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
  ip = "192.168.1.215",
  port = 54321
) 
###################################################################
#### Select features                                           ####
###################################################################
features <- setdiff(names(full.dt), c(response, "Id","SalePrice","dataPartition")) 
###################################################################
#### Pre-processing                                            ####
###################################################################
## log transform (including response variable)
## scale
## remove variables with zero variance
###################################################################
#### Split data                                                ####
###################################################################
## split in train and test after engineering. Split by key is fasted method.
setkey(full.dt,"dataPartition") 
train.dt <- full.dt["train"]
test.dt <- full.dt["test"]
## upload to h2o cloud which converts data to a h2o data frame (hex)
train.hex <- as.h2o(train.dt,"train.hex")
test.hex <- as.h2o(test.dt,"test.hex")
## split training data into training and validation data sets
splits <- h2o.splitFrame(
  train.hex,           ##  splitting the H2O frame we read above
  c(0.8),   
  seed=333)    ##  setting a seed will ensure reproducible results (not R's seed)

train.hex <- h2o.assign(splits[[1]], "train.hex")   
validate.hex <- h2o.assign(splits[[2]], "valid.hex")  


