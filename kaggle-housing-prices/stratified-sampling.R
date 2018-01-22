###################################################################
#### Dependencies                                              ####
###################################################################
require(caret)      # createDataPartition
###################################################################
#### Split data                                                ####
###################################################################
## split in train and test after engineering. Split by key is fasted method.
setkey(full.dt,"dataPartition") 
train.full.dt <- full.dt["train"]
test.dt <- full.dt["test"]
#Spliting training set into train and validate based on OverallQual
index <- createDataPartition(train.full.dt$OverallQual, p=0.80, list=FALSE)
train.dt <- train.full.dt[index,]
validate.dt <- train.full.dt[-index,]




