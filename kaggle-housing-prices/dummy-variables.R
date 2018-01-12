###############################################################################
#### Convert categorical (factor) variables into dummy/indicator variables ####                                                   ####
###############################################################################
require(caret)      # (near) zero variance and dummyVars
set.seed(333)
f <- paste('~', paste(variablesFactor, collapse = ' + '))
encoder <- dummyVars(as.formula(f), full.dt, fullRank = T, drop2nd = T)
full.dummyVars.dt <- as.data.table(predict(encoder, full.dt))
full.dt <- cbind(full.dt, full.dummyVars.dt)
## remove factor columns will decrease performance. We need to pick and chose.
## full.dt <- full.dt[, (variablesFactor):=NULL]
