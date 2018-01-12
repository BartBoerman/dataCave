###############################################################################
#### Convert categorical (factor) variables into dummy/indicator variables ####                                                   ####
###############################################################################
set.seed(333)
f <- paste('~', paste(variablesFactor, collapse = ' + '))
encoder <- dummyVars(as.formula(f), full.dt, fullRank = T, drop2nd = T)
full.dummyVars.dt <- as.data.table(predict(encoder, full.dt))
full.dt <- cbind(full.dt, full.dummyVars.dt)
