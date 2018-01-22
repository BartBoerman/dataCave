###################################################################
#### Determine principle components on train data              ####
###################################################################
tmp.dt <- train.dt[,c(features), with = F]
tmp.dt[,(features):= lapply(.SD, as.numeric), .SDcols = features]
train.pcomp <- prcomp(tmp.dt, scale. = FALSE, tol = 0.01)
###################################################################
#### Analyse PCA                                               ####
###################################################################
summary(train.pcomp)
train.pcomp$rotation

# Eigenvalues
eig <- (train.pcomp$sdev)^2
# Variances in percentage
variance <- eig*100/sum(eig)
# Cumulative variances
cumvar <- cumsum(variance)
eig.val <- data.frame(eig = eig, variance = variance, cumvariance = cumvar)

# The importance of princpal components (PCs) can be visualized with a scree plot.
barplot(eig.val[, 2], names.arg=1:nrow(eig.val), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), 
      eig.val[, 2], 
      type="b", pch=19, col = "red")


# An eigenvalue > 1 indicates that PCs account for more variance than accounted by one of the original variables in standardized data. This is commonly used as a cutoff point for which PCs are retained.
# Note that, a good dimension reduction is achieved when the the first few PCs account for a large proportion of the variability (80-90%).

# Eigenvalues with factoextra
require(factoextra)
eig.val <- get_eigenvalue(train.pcomp)
eig.val 



###################################################################
#### Determine principle components on test data               ####
###################################################################



###test.pcomp <- predict(train.pcomp, newdata=test.data[,3:columns])