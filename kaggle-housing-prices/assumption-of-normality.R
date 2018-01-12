## https://stats.stackexchange.com/questions/52293/r-qqplot-how-to-see-whether-data-are-normally-distributed
###############################################################################
#### Skewness and kurtosis                                                 ####                                                   ####
###############################################################################
## http://rcompanion.org/handbook/I_12.html

require(e1071)

full.dt <- full.dt[!(Id %in% outliers.Id),]

x <- na.omit(full.dt$SalePrice)

# skewness and kurtosis, they should be around (0,3)
skewness(x)
kurtosis(x)

# histogramm
hist(x)

# qq-plot: you should observe a good fit of the straight line
qqnorm(x,  ylab="Sample Quantiles for x")
qqline(x,  col="red")

# p-plot: you should observe a good fit of the straight line
probplot(x, qdist=qnorm)

require(car)

  qqPlot(x, distribution="norm")


require(rcompanion)
  
plotNormalHistogram(x)
plotNormalHistogram(sqrt(x))
plotNormalHistogram(log(x))
T_cub = sign(x) * abs(x)^(1/3)  ## Cube root transformation
plotNormalHistogram(T_cub)

T_tuk = transformTukey(x, plotit=FALSE) ## Tukey’s Ladder of Powers transformation
plotNormalHistogram(T_tuk)


###############################################################################
#### Inverse of BoxCoxTrans does not exist in Caret                        ####                                                   ####
###############################################################################
BoxCoxTrans.inverse <- function(object, newdata) {
  if(!is.vector(newdata) || !is.numeric(newdata)) stop("newdata should be a numeric vector")
  if(is.na(object$lambda)) return(newdata) 
  lambda <- object$lambda
  if(lambda < object$fudge & lambda > -object$fudge)
    lambda <- 0
  else if(lambda < 1+object$fudge & lambda > 1-object$fudge)
    lambda <- 1
  
  if(lambda == 0) exp(newdata) else (lambda*newdata + 1)^(1/lambda) 
}
###############################################################################
#### BoxCox transform response variable                                    ####                                                   ####
###############################################################################
require(caret)
T_box.fit <- BoxCoxTrans(y = full.dt$SalePrice, x = full.dt$GrLivArea, na.rm = TRUE)
T_box <- predict(b2, train.dt$SalePrice)
T_box.inverse <- BoxCoxTrans.inverse(b2,p)
T_box.compare <- cbind(train.dt$SalePrice,r)

plotNormalHistogram(T_box)
###############################################################################
#### BoxCox transform response variable                                    ####                                                   ####
###############################################################################
T_box.fit <- BoxCoxTrans(y = full.dt$SalePrice, na.rm = TRUE)
T_box <- predict(b2, train.dt$SalePrice)
plotNormalHistogram(T_box)

