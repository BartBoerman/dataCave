## https://stats.stackexchange.com/questions/52293/r-qqplot-how-to-see-whether-data-are-normally-distributed
###############################################################################
#### assumption of normality                                               ####
###############################################################################
###############################################################################
#### Box cox transformation on response variable                           ####                                                   ####
###############################################################################
## Inverse of BoxCoxTrans does not exist in Caret
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
## Try outs
b <- BoxCoxTrans(train.dt$SalePrice)

p <- predict(b, train.dt$SalePrice)

r <- BoxCoxTrans.inverse(b,p)

z <- cbind(train.dt$SalePrice,r)


b2 <- BoxCoxTrans(y = full.dt$SalePrice, x = full.dt$GrLivArea, na.rm = TRUE)
p <- predict(b2, train.dt$SalePrice)
r <- BoxCoxTrans.inverse(b2,p)
z <- cbind(train.dt$SalePrice,r)
