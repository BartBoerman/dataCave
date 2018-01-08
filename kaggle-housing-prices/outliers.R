###################################################################
#### Outliers                                                  ####
###################################################################
## Detected 9 outliers via visual inspecting of plots, criteria
## GrLivArea > 4000
## SalePrice > 650000
## LotArea   > 100000
## FirstFlrSf > 3000
outliers.dt <- train.dt[GrLivArea > 4000 | SalePrice > 650000 | LotArea > 100000 | X1stFlrSF > 3000,] 
###################################################################
#### IQR (interquartile range)                                 ####
###################################################################
salePrice.iqr <- quantile(train.dt$SalePrice, 0.75)[[1]] + 1.5 * IQR(train.dt$SalePrice) ### 3th quantatile + (1.5 * IQR)
outliers.SalePrice.dt <- train.dt[SalePrice > salePrice.iqr ,] 
nrow(outliers.SalePrice.dt) ## 61 records maybe to many for just removing the records (4,1%)
train.dt[SalePrice > salePrice.iqr ,.(count=.N), by=Neighborhood][order(Neighborhood)]
train.dt[,.(count=.N), by=Neighborhood][order(Neighborhood)]
#### Alternatives
## Set value to 0.95 quantile
## Analyse outliers per neighborhood
###################################################################
#### Delete outliers                                           ####
###################################################################
