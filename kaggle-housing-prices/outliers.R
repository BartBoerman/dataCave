###################################################################
#### Outliers                                                  ####
###################################################################
## Detected 9 outliers via visual inspecting of plots, criteria
## GrLivArea > 4000
## SalePrice > 650000
## LotArea   > 100000
## FirstFlrSf > 3000
outliers.Id <-  train.dt[GrLivArea > 4000 | SalePrice > 650000 | LotArea > 100000 | X1stFlrSF > 3000,Id]
outliers.dt <- full.dt[Id %in% outliers.Id,]
###################################################################
#### IQR (interquartile range) sale price                      ####
###################################################################
## calculate threshold based on third quantile + 1.5 * IQR
salePrice.iqr <- quantile(train.dt$SalePrice, 0.75)[[1]] + 1.5 * IQR(train.dt$SalePrice) ### 3th quantatile + (1.5 * IQR)
## filter outliers
outliers.SalePrice.dt <- train.dt[SalePrice > salePrice.iqr ,] 
nrow(outliers.SalePrice.dt) ## 61 records maybe to many for just removing the records (4,1%)
train.dt[SalePrice > salePrice.iqr ,.(count=.N), by=Neighborhood][order(Neighborhood)]
train.dt[,.(count=.N), by=Neighborhood][order(Neighborhood)]
#### Alternatives (to do)
## Set value to 0.95 quantile
## Analyse outliers per neighborhood

## Let's just remove them and see....

## did it, saw it, it was painfull. Let's not remove them.
## outliers.SalePrice.Id <-  train.dt[SalePrice > salePrice.iqr,Id]
## outliers.SalePrice.dt <- full.dt[Id %in% outliers.Id,]
## full.dt <- full.dt[!(Id %in% outliers.Id) & !(Id %in% outliers.SalePrice.Id)  ,]

###################################################################
#### IQR (interquartile range) sale price per Neighborhood     ####
###################################################################
train.dt[,.(IQR=quantile(SalePrice, 0.75)[[1]] + 1.5 * IQR(SalePrice), max=max(SalePrice)), by=Neighborhood][order(Neighborhood)]

tmp.dt <- train.dt[,.(count=.N,
                mean=mean(SalePrice, na.rm = T),
                quantile_third=quantile(SalePrice, .75, na.rm=T),
                quantile_95=quantile(SalePrice, .95, na.rm=T),
                iqr =IQR(SalePrice) * 1.5,
                threshold = quantile(SalePrice, .75, na.rm=T) + (IQR(SalePrice) * 1.5),
                max=max(SalePrice, na.rm = T)) 
           ,by=Neighborhood
           ][order(Neighborhood)]
tmp.dt <- tmp.dt[threshold < max,]


full.dt[, SalePriceThreshold := quantile(SalePrice, .75, na.rm=T) + (IQR(SalePrice, na.rm=T) * 1.5), by=.(Neighborhood)]


tmp.dt <- full.dt[SalePrice >= SalePriceThreshold,]

## which is 33% of the data












