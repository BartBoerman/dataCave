require(dplyr)




#### PCA
pca <- h2o.prcomp(training_frame = train.hex[,features], validation_frame = validate.hex[,features], k = 10, transform = "STANDARDIZE")
screeplot(pca)
features_pca <- h2o.predict(pca, full.hex[,features], num_pc=6)
summary(features_pca)

#### Rows with NA values

#### data quality indicator 
full.df$hasNA <- apply(full.df[,features], 1, function(x) all(!is.na(x))) # TRUE when no NA's in row else false
perHasNA <- sum(full.df$hasNA) / nrow(full.df)

temp.dt <- full.dt[, !c(changeColType), with = FALSE]
temp.dt <- full.dt[, .(.N), by = .(Neighborhood,HouseStyle)]
sapply(na.omit(full.dt[,c(variablesSquareFootage), with = FALSE]), max)
sapply(na.omit(full.dt[,c(variablesSquareFootage), with = FALSE]), min)
temp.dt[, lapply(.SD, mean, na.rm=TRUE)]
cor(na.omit(full.dt[,c(variablesSquareFootage, variablesCounts), with = F]))


summary(full.dt[,c(variablesSquareFootage), with = F])


salePrice.stat  <- train.dt[,list(count=.N,
                                           mean=mean(SalePrice),
                                           min=min(SalePrice),
                                           lower=quantile(SalePrice, .25, na.rm=TRUE),
                                           middle=quantile(SalePrice, .50, na.rm=TRUE),
                                           upper=quantile(SalePrice, .75, na.rm=TRUE),
                                           max=max(SalePrice)),
                                     by='Neighborhood']

salePrice.stat <- salePrice.stat[order(-salePrice.stat$count),]

#  note that .SDcols also allows reordering of the columns
temp.dt <- full.dt[, c(variablesSquareFootage), with = FALSE]
temp.dt[, lapply(.SD, mean, na.rm=TRUE)] 
temp.dt <- full.dt[, c("Neighborhood",variablesSquareFootage), with = FALSE]
temp.dt[, sum(LotArea),by="Neighborhood"]  


DT[, .SD, .SDcols=x:y]                # select columns 'x' and 'y'

scaled.dt <- data.table(scale(full.dt[,c(variablesSquareFootage),with=FALSE]))

scaled.dt <- full.dt[ , (variablesSquareFootage) := lapply(.SD, scale), .SDcols = variablesSquareFootage]

IQR(full.dt$SalePrice, na.rm = TRUE)


                            ]

## The general form of data.table syntax is:
##  DT[ i,  j,  by ] # + extra arguments
##      |   |   |
##      |   |    -------> grouped by what?
##      |    -------> what to do?
##       ---> on which rows?

train.dt[,SalePriceLog:=log(SalePrice)]



temp.dt <- train.dt[SalePriceLog > salePriceLog.qtl[[2]],] 
temp.dt <- train.dt[SalePriceLog < salePriceLog.qtl[[1]],] 
temp.dt <- train.dt[SalePrice > (1.5*salePrice.iqr),] 


temp.dt <- full.dt[, !c(changeColType), with = FALSE]
temp.dt[,new1:="hallo"][,new2:="bart"]
temp.dt[,`:=`(new1 = "hi", new2 = "b")] 
temp.dt <- copy(train.dt)

### Some basic stats per Neighborhood
temp.dt[,.(count=.N,meanSalePrice=mean(SalePrice), iqrSalePrice=IQR(SalePrice)), by = Neighborhood][order(meanSalePrice)]

x<-quantile(train.dt$SalePrice, 0.75)[[1]] + 1.5 * IQR(train.dt$SalePrice)

temp.dt[,.(count=.N,qtl=quantile(SalePrice,na.rm=TRUE,0.75)),by = Neighborhood]
           


train.dt[,.(count=.N,
                    mean=mean(SalePrice),
                    min=min(SalePrice),
                    lower=quantile(SalePrice, .25, na.rm=TRUE),
                    middle=quantile(SalePrice, .50, na.rm=TRUE),
                    upper=quantile(SalePrice, .75, na.rm=TRUE),
                    max=max(SalePrice),
                    iqr=IQR(SalePrice, na.rm = T)) 
                    #,by='Neighborhood'
         ]



