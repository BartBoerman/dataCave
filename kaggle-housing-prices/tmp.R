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



## Analyse mean imputation LotFrontage
tmp.dt <- copy(full.dt)
tmp.dt <- full.dt[, c("Id","Neighborhood","LotFrontage")]
tmp.dt[,.(count=.N,
            mean=mean(LotFrontage, na.rm = T),
            median=median(LotFrontage, na.rm = T),
            quantile_upper=quantile(LotFrontage, .75, na.rm=T),
            max=max(LotFrontage, na.rm = T)) 
         ,by=Neighborhood
         ][order(Neighborhood)]


tmp.dt[, LotFrontageMean := replace(LotFrontage, is.na(LotFrontage), median(LotFrontage, na.rm=T)), by=.(Neighborhood)]

### Analyse imputation MSZoning
tmp.dt <- copy(full.dt)
tmp.dt <- full.dt[, c("Id","Neighborhood","MSZoning","SalePrice","LotArea","GrLivArea")]
tmp.dt <- full.dt[is.na(MSZoning),]
Neighborhood.MSZoning.missing <- unique(full.dt[is.na(MSZoning), c("Neighborhood")])
Neighborhood.MSZoning <- unique(full.dt[!is.na(MSZoning), c("Neighborhood","MSZoning")][order(Neighborhood)])

  
train.dt[Neighborhood %in% Neighborhood.MSZoning.missing$Neighborhood,.(count=.N,
            mean=mean(SalePrice),
            min=min(SalePrice),
            max=max(SalePrice)) 
         ,by=c("Neighborhood", "MSZoning") 
         ][order (Neighborhood,MSZoning)]

train.dt[Neighborhood %in% Neighborhood.MSZoning.missing$Neighborhood,.(count=.N,
                                                                        mean=mean(GrLivArea),
                                                                        min=min(GrLivArea),
                                                                        max=max(GrLivArea)) 
         ,by=c("Neighborhood", "MSZoning") 
         ][order (Neighborhood,MSZoning)]

## RL for missing MSZoning in Mitchel because GrLivArea is greater then max of RM
## Not sure (yet) for missing MSZoning in IDOTRR. RM is most common in IDOTRR but might be wrong

## Electrical
tmp.dt <- copy(full.dt)
tmp.dt <- tmp.dt[, c("Id","Neighborhood","MSZoning","Electrical","SalePrice","LotArea")]

full.dt[Neighborhood == "Timber",.(count=.N) 
       ,by = c("Neighborhood", "MSZoning","Electrical") 
       ][order (Neighborhood, MSZoning, Electrical)]

tmp.dt <- tmp.dt[is.na(Electrical), ]
## Most common value for neihborhood Timber is SBrkr

## Utilities
tmp.dt <- copy(full.dt)
tmp.dt <- tmp.dt[, c("Id","Neighborhood","MSZoning","Electrical","Utilities","SalePrice","LotArea")]

full.dt[Neighborhood == "Timber",.(count=.N) 
        ,by = c("Neighborhood", "MSZoning","Electrical") 
        ][order (Neighborhood, MSZoning, Electrical)]

tmp.dt <- tmp.dt[is.na(Utilities) | Utilities == "NoSeWa", ]

## porche
tmp.dt <- copy(full.dt)
tmp.dt <- tmp.dt[, c("Id","Neighborhood","OpenPorchSF", "EnclosedPorch", "ThreeSsnPorch", "ScreenPorch")]
tmp.dt[,porchSf := (OpenPorchSF + EnclosedPorch + ThreeSsnPorch + ScreenPorch)]

## Exterior2nd Exterior1st
tmp.dt <- copy(full.dt)
tmp.dt <- tmp.dt[, c("Id","Neighborhood","MSZoning","OverallQual","OverallCond","Exterior2nd","Exterior1st","SalePrice","LotArea","totalSF")]
tmp.dt <- tmp.dt[Neighborhood == "Edwards" & OverallQual == 5 & OverallCond ==7,]

tmp.dt[,.(count=.N, meanSalePrice=mean(SalePrice)) 
        ,by = c("Exterior1st", "Exterior2nd") 
        ][order (Exterior1st, Exterior2nd)]

### MasVnrType and MasVnrArea

tmp.dt <- copy(full.dt)
tmp.dt <- tmp.dt[, c("Id","Neighborhood","MSZoning","OverallQual","OverallCond","Functional","Exterior2nd","Exterior1st","MasVnrType","MasVnrArea","SalePrice","LotArea","totalSF")]
tmp.dt <- tmp.dt[Exterior1st %in% c("VinylSd","Wd Shng","CmentBd"),]

tmp.dt[,.(count=.N, meanSalePrice=mean(SalePrice, na.rm = T)) 
       ,by = c("Neighborhood", "Exterior1st","MasVnrType") 
       ][order (Neighborhood, Exterior1st, MasVnrType)]

## Functional
tmp.dt <- copy(full.dt)
tmp.dt <- tmp.dt[, c("Id","Neighborhood","MSZoning","OverallQual","OverallCond","Functional","Utilities","Exterior2nd","Exterior1st","MasVnrType","MasVnrArea","SalePrice","LotArea","totalSF")]

## Time

tmp.dt <- copy(full.dt)
tmp.dt <- tmp.dt[, c("Id","MoSold","YrSold","Neighborhood","MSZoning","OverallQual","OverallCond","Functional","Exterior2nd","Exterior1st","MasVnrType","MasVnrArea","SalePrice","LotArea","totalSF")]



###############################################################################
#### One hot encoding of factor variables with dummyVars from caret        ####                                                   ####
###############################################################################
## formulala for one hot encoding
f <- paste('~', paste(variablesFactor, collapse = ' + '))
## one hot encode train data with dummyVars from caret package
encoder <- dummyVars(as.formula(f), train.dt, fullRank = T, drop2nd = T)
## apply encoding on full data set
full.dummyVars.dt <- as.data.table(predict(encoder, full.dt))
###############################################################################
######## Analyse dummy vars                                                ####
###############################################################################
## filter out test data set
full.dummyVars.dt <- cbind(full.dt[,dataPartition], full.dummyVars.dt)
test.dummyVars.dt <- full.dummyVars.dt[V1 == "test",]
## levels not available in test data set have range zero
dummyVars.range.max <- sapply(full.dummyVars.test.dt[,!c("V1")], 
                                function(x) {range(x, na.rm = T, finite = F)})[2,]
dummyVars.df <- data.frame(dummyVars.range.max)
dummyVars.df <- data.frame(variableName = row.names(dummyVars.df), dummyVars.df,row.names = NULL)
dummyVars.df <- dummyVars.df[dummyVars.range.max == 0,]
missingLevels <- as.character(dummyVars.df$variableName)  
### what is the distribution of these levels in the training data?
train.dummyVars.missing.dt <- full.dummyVars.dt[V1 == "train", c(missingLevels), with=FALSE]
train.dummyVars.missing.sum <- sapply(train.dummyVars.missing.dt, 
                              function(x) {sum(x, na.rm = T)})
print(train.dummyVars.sum)
###############################################################################
##### Solution for missing levels                                          ####
###############################################################################
## HouseStyle
full.dt[, hasUnfinishedLevel:= ifelse(HouseStyle %in% c("1.5Unf","2.5Unf"),1,0)]
require(plyr) ## easy mapping of factor levels
full.dt$HouseStyle <-mapvalues(full.dt$HouseStyle, 
                               from = c("1Story", "1.5Fin", "1.5Unf","2.5Unf","2.5Fin","2Story","SFoyer","SLvl"), 
                               to =   c("1Story", "1Story", "1Story","2Story","2Story","2Story","SFoyer","SLvl"))
full.dt$HouseStyle <-mapvalues(full.dt$HouseStyle, 
                               from = c("1Story", "1.5Fin", "1.5Unf","2.5Unf","2.5Fin","2Story","SFoyer","SLvl"), 
                               to =   c("1Story", "1Story", "1Story","2Story","2Story","2Story","SFoyer","SLvl"))
full.dt[, hasUnfinishedLevel:= ifelse(HouseStyle %in% c("1.5Unf","2.5Unf"),1,0)]
## Condition1 and Condition2
full.dt[, ':=' (isConditionNearRail    = ifelse(Condition1 %in% c("RRAe","RRAn","RRNe","RRNn") | Condition2 %in% c("RRAe","RRAn","RRNe","RRNn"),1,0),
                isConditionNearArtery  = ifelse(Condition1 == "Artery" | Condition2 == "Artery",1,0),
                isConditionNearFeedr  = ifelse(Condition1 == "Feedr" | Condition2 == "Feedr",1,0),
                isConditionNearPosFeature  = ifelse(Condition1 %in% c("PosA"," PosN") | Condition2 %in% c("PosA"," PosN"),1,0),
                Condition1 = NULL, ## highly correlated
                Condition2 = NULL  ## highly correlated
                )]
## Utilities
## one recordin train data, remove it as outlier (or drop column)
full.dt <- full.dt[Utilities != "NoSeWa",]
## RoofMatl
full.dt[, ':=' (hasRoofMatlWoodCly = ifelse(RoofMatl %in% c("WdShake","WdShngl","ClyTile"),1,0),
                hasRoofTar = ifelse(RoofMatl == "Tar&Grv",1,0), 
                hasRoofCompShg = ifelse(RoofMatl == "CompShg",1,0), 
                RoofMatl = NULL  ## highly correlated
)]
## Could have done a more thorough job, other missing levels will just be removed
full.dummyVars.dt <- full.dummyVars.dt[,!c(missingLevels,"dataPartition"), with=FALSE]


###################################################################
#### funModeling                                               ####
###################################################################
## https://livebook.datascienceheroes.com/exploratory-data-analysis.html
require(funModeling) ## alternative to describe in psych package
#### Data set health status
## Use case: analyze the zeros, missing values (NA) and number of unique values for a given dataset.
df_status(train.dt)
#### Data profiling
## Descriptive statistics for numerical variables
profiling_num(train.dt, digits = 2)
## Getting frequency distributions for categoric variables
## This fill  plot frequencies
freq(data = full.dt, input = variablesFactor, plot = TRUE)
#### Correlation (Pearson coefficient) of nummerical variables agaimst our target
## If target is categorical, then it will be converted to numeric.
correlation_table(train.dt, target = "SalePrice")
#### Variable importance ranking based on information theory 
## Metrics are: entropy (en), mutual information (mi), information gain (ig) and gain ratio (gr)
tmp <- train.dt[,c(variablesFactor,"SalePrice"), with = FALSE]
variable_importance = var_rank_info(data = tmp, target = "SalePrice")

# Plotting 
ggplot(variable_importance, aes(x = reorder(var, 
                                            gr), y = gr, fill = var)) + geom_bar(stat = "identity") + 
  coord_flip() + theme_bw() + xlab("") + 
  ylab("Variable Importance (based on Information Gain)") + 
  guides(fill = FALSE)
#### Binning
Hmisc::describe(train.dt$OverallQual)

tmp <- copy(train.dt)
tmp$GrLivArea <- equal_freq(tmp$GrLivArea, n_bins = 10)
tmp$LotArea <- equal_freq(tmp$LotArea, n_bins = 5)
tmp$GarageArea <- equal_freq(tmp$GarageArea, n_bins = 5)

ggplot(tmp, aes(x =  GarageArea, y = SalePrice)) +
      geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
      theme(text = element_text(size=9))

ggplot(tmp, aes(x =  TotRmsAbvGrd, y = SalePrice)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
  theme(text = element_text(size=9))

ggplot(tmp, aes(x =  SaleCondition, y = SalePrice)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
  theme(text = element_text(size=9))

tmp2 <- tmp[GarageCars > 3 | TotRmsAbvGrd > 11]

tukey_outlier(train.dt$LotArea)
tukey_outlier(train.dt$GrLivArea)
tukey_outlier(train.dt$SalePrice)


###################################################################
#### Boruta feature selection                                  ####
###################################################################
require(Boruta)
set.seed(333)

tmp.dt <- copy(full.dt)
tmp.dt <- tmp.dt[dataPartition == "train"]
boruta.train <- Boruta(SalePrice~.-c(dataPartition,Id), data = tmp.dt, doTrace = 0)
boruta.final <- TentativeRoughFix(boruta.train)
boruta.df <- attStats(boruta.final)
featuresBoruta <- getSelectedAttributes(boruta.final, withTentative = T)





