###################################################################
#### References                                                ####
###################################################################
##http://www.listendata.com/2016/10/r-data-table.html
##http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/
###################################################################
#### Dependencies                                              ####
###################################################################
require(data.table) # fast data wrangling and analysis
require(psych)      # descriptive statistics, skewness and kurtosis
require(caret)      # (near) zero variance
###################################################################
#### Syntax description                                        ####
###################################################################
## The general form of data.table syntax is:
##  DT[ i,  j,  by ] # + extra arguments
##      |   |   |
##      |   |    -------> grouped by what?
##      |    -------> what to do?
##       ---> on which rows?
###################################################################
#### Get data                                                  ####
###################################################################
train.dt <- fread(input = "train.csv", 
                  sep = ",", 
                  nrows = -1,
                  header = T,
                  na.strings=c("NA","N/A","null"),
                  stringsAsFactors = F,
                  check.names = T,
                  strip.white = T,
                  blank.lines.skip = T,
                  data.table = T
) 
test.dt <- fread(input = "test.csv", 
                 sep = ",", 
                 nrows = -1,
                 header = T,
                 na.strings=c("NA","N/A","null"),
                 stringsAsFactors = F,
                 check.names = T,
                 strip.white = T,
                 blank.lines.skip = T,
                 data.table = T
) 
## Create one data set for feature engineering. 
train.dt[, dataPartition:="train"]
test.dt[, SalePrice:=as.integer(NA)] 
test.dt[, dataPartition:="test"]
full.dt <- rbindlist(list(train.dt, test.dt), use.names = F, fill = F)
###################################################################
#### Data dictionary                                           ####
###################################################################
## Data types
variableTypes.df <- cbind(as.data.frame(names(full.dt)),as.data.frame(sapply(full.dt, class)))
names(variableTypes.df) <- c("variable","type")
## Numeric, square footage
variablesSquareFootage <- c(
  "LotFrontage", 		## Linear feet of street connected to property 
  "LotArea",    		## Lot size in square feet
  "MasVnrArea",  		## Masonry veneer area in square feet
  "BsmtFinSF1",		  ## Type 1 finished square feet	
  "BsmtFinSF2",		  ## Type 2 finished square feet
  "BsmtUnfSF",		  ## Unfinished square feet of basement area
  "TotalBsmtSF", 		## Total square feet of basement area
  "FirstFlrSF",		  ## First Floor square feet
  "SecondFlrSF",	  ## Second floor square feet
  "LowQualFinSF", 	## Low quality finished square feet (all floors)
  "GrLivArea", 		  ## Above grade (ground) living area square feet
  "GarageArea",     ## Size of garage in square feet
  "WoodDeckSF",     ## Wood deck area in square feet
  "OpenPorchSF",    ## Open porch area in square feet  
  "EnclosedPorch",  ## Enclosed porch area in square feet 
  "ThreeSsnPorch",  ## Three season porch area in square feet 
  "ScreenPorch",    ## Screen porch area in square feet
  "PoolArea" 		    ## Pool area in square feet
)
## Counts, a house has n of something
variablesCounts <- c(
  "BsmtFullBath",		## Basement full bathrooms
  "BsmtHalfBath",		## Basement half bathrooms
  "FullBath",			  ## Full bathrooms above grade
  "HalfBath",			  ## Half baths above grade
  "BedroomAbvGr",		## Bedrooms above grade (does NOT include basement bedrooms)
  "KitchenAbvGr",		## Kitchens above grade
  "TotRmsAbvGrd",		## Total rooms above grade (does not include bathrooms)
  "Fireplaces",		  ## Number of fireplaces
  "GarageCars"     	## Size of garage in car capacity
)
## Values
variablesValues <- c(
  "MiscVal",        ## $ Value of miscellaneous feature
  "SalePrice"       ## $ Price paid
)
## Factors
variablesFactor <- colnames(full.dt)[which(as.vector(full.dt[,sapply(full.dt, class)]) == "character")]
variablesFactor <- setdiff(variablesFactor, "dataPartition") 
variablesFactor <- c(variablesFactor,
                     "MSSubClass",     ## Identifies the type of dwelling involved in the sale
                     "OverallQual",    ## Rates the overall material and finish of the house
                     "OverallCond"     ## Rates the overall condition of the house
)
# <- sapply(names(full.dt),function(x){class(full.dt[[x]])})
# <-names(feature_classes[feature_classes != "character"])
###################################################################
#### Data cleansing                                            ####
###################################################################
full.dt[GarageYrBlt == 2207, GarageYrBlt:= 2007] ## Fix typo
full.dt[MSSubClass  == 150, MSSubClass:= 160] ## 150 not in training set
###################################################################
#### Data engineering                                          ####
###################################################################
## In R first character can not be a number in variable names
setnames(full.dt, c("X1stFlrSF","X2ndFlrSF","X3SsnPorch"), c("FirstFlrSF","SecondFlrSF","ThreeSsnPorch"))
## Set columns to numeric
changeColType <- c(variablesSquareFootage, variablesCounts, variablesValues)
full.dt[,(changeColType):= lapply(.SD, as.numeric), .SDcols = changeColType]
## Set columns to factor
changeColType <- variablesFactor
full.dt[,(changeColType):= lapply(.SD, as.factor), .SDcols = changeColType]
###################################################################
#### Descriptive statistics                                    ####
###################################################################
## statisticts
descStats <- describe(full.dt[, c(variablesSquareFootage,variablesValues), with = FALSE]) ## from psych package 
#print(descStats)
## na values
countIsNA <- sapply(full.dt,function(x)sum(is.na(x)))
countIsNA.df <- data.frame(countIsNA)
countIsNA.df <- data.frame(variableName = row.names(countIsNA.df), countIsNA.df,row.names = NULL)
countIsNA.df <- countIsNA.df[countIsNA >0,]
print(countIsNA.df)
## zero variance
zeroVarianceVariables.df <- nearZeroVar(full.dt, names = T, saveMetrics = T,
                                        foreach = T, allowParallel = T)
###################################################################
#### Impute missing values                                     ####
###################################################################
#### function to find the mode (class with highest frequancy, can be multiple).
findMode <- function(x) {
  names(table(x))[table(x)==max(table(x))]
}
#### imputations
## Kitchen
full.dt[is.na(KitchenQual), KitchenQual := "TA" ] ## One record, set to Typical
## Garage
full.dt[is.na(GarageFinish) & GarageType == "Detchd", ':=' (GarageFinish = "Fin",
                                                        GarageCars = 1,
                                                        GarageArea = 360,
                                                        GarageYrBlt = YearBuilt,
                                                        GarageQual = findMode(full.dt$GarageQual),
                                                        GarageCond = findMode(full.dt$GarageCond))] 
full.dt[is.na(GarageFinish), GarageFinish := "None"]
full.dt[is.na(GarageQual), GarageQual := "None"]
full.dt[is.na(GarageCond), GarageCond := "None"]
full.dt[is.na(GarageType), GarageType := "None"]
full.dt[is.na(GarageYrBlt), GarageYrBlt := 0]
## Basement
full.dt[is.na(BsmtExposure) & BsmtFinType1 == "Unf" , BsmtExposure := "No"]
full.dt[is.na(BsmtExposure), BsmtExposure := "None"]
full.dt[is.na(BsmtQual) & BsmtFinType1 == "Unf" , BsmtQual := "TA"]
full.dt[is.na(BsmtQual), BsmtQual := "None"]
full.dt[is.na(BsmtCond), BsmtCond := "None"]
full.dt[is.na(BsmtFinType1), BsmtFinType1 := "None"]
full.dt[is.na(BsmtFinType2) & BsmtFinSF2 > 0, BsmtFinType2 := "Unf"]
full.dt[is.na(BsmtFinType2), BsmtFinType2 := "None"]
full.dt[is.na(BsmtFinSF1),':=' (BsmtFinSF1 = 0, BsmtFinSF2 = 0, BsmtUnfSF = 0, TotalBsmtSF = 0)] 
full.dt[is.na(BsmtFullBath),':=' (BsmtFullBath = 0, BsmtHalfBath = 0)] 
## FireplaceQu  
full.dt[is.na(FireplaceQu), FireplaceQu := "None"]
## MSZoning
## RL for missing MSZoning in Mitchel because GrLivArea is greater then max of RM
## Not sure (yet) for missing MSZoning in IDOTRR. RM is most common in IDOTRR but might be wrong
full.dt[is.na(MSZoning) & Neighborhood == "Mitchel", MSZoning := "RL"]
full.dt[is.na(MSZoning) & Neighborhood == "IDOTRR", MSZoning  := "RM"]
## Electrical
## Most common value for neighborhood Timber is SBrkr
full.dt[is.na(Electrical) , Electrical  := findMode(full.dt$Electrical)]
## Exterior
## Most common for neighborhood and large total square footage is "MetalSd"
full.dt[is.na(Exterior1st),':=' (Exterior1st = "MetalSd",Exterior2nd = "MetalSd")]
## MasVnrType and MasVnrArea. Taking the easy way out here
full.dt[is.na(MasVnrType),':=' (MasVnrType = "None", MasVnrArea = 0)]
## SaleType
full.dt[is.na(SaleType), SaleType := findMode(full.dt$SaleType)]
## Functional
full.dt[is.na(Functional), Functional := findMode(full.dt$Functional)]
## MiscFeature
full.dt[is.na(MiscFeature), MiscFeature := "None"]
## Alley
full.dt[is.na(Alley), Alley := "None"]
## Utilities
full.dt[is.na(Utilities), Utilities := findMode(full.dt$Utilities)]
## PoolQC
full.dt[is.na(PoolQC), PoolQC := "None"]
## Fence
full.dt[is.na(Fence), Fence := "None"]
## LotFrontage
## Alternative 1, impute by the median per neigborhood 
# full.dt[, LotFrontage := replace(LotFrontage, is.na(LotFrontage), median(LotFrontage, na.rm=TRUE)), by=.(Neighborhood)]
## Alternatove 2, impute with logistic regression
fit <- lm(log1p(LotFrontage) ~ log1p(LotArea) + Neighborhood + LotConfig + LandContour + MSZoning, data = full.dt[!is.na(LotFrontage),])
full.dt[is.na(LotFrontage), LotFrontage :=  round(expm1(predict(fit, newdata = full.dt[is.na(LotFrontage),])),0 )]