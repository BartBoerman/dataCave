###################################################################
#### References                                                ####
###################################################################
#http://www.listendata.com/2016/10/r-data-table.html
# http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/
###################################################################
#### Dependencies                                              ####
###################################################################
require(data.table)
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
variablesFactor <- c(variablesFactor,
                     "MSSubClass",     ## Identifies the type of dwelling involved in the sale
                     "OverallQual",    ## Rates the overall material and finish of the house
                     "OverallCond"     ## Rates the overall condition of the house
)
# <- sapply(names(full.dt),function(x){class(full.dt[[x]])})
# <-names(feature_classes[feature_classes != "character"])
###################################################################
#### Dara engineering                                          ####
###################################################################
## In R first character can not be a number in variable names
setnames(full.dt, c("X1stFlrSF","X2ndFlrSF","X3SsnPorch"), c("FirstFlrSF","SecondFlrSF","ThreeSsnPorch"))
## Set columns to numeric
changeColType <- c(variablesSquareFootage, variablesCounts, variablesValues)
full.dt[,(changeColType):= lapply(.SD, as.numeric), .SDcols = changeColType]
## Set columns to factor
changeColType <- variablesFactor
full.dt[,(changeColType):= lapply(.SD, as.factor), .SDcols = changeColType]