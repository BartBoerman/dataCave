<style type="text/css">

body{ /* Normal  */
      font-size: 14px;
  }
td {  /* Table  */
  font-size: 12px;
}
h1, .h1, h2, .h2, h3, .h3 {
    margin-top: 10.5px;
    margin-bottom: 10.5px;
}
h1.title {
  font-size: 28px;
  color: #7db956;
}
h1 { /* Header 1 */
  font-size: 28px;
  color: #3e4a52;
}
h2 { /* Header 2 */
    font-size: 18px;
  color: #3e4a52;
}
h3 { /* Header 3 */
  font-size: 14px;
  color: #3e4a52;
}
code.r{ /* Code block */
    font-size: 12px;
}
pre { /* Code block - determines code spacing between lines */
    font-size: 14px;
}
</style>
Introduction
============

Version
-------

**Raw inital version**.

Goal
----

Exploratory analyis and machine learning modal for predicting housing prices in competition ["House Prices: Advanced Regression Techniques"](https://www.kaggle.com/c/house-prices-advanced-regression-techniques). The aim is to predict house prices based on the provided data:

-   **train.csv**,data for training our model
-   **test.csv**, data used to see how well our model performs on unseen data

In addition I want to gain and share some basic knowledge of

-   data wrangling and analysis with data.table
-   machine learning with h2o
-   stacking with h2o

Required libraries
------------------

-   **knitr**, used to create this document
-   **data.table**, fast data wrangling with R
-   **h2o**, machine learning algorithmes and more from h2o.ai
-   **psych**, descriptive analytics, skewness and kurtosis
-   **caret**, (near) zero variance

``` r
require(knitr) ## a general-purpose programming engine
knitr::opts_chunk$set(error=FALSE, message=FALSE, warning=FALSE)
require(data.table) ## fast data wrangling
require(h2o)        ## machine learning algorithmes
require(psych)      ## descriptive statistics, skewness and kurtosis
require(caret)      ## zero variance 
```

Get data into R
===============

``` r
#### Fetch data
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
## Create one data set for feature engineering 
train.dt[, dataPartition:="train"]
test.dt[, SalePrice:=NA] 
test.dt[, dataPartition:="test"]
full.dt <- rbindlist(list(train.dt, test.dt), use.names = F, fill = F)
```

Data dictionary
===============

Variables in raw data
---------------------

``` r
## Data types
variableTypes.df <- cbind(as.data.frame(names(full.dt)),as.data.frame(sapply(full.dt, class)))
names(variableTypes.df) <- c("variable","type")
kable(variableTypes.df, , row.names = F)
```

| variable      | type      |
|:--------------|:----------|
| Id            | integer   |
| MSSubClass    | integer   |
| MSZoning      | character |
| LotFrontage   | integer   |
| LotArea       | integer   |
| Street        | character |
| Alley         | character |
| LotShape      | character |
| LandContour   | character |
| Utilities     | character |
| LotConfig     | character |
| LandSlope     | character |
| Neighborhood  | character |
| Condition1    | character |
| Condition2    | character |
| BldgType      | character |
| HouseStyle    | character |
| OverallQual   | integer   |
| OverallCond   | integer   |
| YearBuilt     | integer   |
| YearRemodAdd  | integer   |
| RoofStyle     | character |
| RoofMatl      | character |
| Exterior1st   | character |
| Exterior2nd   | character |
| MasVnrType    | character |
| MasVnrArea    | integer   |
| ExterQual     | character |
| ExterCond     | character |
| Foundation    | character |
| BsmtQual      | character |
| BsmtCond      | character |
| BsmtExposure  | character |
| BsmtFinType1  | character |
| BsmtFinSF1    | integer   |
| BsmtFinType2  | character |
| BsmtFinSF2    | integer   |
| BsmtUnfSF     | integer   |
| TotalBsmtSF   | integer   |
| Heating       | character |
| HeatingQC     | character |
| CentralAir    | character |
| Electrical    | character |
| X1stFlrSF     | integer   |
| X2ndFlrSF     | integer   |
| LowQualFinSF  | integer   |
| GrLivArea     | integer   |
| BsmtFullBath  | integer   |
| BsmtHalfBath  | integer   |
| FullBath      | integer   |
| HalfBath      | integer   |
| BedroomAbvGr  | integer   |
| KitchenAbvGr  | integer   |
| KitchenQual   | character |
| TotRmsAbvGrd  | integer   |
| Functional    | character |
| Fireplaces    | integer   |
| FireplaceQu   | character |
| GarageType    | character |
| GarageYrBlt   | integer   |
| GarageFinish  | character |
| GarageCars    | integer   |
| GarageArea    | integer   |
| GarageQual    | character |
| GarageCond    | character |
| PavedDrive    | character |
| WoodDeckSF    | integer   |
| OpenPorchSF   | integer   |
| EnclosedPorch | integer   |
| X3SsnPorch    | integer   |
| ScreenPorch   | integer   |
| PoolArea      | integer   |
| PoolQC        | character |
| Fence         | character |
| MiscFeature   | character |
| MiscVal       | integer   |
| MoSold        | integer   |
| YrSold        | integer   |
| SaleType      | character |
| SaleCondition | character |
| SalePrice     | integer   |
| dataPartition | character |

Definitions
-----------

``` r
## Numeric, square footage
variablesSquareFootage <- c(
  "LotFrontage",        ## Linear feet of street connected to property 
  "LotArea",            ## Lot size in square feet
  "MasVnrArea",         ## Masonry veneer area in square feet
  "BsmtFinSF1",       ## Type 1 finished square feet    
  "BsmtFinSF2",       ## Type 2 finished square feet
  "BsmtUnfSF",        ## Unfinished square feet of basement area
  "TotalBsmtSF",        ## Total square feet of basement area
  "FirstFlrSF",       ## First Floor square feet
  "SecondFlrSF",      ## Second floor square feet
  "LowQualFinSF",   ## Low quality finished square feet (all floors)
  "GrLivArea",        ## Above grade (ground) living area square feet
  "GarageArea",     ## Size of garage in square feet
  "WoodDeckSF",     ## Wood deck area in square feet
  "OpenPorchSF",    ## Open porch area in square feet  
  "EnclosedPorch",  ## Enclosed porch area in square feet 
  "ThreeSsnPorch",  ## Three season porch area in square feet 
  "ScreenPorch",    ## Screen porch area in square feet
  "PoolArea"            ## Pool area in square feet
)
## Counts, a house has n of something
variablesCounts <- c(
  "BsmtFullBath",       ## Basement full bathrooms
  "BsmtHalfBath",       ## Basement half bathrooms
  "FullBath",             ## Full bathrooms above grade
  "HalfBath",             ## Half baths above grade
  "BedroomAbvGr",       ## Bedrooms above grade (does NOT include basement bedrooms)
  "KitchenAbvGr",       ## Kitchens above grade
  "TotRmsAbvGrd",       ## Total rooms above grade (does not include bathrooms)
  "Fireplaces",       ## Number of fireplaces
  "GarageCars"      ## Size of garage in car capacity
)
## Values
variablesValues <- c(
  "MiscVal",        ## $ Value of miscellaneous feature
  "SalePrice"       ## $ Price paid
)
## Factors
variablesFactor <- colnames(full.dt)[which(as.vector(full.dt[,sapply(full.dt, class)]) == "character")]
variablesFactor <- c(variablesFactor,  ## Add integers which are factors
                     "MSSubClass",     ## Identifies the type of dwelling involved in the sale
                     "OverallQual",    ## Rates the overall material and finish of the house
                     "OverallCond"     ## Rates the overall condition of the house
)
```

Data conversion
---------------

``` r
## In R first character can not be a number in variable names
setnames(full.dt, c("X1stFlrSF","X2ndFlrSF","X3SsnPorch"), c("FirstFlrSF","SecondFlrSF","ThreeSsnPorch"))
## Set columns to numeric
changeColType <- c(variablesSquareFootage, variablesCounts, variablesValues)
full.dt[,(changeColType):= lapply(.SD, as.numeric), .SDcols = changeColType]
## Set columns to factor
changeColType <- variablesFactor
full.dt[,(changeColType):= lapply(.SD, as.factor), .SDcols = changeColType]
```

Descriptive statistics
======================

statisticts
-----------

``` r
descStats <- describe(full.dt[, c(variablesSquareFootage,variablesValues), with = FALSE]) ## from psych package 
kable(descStats, digits = 2)
```

|               |  vars|     n|       mean|        sd|    median|    trimmed|       mad|    min|     max|   range|   skew|  kurtosis|       se|
|---------------|-----:|-----:|----------:|---------:|---------:|----------:|---------:|------:|-------:|-------:|------:|---------:|--------:|
| LotFrontage   |     1|  2433|      69.31|     23.34|      68.0|      68.44|     17.79|     21|     313|     292|   1.50|     11.26|     0.47|
| LotArea       |     2|  2919|   10168.11|   7887.00|    9453.0|    9499.49|   3023.02|   1300|  215245|  213945|  12.82|    264.31|   145.98|
| MasVnrArea    |     3|  2896|     102.20|    179.33|       0.0|      61.41|      0.00|      0|    1600|    1600|   2.60|      9.23|     3.33|
| BsmtFinSF1    |     4|  2918|     441.42|    455.61|     368.5|     382.44|    546.34|      0|    5644|    5644|   1.42|      6.88|     8.43|
| BsmtFinSF2    |     5|  2918|      49.58|    169.21|       0.0|       1.90|      0.00|      0|    1526|    1526|   4.14|     18.79|     3.13|
| BsmtUnfSF     |     6|  2918|     560.77|    439.54|     467.0|     512.46|    415.13|      0|    2336|    2336|   0.92|      0.40|     8.14|
| TotalBsmtSF   |     7|  2918|    1051.78|    440.77|     989.5|    1034.98|    350.63|      0|    6110|    6110|   1.16|      9.13|     8.16|
| FirstFlrSF    |     8|  2919|    1159.58|    392.36|    1082.0|    1127.14|    348.41|    334|    5095|    4761|   1.47|      6.94|     7.26|
| SecondFlrSF   |     9|  2919|     336.48|    428.70|       0.0|     274.21|      0.00|      0|    2065|    2065|   0.86|     -0.43|     7.93|
| LowQualFinSF  |    10|  2919|       4.69|     46.40|       0.0|       0.00|      0.00|      0|    1064|    1064|  12.08|    174.51|     0.86|
| GrLivArea     |    11|  2919|    1500.76|    506.05|    1444.0|    1453.45|    464.05|    334|    5642|    5308|   1.27|      4.11|     9.37|
| GarageArea    |    12|  2918|     472.87|    215.39|     480.0|     468.42|    183.84|      0|    1488|    1488|   0.24|      0.93|     3.99|
| WoodDeckSF    |    13|  2919|      93.71|    126.53|       0.0|      71.15|      0.00|      0|    1424|    1424|   1.84|      6.72|     2.34|
| OpenPorchSF   |    14|  2919|      47.49|     67.58|      26.0|      33.80|     38.55|      0|     742|     742|   2.53|     10.91|     1.25|
| EnclosedPorch |    15|  2919|      23.10|     64.24|       0.0|       4.94|      0.00|      0|    1012|    1012|   4.00|     28.31|     1.19|
| ThreeSsnPorch |    16|  2919|       2.60|     25.19|       0.0|       0.00|      0.00|      0|     508|     508|  11.37|    149.05|     0.47|
| ScreenPorch   |    17|  2919|      16.06|     56.18|       0.0|       0.00|      0.00|      0|     576|     576|   3.94|     17.73|     1.04|
| PoolArea      |    18|  2919|       2.25|     35.66|       0.0|       0.00|      0.00|      0|     800|     800|  16.89|    297.91|     0.66|
| MiscVal       |    19|  2919|      50.83|    567.40|       0.0|       0.00|      0.00|      0|   17000|   17000|  21.94|    562.72|    10.50|
| SalePrice     |    20|  1460|  180921.20|  79442.50|  163000.0|  170783.29|  56338.80|  34900|  755000|  720100|   1.88|      6.50|  2079.11|

Missing values
--------------

``` r
countIsNA <- sapply(full.dt,function(x)sum(is.na(x)))
countIsNA.df <- data.frame(countIsNA)
countIsNA.df <- data.frame(variableName = row.names(countIsNA.df), countIsNA.df,row.names = NULL)
countIsNA.df <- countIsNA.df[countIsNA >0,]
kable(countIsNA.df)
```

|     | variableName |  countIsNA|
|-----|:-------------|----------:|
| 3   | MSZoning     |          4|
| 4   | LotFrontage  |        486|
| 7   | Alley        |       2721|
| 10  | Utilities    |          2|
| 24  | Exterior1st  |          1|
| 25  | Exterior2nd  |          1|
| 26  | MasVnrType   |         24|
| 27  | MasVnrArea   |         23|
| 31  | BsmtQual     |         81|
| 32  | BsmtCond     |         82|
| 33  | BsmtExposure |         82|
| 34  | BsmtFinType1 |         79|
| 35  | BsmtFinSF1   |          1|
| 36  | BsmtFinType2 |         80|
| 37  | BsmtFinSF2   |          1|
| 38  | BsmtUnfSF    |          1|
| 39  | TotalBsmtSF  |          1|
| 43  | Electrical   |          1|
| 48  | BsmtFullBath |          2|
| 49  | BsmtHalfBath |          2|
| 54  | KitchenQual  |          1|
| 56  | Functional   |          2|
| 58  | FireplaceQu  |       1420|
| 59  | GarageType   |        157|
| 60  | GarageYrBlt  |        159|
| 61  | GarageFinish |        159|
| 62  | GarageCars   |          1|
| 63  | GarageArea   |          1|
| 64  | GarageQual   |        159|
| 65  | GarageCond   |        159|
| 73  | PoolQC       |       2909|
| 74  | Fence        |       2348|
| 75  | MiscFeature  |       2814|
| 79  | SaleType     |          1|
| 81  | SalePrice    |       1459|

(Near) zero variance
--------------------

``` r
zeroVarianceVariables <- nearZeroVar(full.dt, names = TRUE, 
                                     freqCut = 19, uniqueCut = 10)
kable(zeroVarianceVariables)
```

| x             |
|:--------------|
| Street        |
| LandContour   |
| Utilities     |
| LandSlope     |
| Condition2    |
| RoofMatl      |
| BsmtCond      |
| BsmtFinType2  |
| BsmtFinSF2    |
| Heating       |
| LowQualFinSF  |
| KitchenAbvGr  |
| Functional    |
| GarageQual    |
| GarageCond    |
| OpenPorchSF   |
| EnclosedPorch |
| ThreeSsnPorch |
| ScreenPorch   |
| PoolArea      |
| MiscVal       |

Skewness
--------

``` r
## skewness of numerical variables
skewedVariables <- sapply(full.dt[, c(variablesSquareFootage,variablesValues), with = FALSE],function(x){skew(x,na.rm=TRUE)}) ## from psych package
## keep only features that exceed a threshold for skewness
skewedVariables <- skewedVariables[skewedVariables > 0.75]
kable(skewedVariables, digits = 2, caption = "List of skewed variables")
```

|               |      x|
|---------------|------:|
| LotFrontage   |   1.50|
| LotArea       |  12.82|
| MasVnrArea    |   2.60|
| BsmtFinSF1    |   1.42|
| BsmtFinSF2    |   4.14|
| BsmtUnfSF     |   0.92|
| TotalBsmtSF   |   1.16|
| FirstFlrSF    |   1.47|
| SecondFlrSF   |   0.86|
| LowQualFinSF  |  12.08|
| GrLivArea     |   1.27|
| WoodDeckSF    |   1.84|
| OpenPorchSF   |   2.53|
| EnclosedPorch |   4.00|
| ThreeSsnPorch |  11.37|
| ScreenPorch   |   3.94|
| PoolArea      |  16.89|
| MiscVal       |  21.94|
| SalePrice     |   1.88|

Correlation
-----------

To do.

Feature engineering
===================

Set response variable
---------------------

``` r
response <- "SalePrice"
```

Engineering
-----------

This stuff will be done soon.

Remove zero variance variables
------------------------------

Consider engineering to capture information from these variables. On the "to do" list it goes.

``` r
full.dt <- full.dt[, -c(zeroVarianceVariables), with = FALSE]
variablesSquareFootage <- setdiff(c(variablesSquareFootage), c(zeroVarianceVariables))
variablesValues      <- setdiff(c(variablesValues ), c(zeroVarianceVariables))
```

Log transform skewed
--------------------

``` r
# Some items of .SDcols are not column names (or are NA)
# cols <- names(skewedVariables)
# full.dt[, (cols) := lapply(.SD, function(x) log(x)), .SDcols = cols]
```

Scale numerical
---------------

``` r
varScale <- setdiff(c(variablesSquareFootage, variablesValues), c(response)) ## Do not scale response
full.dt <- full.dt[ , (variablesSquareFootage) := lapply(.SD, scale), .SDcols = variablesSquareFootage]
```

Select features
---------------

``` r
features <- setdiff(names(full.dt), c(response, "Id","SalePrice","dataPartition")) 
```
