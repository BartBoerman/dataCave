###################################################################
#### References                                                ####
###################################################################
## https://www.kaggle.com/holar9/house-price-prediction-stacking-top-15-lb
###################################################################
#### Feature engineering                                       ####
###################################################################
overallCond.bin <- c('1' = 1, '2'= 1, '3' = 1, '4' = 2,'5' = 2, '6' = 2, '7' = 3, '8' = 3,
             '      9' = 3)
houseStyle.bin <- c("1Story" = "1Story", 
                    "1.5Fin" = "1.5Story", 
                    "1.5Unf" = "1.5Story",
                    "2.5Unf" = "2.5Story",
                    "2.5Fin" = "2.5Story",
                    "2Story" = "2Story",
                    "SFoyer" = "SFoyer",
                    "SLvl" = "SLvl") 
full.dt[, ':=' (
               # BinOverallCond = as.integer(overallCond.bin[OverallCond]), 
               BinHouseStyle = as.factor(houseStyle.bin[HouseStyle]),
               hasShed = ifelse(MiscFeature == "Shed",1,0),
               hasBsmtUnf = ifelse(BsmtUnfSF > 0,1,0),
               hasLowQualFin = ifelse(LowQualFinSF > 0,1,0),
               hasOpenPorch = ifelse(OpenPorchSF > 0,1,0),
               hasEnclosedPorch = ifelse(EnclosedPorch > 0,1,0),
               hasThreeSsnPorch = ifelse(ThreeSsnPorch > 0,1,0),
               hasScreenPorch = ifelse(ScreenPorch > 0,1,0),
               hasSecondFloor = ifelse(SecondFlrSF > 0,1,0),
               hasHasMasVnr = ifelse(MasVnrArea > 0,1,0),
               hasHasWoodDeck = ifelse(WoodDeckSF > 0,1,0),
               hasBadHeating = ifelse(HeatingQC %in% c("Fa","Po"),1,0),
               hasPavedDrive = ifelse(PavedDrive == "Y",1,0),
               hasElectricalSBrkr = ifelse(Electrical == "SBrkr",1,0),
               hasRegLotShape = ifelse(LotShape == "Reg",1,0),
               hasLvlLandContour = ifelse(LandContour == "Lvl",1,0),
               hasGtlLandSlope = ifelse(LandSlope == "Gtl",1,0),
               hasDetchdGarage = ifelse(GarageType == 'Detchd',1,0),
               hasUnfinishedLevel= ifelse(HouseStyle %in% c("1.5Unf","2.5Unf"),1,0),
               hasRoofMatlWoodCly = ifelse(RoofMatl %in% c("WdShake","WdShngl","ClyTile"),1,0),
               hasRoofTar = ifelse(RoofMatl == "Tar&Grv",1,0), 
               hasRoofCompShg = ifelse(RoofMatl == "CompShg",1,0), 
               isNewerDwelling = ifelse(MSSubClass %in% c(20,60,120),1,0),
               isRemodeled = ifelse(YearRemodAdd == YearBuilt, 1, 0),
               isRemodeledRecent = ifelse(YearRemodAdd == YrSold, 1, 0),
               isNew = ifelse(YearBuilt == YrSold, 1, 0),
               isNormalSale = ifelse(SaleCondition %in% c("Normal","Partial"), 1, 0),
               isConditionNearRail    = ifelse(Condition1 %in% c("RRAe","RRAn","RRNe","RRNn") | Condition2 %in% c("RRAe","RRAn","RRNe","RRNn"),1,0),
               isConditionNearArtery  = ifelse(Condition1 == "Artery" | Condition2 == "Artery",1,0),
               isConditionNearFeedr  = ifelse(Condition1 == "Feedr" | Condition2 == "Feedr",1,0),
               isConditionNearPosFeature  = ifelse(Condition1 %in% c("PosA"," PosN") | Condition2 %in% c("PosA"," PosN"),1,0),
               ageInYears = YrSold - YearBuilt,
               soldHighSeason = ifelse(MoSold %in% c(5,6,7),1,0),
               yearsSinceRemodeled = ifelse(YearRemodAdd == YearBuilt, YrSold - YearRemodAdd, 0),
               sfFloors = FirstFlrSF + SecondFlrSF,
               sfTotal          = (TotalBsmtSF + FirstFlrSF + SecondFlrSF),
               scoreOverall = as.integer(OverallQual) * as.integer(OverallCond),
               scoreBasement = as.integer(BsmtQual) * as.integer(BsmtCond),
               scoreExterior = as.integer(ExterQual) * as.integer(ExterCond),
               countBathrooms = BsmtFullBath +  BsmtHalfBath + FullBath + HalfBath
)]

variablesDrop <- c("BsmtCond",
                "BsmtFullBath",
                "BsmtHalfBath",
                "BsmtQual",
                "BsmtUnfSF",
                "BsmtFinSF1",
                "BsmtFinSF2",
                "Condition1", 
                "Condition2",  
                "Electrical",
                "EnclosedPorch",
                "ExterCond",
                "ExterQual",
                "FirstFlrSF",
                "FullBath",
                "GarageType",
                "HalfBath",
                "HeatingQC",
                "HouseStyle",
                "LandContour",
                "LandSlope",
                "LotShape"
                "LowQualFinSF",
                "MasVnrArea",
                "MiscFeature",
                "MoSold",
                "MSSubClass",
               # "Neighborhood",
                "OpenPorchSF", 
                "OverallCond",
                "OverallQual",
                "PavedDrive",
                "PoolArea",
                "RoofMatl", 
                "SaleCondition",
                "ScreenPorch",
                "SecondFlrSF",
                "ThreeSsnPorch",
                "Utilities",
                "WoodDeckSF",
                "YearBuilt",
                "YearRemodAdd",
                "YrSold")
variablesSquareFootage <- setdiff(variablesSquareFootage, variablesDrop)
variablesValues <- setdiff(variablesValues, variablesDrop)
variablesFactor <- setdiff(variablesFactor, variablesDrop)
response <- "SalePrice"  
features <- setdiff(names(full.dt), c(response, "Id","dataPartition")) 




