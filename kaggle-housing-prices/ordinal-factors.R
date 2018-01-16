###################################################################
#### Set order for ordinal factors                             ####
###################################################################
## OverallQual, rates the overall material and finish of the house
full.dt[,OverallQual:=ordered(OverallQual, levels = c(1:10))]
## OverallCond, rates the overall condition of the house
full.dt[,OverallCond:=ordered(OverallCond, levels = c(1:10))]
## KitchenQual, kitchen quality
full.dt[,KitchenQual:=ordered(KitchenQual, levels = c("Po","Fa","TA","Gd","Ex"))]
## GarageFinish (contains NA's)
full.dt[,GarageFinish:=ordered(GarageFinish, levels = c("None","Unf","RFn","Fin"))]
## ExterQual, evaluates the quality of the material on the exterior  
full.dt[,ExterQual:=ordered(ExterQual, levels = c("Po","Fa","TA","Gd","Ex"))]
## ExterCond, evaluates the present condition of the material on the exterior
full.dt[,ExterCond:=ordered(ExterCond, levels = c("Po","Fa","TA","Gd","Ex"))]
## BsmtQual (contains NA's), evaluates the height of the basement
full.dt[,BsmtQual:=ordered(BsmtQual, levels = c("None","Po","Fa","TA","Gd","Ex"))]
## BsmtCond (contains NA's), evaluates the general condition of the basement
full.dt[,BsmtCond:=ordered(BsmtCond, levels = c("None","Po","Fa","TA","Gd","Ex"))]
## BsmtExposure (contains NA's), refers to walkout or garden level walls
full.dt[,BsmtExposure:=ordered(BsmtExposure, levels = c("None","No","Mn","Av","Gd"))]
## BsmtFinType1 (contains NA's), rating of basement finished area
full.dt[,BsmtFinType1:=ordered(BsmtFinType1, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))]
## FireplaceQu (contains NA's), fireplace quality
full.dt[,FireplaceQu:=ordered(FireplaceQu, levels = c("None","Po","Fa","TA","Gd","Ex"))]
## Electrical
full.dt[,Electrical:=ordered(Electrical, levels = c("FuseP","Mix","FuseF","FuseA","SBrkr"))]
## Fence
full.dt[,Fence:=ordered(Fence, levels = c("None","MnWw","MnPrv","GdWo","GdPrv"))]
## PoolQC
full.dt[,PoolQC:=ordered(PoolQC, levels = c("None","Fa","Gd","Ex"))]
## Ordered factors are not supported by h2o, Let's convert them into integers during pre-processing. Lowest level will be 1 etc.