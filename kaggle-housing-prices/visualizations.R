## http://www.cookbook-r.com/Graphs/Scatterplots_(ggplot2)/
###################################################################
#### Dependencies                                              ####
###################################################################
require(ggplot2)      ## Visualizations
require("gridExtra")  ## Arrange visualizations using grid 
###################################################################
#### Distributions                                             ####
###################################################################
#### Have sale prices and square footages a normal distribution?
## Hint: analyze with hist(), then beautification with ggplot2.
## SalePrice
h1 <- ggplot(train.dt, aes(x = SalePrice)) +
            geom_histogram(breaks=seq(0, 800000, by=25000),alpha = .5) +
            labs(title="SalePrice", x="", y="") +
            theme(text = element_text(size=9)) 

h1.log <- ggplot(train.dt, aes(x = log(SalePrice))) +
            geom_histogram(breaks=seq(10, 14, by=0.25),alpha = .5) +
            labs(title="Log SalePrice",x="", y="") +
            theme(text = element_text(size=9)) 
## GrLivArea
h2 <- ggplot(train.dt, aes(x = GrLivArea)) +
            geom_histogram(breaks=seq(0, 6000, by=100),alpha = .5) +
            labs(title="GrLivArea",x="", y="") +
            theme(text = element_text(size=9)) 
h2.log <- ggplot(train.dt, aes(x = log(GrLivArea))) +
            geom_histogram(breaks=seq(6, 9, by=0.25),alpha = .5) +
            labs(title="Log GrLivArea",x="", y="") +
            theme(text = element_text(size=9)) 
## FirstFlrSF
h3 <- ggplot(train.dt, aes(x = X1stFlrSF)) +
            geom_histogram(breaks=seq(0, 3000, by=100),alpha = .5) +
            labs(title="FirstFlrSF",x="", y="") +
            theme(text = element_text(size=9)) 
h3.log <- ggplot(train.dt, aes(x = log(X1stFlrSF))) +
            geom_histogram(breaks=seq(6, 8, by=0.25),alpha = .5) +
            labs(title="Log FirstFlrSF",x="", y="") +
            theme(text = element_text(size=9)) 
## LotArea
h4 <- ggplot(train.dt, aes(x = LotArea)) +
            geom_histogram(breaks=seq(0, 200000, by=10000),alpha = .5) +
            labs(title="LotArea",x="", y="") +
            theme(text = element_text(size=9)) 
h4.log <- ggplot(train.dt, aes(x = log(LotArea))) +
            geom_histogram(breaks=seq(7, 12, by=0.25),alpha = .5) +
            labs(title="Log LotArea",x="", y="") +
            theme(text = element_text(size=9)) 
## Arrange plots in a grid
grid.arrange(h1, h1.log, h2, h2.log, h3, h3.log, h4, h4.log, 
             ncol = 2, nrow = 4)
#### Nr of sales per neighborhood
p1 <-ggplot(train.dt, aes(x = Neighborhood)) +
          geom_bar(stat = "count", aes(fill = MSZoning)) +
          theme(text = element_text(size=9)) +
          coord_flip()
#### Nr of sales per MSSubClass
p2 <-ggplot(full.dt, aes(x = MSSubClass)) +
          geom_bar(stat = "count", aes(fill = dataPartition)) +
          theme(text = element_text(size=9)) +
          coord_flip()
###################################################################
#### median saleprice                                          ####
###################################################################
p2 <-ggplot(full.dt, aes(x = OverallQual, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9)) 
p2 <-ggplot(full.dt, aes(x = OverallCond, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9)) 
p2 <-ggplot(full.dt, aes(x = ExterQual, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9)) 
p2 <-ggplot(full.dt, aes(x = ExterCond, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9)) 
p2 <-ggplot(full.dt, aes(x = BsmtQual, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9)) 
p2 <-ggplot(full.dt, aes(x = BsmtCond, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9)) 
p2 <-ggplot(full.dt, aes(x = countBathrooms, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9)) 
p2 <-ggplot(full.dt, aes(x =  HouseStyle, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9)) 
p2 <-ggplot(full.dt, aes(x =  hasUnfinishedLevel, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9)) 
p2 <-ggplot(full.dt, aes(x =  GarageCars, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9))
p2 <-ggplot(full.dt, aes(x =  FullBath, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9))
p2 <-ggplot(full.dt, aes(x =  HalfBath+FullBath, y = SalePrice)) +
          geom_bar(position = "dodge", stat = "summary", fun.y = "median", na.rm = T) +
          theme(text = element_text(size=9))

p3a <-ggplot(train.dt, aes(as.factor(FullBath+HalfBath),SalePrice)) +
          geom_boxplot(varwidth = TRUE, fill = "white", colour = "#3366FF", outlier.colour = "red", outlier.shape = 1) +
          theme(text = element_text(size=9)) +   
          coord_flip()

p3a <-ggplot(train.dt, aes(as.factor(BsmtFullBath),SalePrice)) +
          geom_boxplot(varwidth = TRUE, fill = "white", colour = "#3366FF", outlier.colour = "red", outlier.shape = 1) +
          theme(text = element_text(size=9)) +   
          coord_flip()



###################################################################
#### Other graphs                                              ####
###################################################################
#### Boxplot saleprice per neighborhood 
p3a <-ggplot(train.dt, aes(as.factor(Neighborhood),SalePrice)) +
        geom_boxplot(varwidth = TRUE, fill = "white", colour = "#3366FF", outlier.colour = "red", outlier.shape = 1) +
        theme(text = element_text(size=9)) +   
        coord_flip()
#### Boxplot saleprice per MSSubClass 
p3b <-ggplot(train.dt, aes(as.factor(MSSubClass),SalePrice)) +
        geom_boxplot(varwidth = TRUE, fill = "white", colour = "#3366FF", outlier.colour = "red", outlier.shape = 1) +
        theme(text = element_text(size=9)) +   
        coord_flip()
#### Scatterplot saleprice per OverallQual
p4 <-ggplot(train.dt, aes(OverallQual,SalePrice)) +
        geom_point(shape=1) +      ## Use hollow circles
        theme(text = element_text(size=9)) 
#### Scatterplot saleprice per GrLivArea
p5 <-ggplot(train.dt, aes(GrLivArea,SalePrice)) +
        geom_point(shape=1) +      ## Use hollow circles
        geom_smooth(method=lm,     ## Add linear regression line 
                    se=FALSE) +    ## Don't add shaded confidence region
        theme(text = element_text(size=9)) 
#### Scatterplot saleprice per LotArea
p6 <-ggplot(train.dt, aes(LotArea,SalePrice)) +
        geom_point(shape=1) +      ## Use hollow circles
        geom_smooth(method=lm,     ## Add linear regression line 
                    se=FALSE) +    ## Don't add shaded confidence region
        theme(text = element_text(size=9)) 
#### Scatterplot saleprice per GarageArea
p5 <-ggplot(train.dt, aes(GarageArea,SalePrice)) +
  geom_point(shape=1) +      ## Use hollow circles
  geom_smooth(method=lm,     ## Add linear regression line 
              se=FALSE) +    ## Don't add shaded confidence region
  theme(text = element_text(size=9)) 
#### Scatterplot saleprice per FirstFlrSF
p7 <-ggplot(train.dt, aes(X1stFlrSF,SalePrice)) +
        geom_point(shape=1) +      ## Use hollow circles
        geom_smooth(method=lm,     ## Add linear regression line 
                    se=FALSE) +    ## Don't add shaded confidence region
        theme(text = element_text(size=9)) 
#### Scatterplot FirstFlrSF vs GrLivArea
p8 <-ggplot(train.dt, aes(GrLivArea,X1stFlrSF)) +
        geom_point(shape=1) +      ## Use hollow circles
        geom_smooth(method=lm,     ## Add linear regression line 
                    se=FALSE) +    ## Don't add shaded confidence region
        theme(text = element_text(size=9)) 
#### Scatterplot LotArea vs GrLivArea
p9 <-ggplot(train.dt, aes(GrLivArea,LotArea)) +
        geom_point(shape=1) +      ## Use hollow circles
        geom_smooth(method=lm,     ## Add linear regression line 
                    se=FALSE) +    ## Don't add shaded confidence region
        theme(text = element_text(size=9)) 







