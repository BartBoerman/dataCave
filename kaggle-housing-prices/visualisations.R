## convert to R data frame

full.df <- as.data.frame(full.hex)
train.df <- as.data.frame(train.hex)
View(full.df)

str(full.df)

hist(log10(as.numeric(full.df$LotArea)))

hist(log(full.df$X1stFlrSF))

hist(log(full.df$X2ndFlrSF))

x<- as.vector(full.df$Neighborhood)

require(ggplot2)


p1 <-ggplot(full.df, aes(x = Neighborhood)) +
      geom_bar(stat = "count", aes(fill = MSZoning)) +
      theme(text = element_text(size=9)) +
      coord_flip()

train.df$SalePrice <- as.numeric(train.df$SalePrice)



p2 <-ggplot(train.df, aes(Neighborhood,SalePrice)) +
        geom_boxplot(varwidth = TRUE, fill = "white", colour = "#3366FF", outlier.colour = "red", outlier.shape = 1) +
        theme(text = element_text(size=9)) +   
        coord_flip()




temp = aggregate(SalePrice ~ Neighborhood, data = full.df, FUN = "max")



