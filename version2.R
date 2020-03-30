redwine <- data.frame(winequality.red)
whitewine <- data.frame(winequality.white)
# redwine$color <- 1
# whitewine$color <- 0
wines <- rbind(redwine, whitewine)
dim(redwine)
dim(whitewine)
dim(wines)
names(wines)
summary(wines)
str(wines)
View(wines)

library(leaps)
library(ggplot2)
library(reshape2)
library(MASS)
library(ggcorrplot)
library(plotmo)
windows()
boxplots_new = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(wines[[i]])
  mtext(names(wines)[i], cex = 0.8, side = 1, line = 2)
}
par(boxplots_new)
dev.off()
#We now use a scatter plot matrix to get an insight on the outliers locations
windows()
pairs(wines[, -grep("quality", colnames(wines))])
dev.off()

#Now we look at the predictor values distribution
windows()
histograms_new = par(mfrow = c(6,2))
for ( i in 1:12 ) {
  truehist(wines[[i]], xlab = names(wines)[i], col = 'lightgreen', main = paste("Average =", signif(mean(wines[[i]]),3)))
}
par(histograms_new)

#For each variables, we consider observations that lie outside 1.5 * IQR as outliers
outliers = c()
for ( i in 1:11 ) {
  stats = boxplot.stats(wines[[i]])$stats
  bottom_outlier_rows = which(wines[[i]] < stats[1])
  top_outlier_rows = which(wines[[i]] > stats[5])
  outliers = c(outliers , top_outlier_rows[ !top_outlier_rows %in% outliers ] )
  outliers = c(outliers , bottom_outlier_rows[ !bottom_outlier_rows %in% outliers ] )
}

#We use the Cook’s ditance to detect influential observations
windows()
mod = lm(quality ~ ., data = wines)
cooksd = cooks.distance(mod)
plot(cooksd, pch = "*", cex = 2, main = "Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm = T), col = "red")

head(wines[cooksd > 4 * mean(cooksd, na.rm=T), ])

#We remove all the ouliers in our list from the dataset and create a new set of histograms

coutliers = as.numeric(rownames(wines[cooksd > 4 * mean(cooksd, na.rm=T), ]))
outliers = c(outliers , coutliers[ !coutliers %in% outliers ] )

cleanWhiteDat = wines[-outliers, ]
windows()
oldpar = par(mfrow=c(6,2))
for ( i in 1:12 ) {
  truehist(cleanWhiteDat[[i]], xlab = names(cleanWhiteDat)[i], col = 'lightgreen', main = paste("Average =", signif(mean(cleanWhiteDat[[i]]),3)))
}
par(oldpar)

dim(cleanWhiteDat)
skim(cleanWhiteDat)
#By removing the outliers, the dataset size reduced to 4899 observations.

# Aish..! After removing the outliers, the dataset size is reduced to '7970' observations..

#We now use a scatterplot matrice to roughly determine if there is a linear correlation between our variables
windows()
pairs(cleanWhiteDat, col = cleanWhiteDat$quality, pch = cleanWhiteDat$quality)
dev.off()
windows()
pairs(cleanWhiteDat[,c(7, 8, 10, 11)], col = cleanWhiteDat$quality, pch = cleanWhiteDat$quality)


#The following correlation matrix confirm the correlation between variables.
windows()
ggcorrplot(cor(cleanWhiteDat), hc.order = TRUE, type = "lower", lab = TRUE, insig = "blank")
# correlation matrix confirms strong correlation between residual.sugar/density and density/alcohol. 
# It also confirm that alcohol is the variable with the highest correlation with quality. 

# Then I create a shortcut function to sort the dataframe columns by the absolute value of their 
# correlation with the outcome.

sortByCorr = function(dataset, refColName) {
  # Sort the dataframe columns by the absolute value of their correlation with
  # a given column
  #
  # Args:
  #   dataset: A vector, matrix, or data frame to sort
  #   refColName: The name of the reference colum for the correlation
  #
  # Returns:
  #   The sorted dataframe
  refColIdx = grep(refColName, colnames(dataset))
  corrTmp = cor(dataset)[, refColIdx]
  corrTmp[order(abs(corrTmp), decreasing = TRUE)]
  
  dataset[, order(abs(corrTmp), decreasing = TRUE)]
}

colnames(sortByCorr(dataset = cleanWhiteDat, refColName = 'quality'))

windows()
pairs(log(cleanWhiteDat))


# In order to have a point of comparison for the models to comes,
# we fit an initial model for each dataset with all variables

wine.fit = lm(quality ~ .,cleanWhiteDat)
summary(wine.fit)
#Calculating the MSE
mean(wine.fit$residuals^2)

# The most important variables are: fixed.acidity, volatile.acidity, residual.sugar, free.sulfur.dioxide, 
# density, pH, sulphates and alcohol.
# citric.acid and chlorides doesn’t seems to be relevant in this model.
# Adjusted R2 = 0.3247
# MSE = 0.4414354

wine.fit2 = lm(quality ~ . - citric.acid - chlorides, data = cleanWhiteDat)
summary(wine.fit2)
#Calculating the MSE
mean(wine.fit2$residuals^2)
vif(wine.fit2)
#Multicollinearity test (VIF)
#Based on vif density has value of 18.72 which is greater than 10. Hence we can remove density from the model

wine.fit3 = lm(quality ~ ., -citric.acid - chlorides - density, data = cleanWhiteDat)
summary(wine.fit3)
#Calculating the MSE
mean(wine.fit3$residuals^2)
