redwine <- data.frame(winequality.red)
whitewine <- data.frame(winequality.white)
redwine$color <- 1
whitewine$color <- 0
wines <- rbind(redwine, whitewine)
dim(redwine)
dim(whitewine)
dim(wines)
names(wines)
summary(wines)
str(wines)

library(dplyr)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(car)

windows()
ggcorrplot(cor(wines), hc.order = TRUE,lab = TRUE)

wines.color <- select(wines,-c(quality)) #Target = Color
wines.quality <- select(wines,-c(color)) #Target = Quality
windows()
ggpairs(wines)

#which(wines$pH < 0)

windows()
ggcorrplot(cor(wines.color), hc.order = TRUE,lab = TRUE)
wines.transformed <- log(wines)

#Log Transformed Box Plot
#windows()
# boxplots2 = par(mfrow = c(2,6))
# for ( i in 1:11 ) {
#   boxplot(wines.transformed[[i]])
#   mtext(names(wines.transformed)[i], cex = 0.8, side = 1, line = 2)
# }
# par(boxplots2)


dim(wines.color)
#Removing Outliers
# for ( i in 1:11 ) {
#   outliers <- boxplot(wines.color[[i]])$out
#   print(outliers)
#   wines.color2 <- wines.color[-which(wines.color[[i]] %in% outliers),]
# }

#Boxplot of Removed outliers
# windows()
# boxplots3 = par(mfrow = c(2,6))
# for ( i in 1:11 ) {
#   boxplot(wines.color2[[i]])
#   mtext(names(wines.color2)[i], cex = 0.8, side = 1, line = 2)
# }
# par(boxplots3)

#Removed Outliers Model
wine.model.color.notoutliers <- lm(color ~ .,data = wines.color2)
dim(wines.color2)
summary(wine.model.color.notoutliers)

#Simple Model with Outliers
wine.model.color <- lm(color ~ .,data = wines.color)
summary(wine.model.color)


wines.color$fitted <- wine.model.color$fitted
wines.color$residuals <- wine.model.color$residuals
wines.color$standardized.residuals <- rstandard(wine.model.color)
possible.outliers <- subset(wines.color, standardized.residuals < -1.96 | standardized.residuals > 1.96)
possible.outliers

#Removing Possible Outliers
for ( i in 1:11 ) {
  wines.color3 <- wines.color[-which(wines.color[[i]] %in% possible.outliers),]
}
dim(wines.color3)

#Obtaining Influencial Point by Cooks Distance
wines.color$cooks <- cooks.distance(wine.model.color)
windows()
plot(sort(wines.color$cooks, decreasing=TRUE))
max(wines.color$cooks)

#Removing Influencial Point by Cooks Distance
wines.color.cooks <- filter(wines.color,cooks < 1)
wine.model.color.cooks <- lm(color ~ . -fitted - residuals - standardized.residuals - cooks,data = wines.color.cooks)
summary(wine.model.color.cooks)
plot(wine.model.color.cooks)

vif(wine.model.color.cooks)
mean(vif(wine.model.color.cooks))

wine.model.color.cooks2 <- lm(color ~ .-fitted - residuals - standardized.residuals - cooks - citric.acid - sulphates - free.sulfur.dioxide,data = wines.color.cooks)
summary(wine.model.color.cooks2)

anova(wine.model.color.cooks,wine.model.color.cooks2)
#Scaling Data
wine.scale <- scale(wines.color.cooks)
wine.scale <- data.frame(wine.scale)
wine.model.color.cooks3 <- lm(color ~ . -fitted - residuals - standardized.residuals - cooks,data = wine.scale)
summary(wine.model.color.cooks3)
vif(wine.model.color.cooks3)

# windows()
# ggpairs(wines.transformed)

#Boxplot with Outliers
windows()
boxplots = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(wines.color[[i]])
  mtext(names(wines.color)[i], cex = 0.8, side = 1, line = 2)
}
par(boxplots)

#No Null Values

windows()
pairs(wines.color[, -grep("color",colnames(wines.color))])
dev.off()

#HIstogram with Outliers
windows()
histograms = par(mfrow = c(5,2))
for(i in 1:11){
  hist(wines.color[[i]],xlab = names(wines.color)[i],col = 'red')
}
par(histograms)
dev.off()

#Obtaining Outliers
outliers = c()
for ( i in 1:11 ) {
  stats = boxplot.stats(wines.color[[i]])$stats
  top_outlier_rows = which(wines.color[[i]] > stats[5])
  outliers = c(outliers , top_outlier_rows[ !top_outlier_rows %in% outliers ] )
}

#Cooks Distance
model = lm(color ~ ., data = wines.color)
cooksd = cooks.distance(model)
plot(cooksd, pch = "*", cex = 2, main = "Influence by Cooks distance")
abline(h = 4*mean(cooksd, na.rm = T), col = "red")

head(wines.color[cooksd > 4 * mean(cooksd, na.rm=T), ])

#Removing Outliers
coutliers = as.numeric(rownames(wines.color[cooksd > 4 * mean(cooksd, na.rm=T), ]))
outliers = c(outliers , coutliers[ !coutliers %in% outliers ] )
windows()
clean.wines.color = wines.color[-outliers, ]

#Histogram without Outliers
oldpar = par(mfrow=c(6,2))
for ( i in 1:12 ) {
  hist(clean.wines.color[[i]], xlab = names(clean.wines.color)[i], col = 'lightgreen', main = paste("Average =", signif(mean(clean.wines.color[[i]]),3)))
}
par(oldpar)
dev.off()

windows()
pairs(clean.wines.color, col = clean.wines.color$color, pch = clean.wines.color$color)
#windows()
#pairs(clean.wines.color[,c(7, 8, 10, 11)], col = clean.wines.color$color, pch = clean.wines.color$color)

windows()
ggcorrplot(cor(clean.wines.color), hc.order = TRUE,lab = TRUE)

windows()
clean.boxplots = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(clean.wines.color[[i]])
  mtext(names(clean.wines.color)[i], cex = 0.8, side = 1, line = 2)
}
par(clean.boxplots)

clean.wine.model <- lm(color ~ .,clean.wines.color)
summary(clean.wine.model)

clean.wine.model2 <- lm(color ~ .-sulphates,data = clean.wines.color)
summary(clean.wine.model2)

clean.wine.model3 <- lm(color ~ .-sulphates -citric.acid,data = clean.wines.color)
summary(clean.wine.model3)



vif(clean.wine.model3)
1/vif(clean.wine.model3)
mean(vif(clean.wine.model3))

durbinWatsonTest(clean.wine.model3)
windows()
plot(clean.wine.model3)

