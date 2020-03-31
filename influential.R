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
