redwine <- data.frame(winequality.red)
whitewine <- data.frame(winequality.white)
redwine$color <- 1
whitewine$color <- 0
wines <- rbind(redwine, whitewine)
dim(redwine)
dim(whitewine)
dim(wines)
names(wines)

library(dplyr)
library(ggplot2)

wines.color <- select(wines,-c(quality))
wines.quality <- select(wines,-c(color))

windows()
boxplots = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(wines.color[[i]])
  mtext(names(wines.color)[i], cex = 0.8, side = 1, line = 2)
}
par(boxplots)

windows()
pairs(wines.color[, -grep("color",colnames(wines.color))])
dev.off()

windows()
histograms = par(mfrow = c(5,2))
for(i in 1:11){
  hist(wines.color[[i]],xlab = names(wines.color)[i],col = 'red')
}
par(histograms)
dev.off()

outliers = c()
for ( i in 1:11 ) {
  stats = boxplot.stats(wines.color[[i]])$stats
  top_outlier_rows = which(wines.color[[i]] > stats[5])
  outliers = c(outliers , top_outlier_rows[ !top_outlier_rows %in% outliers ] )
}

model = lm(color ~ ., data = wines.color)
cooksd = cooks.distance(model)
plot(cooksd, pch = "*", cex = 2, main = "Influence by Cooks distance")
abline(h = 4*mean(cooksd, na.rm = T), col = "red")

head(wines.color[cooksd > 4 * mean(cooksd, na.rm=T), ])

library(ggcorrplot)

coutliers = as.numeric(rownames(wines.color[cooksd > 4 * mean(cooksd, na.rm=T), ]))
outliers = c(outliers , coutliers[ !coutliers %in% outliers ] )
windows()
clean.wines.color = wines.color[-outliers, ]
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

library(car)

vif(clean.wine.model3)
1/vif(clean.wine.model3)
mean(vif(clean.wine.model3))

durbinWatsonTest(clean.wine.model3)
windows()
plot(clean.wine.model3)
