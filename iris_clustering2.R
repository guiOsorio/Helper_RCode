# https://www.youtube.com/watch?v=PX5nSBGB5Tw

data(iris)
plot(iris)

# SCALE DATA
irisScaled <- scale(iris[, -5])

# K-MEANS CLUSTERING
## CLUSTERING
fitK <- kmeans(irisScaled, 3)
fitK
str(fitK)
plot(iris, col = fitK$cluster)

## CHOOSING K
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(irisScaled, i)
}
k

betweenss_totss <- list()
for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

plot(c(1:10), betweenss_totss, type = "b",
     ylab = "Between SS / Total SS",
     xlab = "Clusters(k)")

for(i in 1:4){
  plot(iris, col = k[[i]]$cluster)
}


# HIERARCHICAL CLUSTERING
d <- dist(irisScaled)
fitH <- hclust(d, "ward.D2")
plot(as.dendrogram(fitH))
plot(fitH)
rect.hclust(fitH, k = 3, border = "red")
clusters <- cutree(fitH, 3)
clusters
plot(iris, col = clusters)


# MODEL-BASED CLUSTERING
library(mclust)
fitM <- Mclust(irisScaled)
fitM
plot(fitM)

# DENSITY-BASED CLUSTERING
install.packages("dbscan")
library(dbscan)
kNNdistplot(irisScaled, k = 3)
abline(h = 0.7, col = "red", lty = 2)
fitD <- dbscan(irisScaled, eps = 0.7, minPts = 5)
fitD
plot(iris, col = fitD$cluster)
