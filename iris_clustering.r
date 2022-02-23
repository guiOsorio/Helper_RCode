rm(list = ls())

# https://www.youtube.com/watch?v=KmYUE7Of5rU

install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)

View(iris)

mydata <- iris[-c(5)]

# Checks the optimal number of clusters
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(mydata)

# K-means cluster
KM <- kmeans(mydata,2)

# Cluster plot
autoplot(KM, mydata, frame=TRUE)

# Cluster centers
KM$centers
