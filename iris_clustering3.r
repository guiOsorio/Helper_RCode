## K-MEANS CLUSTERING DEMONSTRATION (IRIS DATASET)
# FROM https://www.youtube.com/watch?v=sAtnX3UJyN0

View(iris)

Iris = iris
Iris.features = Iris
Iris.features$Species <- NULL
View(Iris.features)

results <- kmeans(Iris.features, 3)
results

table(Iris$Species, results$cluster)

plot(Iris[,3:4], col = results$cluster)
plot(Iris[,3:4], col = Iris$Species)

plot(Iris[,1:2], col = results$cluster)
plot(Iris[,1:2], col = Iris$Species)
