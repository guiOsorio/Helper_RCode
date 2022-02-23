# https://www.youtube.com/watch?v=xKl4LJAXnEA
data(iris)
head(iris)
summary(iris)

# PCA
myPr <- prcomp(iris[,1:4], scale = TRUE)
plot(scale(iris$Sepal.Length), scale(iris$Sepal.Width))
myPr
summary(myPr)
plot(myPr, type = "l")
biplot(myPr, scale = 0)

# EXTRACT PC SCORES
str(myPr)
myPr$x
iris2 <- cbind(iris, myPr$x[,1:2])
head(iris2)

# PLOT WITH GGPLOT
library(ggplot2)
ggplot(iris2, aes(PC1, PC2, col = Species, fill = Species)) +
  stat_ellipse(geom = 'polygon', col = 'black', alpha = 0.5) +
  geom_point(shape = 21, col = 'black')

# CORRELATIONS BETWEEN VARIABLES AND PRINCIPAL COMPONENTS
cor(iris[,1:4], iris2[,6:7])
