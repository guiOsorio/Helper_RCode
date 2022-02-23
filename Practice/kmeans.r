# 1
set.seed(1)
km1 <- kmeans(iris[,grep("Sepal", names(iris))], centers = 3)
clusters1 <- km1$cluster

# 2
iris2 <- data.frame(iris, Cluster = clusters1)
table2 <- with(iris2, table(Cluster, Species))
table2 / colSums(table2)

# 3
library(ggplot2)
iris2 %>%
  ggplot(aes(Sepal.Length, Sepal.Width, color = Cluster, shape = Species)) +
  geom_point(size = 3, alpha = 0.4) +
  theme_bw()

# 4
km2 <- kmeans(iris[, 1:4], centers = 3)
clusters2 <- km2$cluster
iris3 <- data.frame(iris, Cluster = clusters2)
table3 <- with(iris3, table(Cluster, Species))
table3 / colSums(table3)

# 5
iris4 <- iris
iris4$Sepal.Width <- iris$Sepal.Width * 2
km3 <- kmeans(iris4[, 1:4], centers = 3)
clusters3 <- km3$cluster
iris5 <- data.frame(iris4, Cluster = clusters3)
table4 <- with(iris5, table(Cluster, Species))
table4 / colSums(table4)

# 6
iris6a <- scale(iris5[, -5])
km6a <- kmeans(iris6a, centers = 3)
clusters6a <- km6a$cluster
clusters6a
iris6a <- data.frame(iris6a, Cluster = clusters6a)

iris6b <- scale(iris[, -5])
km6b <- kmeans(iris6b, centers = 3)
clusters6b <- km6b$cluster
iris6b <- data.frame(iris6b, Cluster = clusters6b)

table(clusters6a, clusters6b)

# 7
tita <- read.csv('./train.csv')
tita$Sex <- ifelse(tita$Sex == 'male', 1L, 0L)
tita$Pclass <- ifelse(tita$Pclass == 3, 1L, 0L)

set.seed(1)
tita_sc1 <- scale(tita[, c("Sex", "SibSp", "Parch", "Fare")])
kmtita1 <- kmeans(tita_sc1, 4, nstart = 20)

# 8
kmtita1$centers
tita_table <- table(tita$Survived, kmtita1$cluster)
tita_table[2,] / colSums(tita_table)

# 9
ks <- 2:20
wss <- lapply(
  ks,
  function(k) {
    kmeans(tita_sc1, k , nstart = 20)
  }
)

# 10
wss_bt <- lapply(
  wss,
  function(x) {
    x$betweenss / x$totss
  }
)
plot(ks, wss_bt, type = "b", xlab = "k", ylab = "between_SS / total_SS")
# 5 clusters would be more fitting