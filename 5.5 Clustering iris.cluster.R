
# k-means clustering with kmeans function ---------------------------------
library(datasets)
library(ggplot2)
head(iris)

iris <- as.data.frame(iris)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

set.seed(20)
irisCluster <- kmeans(iris[, 1:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)

iris$cluster <- irisCluster$cluster
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster)) + geom_point()




