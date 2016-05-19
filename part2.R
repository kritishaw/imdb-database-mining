library(cluster)
library(igraph)
library(fpc)
library(fields)
library(rgl)
library(MCL)
library(biclust)

FindClusterPurity <- function(classes, clusters) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

data <- read.csv("dataset1.csv")
plot(data)
classes <- data[,4]
filter_data <- data[,1:3]

x <- filter_data[,1]
x <- x/4
y <- filter_data[,2]
y <- y/2
z <- filter_data[,3]
z <- z
data2 <- data.frame(x=x,y=y,z=z)

set.seed(6716)
kclus <- kmeans(data2, centers=8)
table(data.frame(classes, kclus$cluster))

plot3d(filter_data, col=classes, main="Actual Plot")

plot3d(data2, col=(kclus$cluster+1), main="Kmeans Clustering")

FindClusterPurity(classes, kclus$cluster)

##########################
##Detach statements
detach("package:igraph", unload = TRUE)
detach("package:fpc", unload = TRUE)
detach("package:cluster", unload = TRUE)
detach("package:fields", unload = TRUE)
detach("package:rgl", unload = TRUE)
detach("package:MCL", unload = TRUE)
detach("package:biclust", unload = TRUE)