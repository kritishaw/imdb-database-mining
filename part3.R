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

data <- read.csv("dataset2.csv")
classes <- data[,6]
filter_data <- data[,2:5]

plot3d(mydata, col=as.numeric(classes)+2, main="Actual Plot")

cat("\nDistance Based Clustering -- K-Means")
wss <- (nrow(filter_data)-1)*sum(apply(filter_data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(filter_data,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

set.seed(6716)
kclus <- kmeans(filter_data, centers=2)
table(data.frame(origclasses, kclus$cluster))

plot3d(filter_data, col=(kclus$cluster+1), main="Kmeans Clustering")
FindClusterPurity(classes, kclus$cluster)

##############################
##Detach statements
detach("package:igraph", unload = TRUE)
detach("package:fpc", unload = TRUE)
detach("package:cluster", unload = TRUE)
detach("package:fields", unload = TRUE)
detach("package:rgl", unload = TRUE)
detach("package:MCL", unload = TRUE)
detach("package:biclust", unload = TRUE)