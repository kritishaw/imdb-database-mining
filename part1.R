library(cluster)
library(igraph)
library(fpc)
library(rgl)
library(fields)
library(MCL)
library(biclust)

FindClusterPurity <- function(classes, clusters) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}


data <- read.csv("dataset1.csv")
plot(data)
classes <- data[,4]
filter_data <- data[,1:3]
plot3d(filter_data, col=classes, main="Actual Plot")

#####################################################################################################
cat("\nDistance Based Clustering - K means")

set.seed(6716)
kclus <- kmeans(filter_data, centers=8)
table(data.frame(classes, kclus$cluster))
plot3d(filter_data, col=(kclus$cluster+1), main="Kmeans Clustering")
FindClusterPurity(data$cluster, kclus$cluster)

#################################################################################################

cat("\nDensity based clustering - DBSCAN")
set.seed(6716)
dbfit <- fpc::dbscan(filter_data, eps = 1.8, MinPts = 13)
pairs(filter_data, col = dbfit$cluster + 1L)
table(data.frame(classes, dbfit$cluster))
plot3d(filter_data, col=(dbfit$cluster+1), main="DBSCAN Clustering")
FindClusterPurity(data$cluster, dbfit$cluster)

################################################################################################

cat("\nGraph based clustering - MCL")

set.seed(6716)
m <- as.matrix(filter_data)
distances <- rdist(m) 

distancesBin <- binarize(distances, threshold=5)
distancesBin <- distancesBin * -1
distancesBin <- distancesBin + 1
for(i in 1:nrow(distancesBin))
{
  distancesBin[i,i] <- 0 ## diagonal doesn't count
}
mclfit <- mcl(distancesBin, addLoops=FALSE, ESM=TRUE) 

plot3d(filter_data, col=(mclfit$Cluster+1), main="MCL Clustering")
table(data.frame(classes, mclfit$Cluster))
FindClusterPurity(classes, mclfit$Cluster)


#####################################
##Detach statements
detach("package:igraph", unload = TRUE)
detach("package:fpc", unload = TRUE)
detach("package:cluster", unload = TRUE)
detach("package:fields", unload = TRUE)
detach("package:rgl", unload = TRUE)
detach("package:MCL", unload = TRUE)
detach("package:biclust", unload = TRUE)