sink("logs.txt")

#Loading all packages required
library(scatterplot3d)
library(klaR)
library(rgl)
library(cluster)
library(igraph)
library(fpc)
library(fields)
library(rgl)
library(MCL)
library(biclust)


#Finding the cluster purity function
FindClusterPurity <- function(classes, clusters) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}


#Running kmodes algorithm for different subsets of features in order to compare the results taking the value of k = 25 which is the number of genres

forClustering2 <- read.csv("Clustering2.csv")
act2<-forClustering2
(cl2 <- kmodes(act2, 25))
cl2$cluster
FindClusterPurity(forClustering2$Genre.1, cl2$cluster)
plot3d(forClustering2$Genre.1, col=(cl2$cluster+1), main="KMod Clustering")

#//////////////////////////////////////////////////////////////////////////////////////////////


forClustering3 <- read.csv("Clustering3.csv")
act3<-forClustering3[,2:5]
(cl3 <- kmodes(act3, 25))
cl3$cluster
FindClusterPurity(forClustering3$Genre.1, cl3$cluster)
plot3d(forClustering3$Genre.1, col=(cl3$cluster+1), main="KMod Clustering")

#//////////////////////////////////////////////////////////////////////////////////////////////

forClustering4 <- read.csv("Clustering4.csv")
act4<-forClustering4[,2:10]
(cl4 <- kmodes(act4, 25))
cl4$cluster
FindClusterPurity(forClustering4$Genre.1, cl4$cluster)
plot3d(forClustering4$Genre.1, col=(cl4$cluster+1), main="KMod Clustering")


#//////////////////////////////////////////////////////////////////////////////////////////////

forClustering5 <- read.csv("Clustering5.csv")
act5<-forClustering5[,2:6]
(cl5 <- kmodes(act5, 25))
cl5$cluster
FindClusterPurity(forClustering5$Genre.1, cl5$cluster)
plot3d(forClustering5$Genre.1, col=(cl5$cluster+1), main="KMod Clustering")

#//////////////////////////////////////////////////////////////////////////////////////////////

forClustering6 <- read.csv("Clustering6.csv")
act6<-forClustering6[,2:4]
(cl6 <- kmodes(act6, 25))
cl6$cluster
FindClusterPurity(forClustering6$Genre.1, cl6$cluster)
plot3d(forClustering6$Genre.1, col=(cl6$cluster+1), main="KMod Clustering")

#//////////////////////////////////////////////////////////////////////////////////////////////

forClustering7 <- read.csv("Clustering7.csv")
act7<-forClustering7[,2:6]
(cl7 <- kmodes(act7, 25))
cl7$cluster
FindClusterPurity(forClustering7$Genre.1, cl7$cluster)
plot3d(forClustering7$Genre.1, col=(cl7$cluster+1), main="KMod Clustering")

#//////////////////////////////////////////////////////////////////////////////////////////////

#Running the kmods algorithm for higher values of k in order to compare the results

forClustering5 <- read.csv("Clustering5.csv")
act5<-forClustering5[,2:7]
(cl5 <- kmodes(act5, 40))
cl5$cluster
FindClusterPurity(forClustering5$Genre.1, cl5$cluster)
plot3d(mydata, col=(cl5$cluster+1), main="KMod Clustering")

#//////////////////////////////////////////////////////////////////////////////////////////////

forClustering7 <- read.csv("Clustering7.csv")
act7<-forClustering7[,2:6]
(cl7 <- kmodes(act7, 50))
cl7$cluster
FindClusterPurity(forClustering7$Genre.1, cl7$cluster)
plot3d(forClustering7$Genre.1, col=(cl7$cluster+1), main="KMod Clustering")

#//////////////////////////////////////////////////////////////////////////////////////////////

#Running the Daisy Agnes method(Agglomerative Clustering) for different subsets of features taking metric as Manhattan


data = read.csv("Clustering7.csv")
dissMat = daisy(data[, 2:6], metric = "manhattan")
results = agnes(dissMat, diss = inherits(dissMat, "dist"), metric = "manhattan",
                stand = FALSE, method = "average", keep.data = FALSE)

#//////////////////////////////////////////////////////////////////////////////////////////////


data5 = read.csv("Clustering5.csv")
dissMat5 = daisy(data5[, 2:7], metric = "manhattan")
results5 = agnes(dissMat5, diss = inherits(dissMat5, "dist"), metric = "manhattan",
                stand = FALSE, method = "average", keep.data = FALSE)

#//////////////////////////////////////////////////////////////////////////////////////////////

#Running the Daisy method(Agglomerative Clustering) for different subsets of features taking metric as gower


daisy.mat <- daisy(act5, metric="gower")
clus <- pam(daisy.mat, 25, diss=TRUE)
# table(data.frame(genreclasses, clus$cluster))
FindClusterPurity(forClustering5$Genre.1, clus$cluster)

#//////////////////////////////////////////////////////////////////////////////////////////////

daisy.mat <- daisy(act7, metric="gower")
clus <- pam(daisy.mat, 25, diss=TRUE)
# table(data.frame(genreclasses, clus$cluster))
FindClusterPurity(forClustering7$Genre.1, clus$cluster)

#//////////////////////////////////////////////////////////////////////////////////////////////

#Running the Daisy method(Agglomerative Clustering) for higher values of k


daisy.mat <- daisy(act5, metric="gower")
clus <- pam(daisy.mat, 40, diss=TRUE)
# table(data.frame(genreclasses, clus$cluster))
FindClusterPurity(forClustering5$Genre.1, clus$cluster)

#//////////////////////////////////////////////////////////////////////////////////////////////


daisy.mat <- daisy(act6, metric="gower")
clus <- pam(daisy.mat, 40, diss=TRUE)
# table(data.frame(genreclasses, clus$cluster))
FindClusterPurity(forClustering6$Genre.1, clus$cluster)

#//////////////////////////////////////////////////////////////////////////////////////////////

#Running the Kmeans algorithm to cluster the movies based on ratings
  
  forClustering <- read.csv("Ratings.csv")
act<-as.numeric(as.character(forClustering$Rating))
act<-na.omit((act))
cl <- kmeans(act,25)
cl$cluster
FindClusterPurity(forClustering$Genre.1, cl$cluster)

#//////////////////////////////////////////////////////////////////////////////////////////////


