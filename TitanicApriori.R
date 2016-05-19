library(arules)

titanicData = read.csv(file = "titanic.csv")
summary(titanicData)

trules <- apriori(titanicData)
inspect(trules)

trules1 <- apriori(titanicData, parameter = list(minlen=1, supp=0.01, conf=0.9),appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"), control = list(verbose=F))
inspect(trules1)

trules1_sorted <- sort(trules1, by="lift")
inspect(trules1_sorted)

subset.matrix <- is.subset(trules1_sorted, trules1_sorted)

subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
pruned_trules <- trules1_sorted[!redundant]
inspect(pruned_trules)
