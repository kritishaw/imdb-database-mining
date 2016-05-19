library(arules)

westeros <- read.csv("game_of_thrones.csv", header = TRUE)
westeros <- merge(westeros[, 2:3], as.data.frame(sapply(westeros[,4:10], as.logical)), by="row.names")

westeros <- as.data.frame(sapply(westeros, gsub,pattern="House ",replacement=""))
westeros <- westeros[, 2:10]

Grules <- apriori(westeros)
inspect(Grules)




#westeros <- as.data.frame(sapply(westeros, gsub,pattern="House ",replacement=""))
gc_rules <- apriori(westeros, parameter = list(supp = 0.01, conf = 0.9), appearance = list(rhs=c("Survives=TRUE", "Survives=FALSE"), default = "lhs"))
inspect(gc_rules)


gotrules_sorted <- sort(gc_rules, by = "lift")
subset.matrix <- is.subset(gotrules_sorted, gotrules_sorted)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
red_gotrules <- colSums(subset.matrix, na.rm = TRUE) >= 1
pruned_gotrules <- gotrules_sorted[!red_gotrules]
inspect(pruned_gotrules)

