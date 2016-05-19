library(arules)
retailData <- read.csv("retail.csv")
retailData <- retailData[, 2:15]
summary(retailData)

retailData1 <- sapply(retailData, as.logical)

ret_data <- as(retailData1, "transactions")
ret_data
ret_rules <- apriori(ret_data)
inspect(ret_rules)

rconf_rules <- apriori(ret_data, parameter = list(supp = 0.01, conf = 0.9), appearance = list(rhs=c("Beverage", "Meat", "PersonalCare"), default = "lhs"))
inspect(rconf_rules)

#rrules_sorted <- sort(rconf_rules, by = "lift")

subset.matrix <- is.subset(rrules_sorted, rrules_sorted)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
red_rrules <- colSums(subset.matrix, na.rm = TRUE) >= 1
pruned_rrules <- rrules_sorted[!red_rrules]
inspect(pruned_rrules)