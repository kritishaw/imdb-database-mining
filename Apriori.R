#Find all the rules
# Prereq : package arules is installed
BruteRules <- function(dataset){
  library(arules)
  #the default value for support and confidence is 0.1 and 0.8
  apriori(dataset, parameter = list(minlen=1, support = 0.01, confidence = 0.01))
}

#selecting the specified rules with support = 0.01; confidence =0.90
ReqRule <- function(dataset, appList){
  library('arules')
  apriori(dataset, parameter = list(minlen=1, support = 0.01, confidence = 0.9), appearance = appList)
}

#selecting unique rules
RedundantRule <- function(rules){
  subset.matrix <- is.subset(rules, rules)
  subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
  redRules <- colSums(subset.matrix, na.rm=T) >= 1
}

NonRedundantRule <- function(rules, redRules){
  rulesPruned <- rules[!redRules]
  sorted <- sort(rulesPruned, by="lift")
}

#####GAME OF THRONES DATASET#####
getGOT <- function(){
  #read and preprocess the Game Of Thrones Data
  andals <- read.csv("game_of_thrones.csv", header = TRUE)
  andals <- merge(andals[, 2:3], as.data.frame(sapply(andals[,4:10], as.logical)), by="row.names")
  andals <- as.data.frame(sapply(andals, gsub,pattern="House ",replacement=""))
  andals <- andals[, 2:10]
  andals
}

fire_ice <- getGOT()
appListG <- list(rhs=c("Survives=TRUE", "Survives=FALSE"), default = "lhs") 

#Find all rules
Grules1 <- BruteRules(fire_ice)
Grules1

#Find rules with reqd criteria
Grules2 <- ReqRule(fire_ice, appListG)
inspect(Grules2)

#Redundant Rules
Gred <- RedundantRule(Grules2)
which(Gred)

#Non Redundant Rules
Grules3 <- NonRedundantRule(Grules2, Gred)
inspect(Grules3)
Grules3

#######GoT-QUERIES##############
rulesG <- Grules3
GOT_q1 <- function(){
  #Generating Rules for different possibilities of Nobility and Gender values 
  rulesNobility <- subset(rulesG,  (rhs %in% c("Survives=TRUE")) & ((lhs %in% c("Nobility=TRUE") )))
  rulesNoNobility <- subset(rulesG,  (rhs %in% c("Survives=TRUE")) & ((lhs %in% c("Nobility=FALSE") )))
  
  rulesMale <- subset(rulesG,  (rhs %in% c("Survives=TRUE")) & lhs %pin% c("Gender=M"))
  rulesFemale <- subset(rulesG,  (rhs %in% c("Survives=TRUE")) & lhs %pin% c("Gender=F"))
  
  rulesMaleNobility <- subset(rulesNobility,  (rhs %in% c("Survives=TRUE")) & lhs %pin% c("Gender=M"))
  rulesFemaleNobility <- subset(rulesNobility,  (rhs %in% c("Survives=TRUE")) & lhs %pin% c("Gender=F"))
  
  rulesMaleNoNobility <- subset(rulesNoNobility,  (rhs %in% c("Survives=TRUE")) & lhs %pin% c("Gender=M"))
  rulesFemaleNoNobility <- subset(rulesNoNobility,  (rhs %in% c("Survives=TRUE")) & lhs %pin% c("Gender=F"))
  
  inspect(rulesNobility)
  inspect(rulesNoNobility)
  
  inspect(rulesMale)
  inspect(rulesFemale)
  
  inspect(rulesMaleNobility)
  inspect(rulesFemaleNobility)
  
  inspect(rulesMaleNoNobility)
  inspect(rulesFemaleNoNobility)
}

Jon <- function(){
#Rules which point to Jon Snow not "Survives=TRUE"
  rulesJDies1 <- subset(rulesG, rhs %in% c("Survives=TRUE") & lhs %in% c("House=Night's Watch") & lhs %pin% c("Gender=M") & lhs %in% c("Book1=FALSE"))
  rulesJDies2 <- subset(rulesG, rhs %in% c("Survives=TRUE") & lhs %in% c("House=Night's Watch") & lhs %pin% c("Gender=M") & lhs %in% c("Book2=FALSE"))
  rulesJDies3 <- subset(rulesG, rhs %in% c("Survives=TRUE") & lhs %in% c("House=Night's Watch") & lhs %pin% c("Gender=M") & lhs %in% c("Book3=FALSE"))
  rulesJDies4 <- subset(rulesG, rhs %in% c("Survives=TRUE") & lhs %in% c("House=Night's Watch") & lhs %pin% c("Gender=M") & lhs %in% c("Book5=FALSE"))
  
  rulesJDies1
  rulesJDies2
  rulesJDies3
  rulesJDies4
}

Sansa <- function(){
  #Rules to support that Sansa Stark Survives
  rulesSansa <- subset(rulesG, rhs %in% c("Survives=TRUE") & lhs %in% c("House=Stark") & lhs %in% c("Gender=F"))
  inspect(rulesSansa)
}

##########RETAIL DATASET#####################

getRetail <- function(){
  #read and preprocess the Retail data
  retailData <- read.csv("retail.csv")
  retailData <- retailData[, 2:15]
  retailData <- sapply(retailData, as.logical)
  retailData <- as(retailData, "transactions")
  retailData
}

ret_data <- getRetail()
appListR <- list(rhs=c("Beverage", "Meat", "PersonalCare"), default = "lhs")

#Find all Rules
Rrules1 <- BruteRules(ret_data)
Rrules1

#Find rules with reqd criteria
Rrules2 <- ReqRule(ret_data, appListR)
inspect(Rrules2)

#Redundant Rules
Rred <- RedundantRule(Rrules2)
which(Rred)

#Non Redundant Rules
Rrules3 <- NonRedundantRule(Rrules2, Rred)
inspect(Rrules3)
Rrules3

###########TITANIC DATASET####################

titanicData = read.csv(file = "titanic.csv")
appListT <- list(rhs=c("Survived=No", "Survived=Yes"), default="lhs")

#Find All Rules
Trules1 <- BruteRules(titanicData)
Trules1
inspect(Trules1)

#Find Rules with criteria
Trules2 <- ReqRule(titanicData, appListT)
inspect(Trules2)

#Redundant Rules
Tred <- RedundantRule(Trules2)
which(Tred)

#Non Redundant Rules
Trules3 <- NonRedundantRule(Trules2, Tred)
inspect(Trules3)
Trules3
