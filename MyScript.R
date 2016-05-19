install.packages("RWeka")
install.packages("partykit")
install.packages("oblique.tree")
install.packages("class")
install.packages("e1071")
install.packages("caret")

#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version


library(rJava)
library(RWeka)
library(datasets)
library(partykit)
library(oblique.tree)
library(class) 
library(e1071)
library(caret)

#to divide the dataset into training and test
source("DivideDataSet.R")

data(iris)
sampledIris <- DivideDataset(iris)
lifeExpectancy <- read.csv("life_expectancy.csv", header = TRUE)
sampledLifeExpectancy <- DivideDataset(lifeExpectancy)
class = sampledIris$trainingSet$Species~.
class1 = sampledLifeExpectancy$trainingSet$Continent~.

#C45 
fit <- J48(class, data = sampledIris$trainingSet)
directory <- "./plots/c45-Iris.png"
dir.create(dirname(directory), showWarnings = FALSE)
png(directory)
plot(fit)
dev.off()
summary(fit)
predictions<-predict(fit,sampledIris$testSet)
confusionMatrix(predictions,sampledIris$testSet$Species)
table(predictions, sampledIris$testSet$Species)

png("./plots/c45-iris-prediction.png")
plot(predictions)
dev.off()


fit1 <- J48(class1, sampledLifeExpectancy$trainingSet[,c(5,7,8)])
png("./plots/c45-lifeExpectancy.png")
plot(fit1)
dev.off()
summary(fit1)
predictions1<-predict(fit1,sampledLifeExpectancy$testSet)
confusionMatrix(predictions1,sampledLifeExpectancy$testSet$Continent)
table(predictions1, sampledLifeExpectancy$testSet$Continent)
png("./plots/c45-lifeExpectancy-prediction.png")
plot(predictions1)
dev.off()

#RIPPER
fit <- JRip(class, data = sampledIris$trainingSet)
summary(fit)
predictions<-predict(fit,sampledIris$testSet)
confusionMatrix(predictions,sampledIris$testSet$Species)
table(predictions, sampledIris$testSet$Species)
png("./plots/ripper-iris-prediction.png")
plot(predictions)
dev.off()

fit1 <- JRip(class1, sampledLifeExpectancy$trainingSet[,c(5,7,8)])
summary(fit1)
predictions1<-predict(fit1,sampledLifeExpectancy$testSet)
confusionMatrix(predictions1,sampledLifeExpectancy$testSet$Continent)
table(predictions1, sampledLifeExpectancy$testSet$Continent)
png("./plots/ripper-lifeExpectancy-prediction.png")
plot(predictions1)
dev.off()

#OBLIQUE
fit <- oblique.tree(Species ~ ., data = sampledIris$trainingSet, split.impurity = "gini", oblique.splits = "only")
png("./plots/Oblique-Iris.png")
plot(fit);text(fit)
dev.off()
summary(fit)
predictions<-predict(fit,sampledIris$testSet,type="class")
confusionMatrix(predictions,sampledIris$testSet$Species)
table(predictions, sampledIris$testSet$Species)
png("./plots/oblique-iris-prediction.png")
plot(predictions)
dev.off()


fit1 <- oblique.tree(Continent ~ ., data = sampledLifeExpectancy$trainingSet[,c(3,5,7,8)], split.impurity = "gini", oblique.splits = "only")
png("./plots/Oblique-LifeExpectancy.png")
plot(fit1);text(fit1)
dev.off()
summary(fit1)
predictions1<-predict(fit1,sampledLifeExpectancy$testSet,type="class")
confusionMatrix(predictions1,sampledLifeExpectancy$testSet$Continent)
table(predictions1, sampledLifeExpectancy$testSet$Continent)
png("./plots/oblique-lifeExpectancy-prediction.png")
plot(predictions1)
dev.off()

#KNN
fit <- knn(sampledIris$trainingSet[,1:4], sampledIris$testSet[,1:4], cl=sampledIris$trainingSet$Species, k = 7, prob=FALSE, use.all = TRUE)
ansknnIris <- table(fit, sampledIris$testSet$Species)
confusionMatrix(ansknnIris)
png("./plots/kNN-iris-prediction.png")
plot(ansknnIris)
dev.off()

fit1<-knn(sampledLifeExpectancy$trainingSet[,c(3,5,7)], sampledLifeExpectancy$testSet[,c(3,5,7)], cl=sampledLifeExpectancy$trainingSet$Continent, k = 7, prob=FALSE, use.all = TRUE)
ansknnLife <- table(fit1,sampledLifeExpectancy$testSet$Continent)
confusionMatrix(ansknnLife)
png("./plots/kNN-lifeExpectancy-prediction.png")
plot(ansknnLife)
dev.off()

#Naive Bayes
fit <- naiveBayes(sampledIris$trainingSet[,1:4],as.factor(sampledIris$trainingSet$Species)) 
summary(fit)
predictions<-predict(fit,sampledIris$testSet)
confusionMatrix(predictions,sampledIris$testSet$Species)
table(predictions, sampledIris$testSet$Species)
png("./plots/naiveBayes-iris-prediction.png")
plot(predictions)
dev.off()

fit1 <- naiveBayes(sampledLifeExpectancy$trainingSet[,c(3,5,7)],as.factor(sampledLifeExpectancy$trainingSet$Continent))
summary(fit1)
predictions1<-predict(fit1,sampledLifeExpectancy$testSet)
confusionMatrix(predictions1,sampledLifeExpectancy$testSet$Continent)
table(predictions1, sampledLifeExpectancy$testSet$Continent)
png("./plots/naiveBayes-lifeExpectancy-prediction.png")
plot(predictions1)
dev.off()
