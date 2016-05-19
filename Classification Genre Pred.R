library(rJava)
library(RWeka)
library(datasets)
library(partykit)
library(class) 
library(e1071)
library(caret)
library(klaR)

dat = read.csv(file = "Classification.csv")

cdat <- dat
cdat <- cdat[!(is.na(cdat$Genre.1) | cdat$Genre.1==""), ]

set.seed(61904)

sampleSize <- floor(0.8 * nrow(cdat))
trainIndex <- sample(seq_len(nrow(cdat)), size = sampleSize)
trainingSet <- cdat[trainIndex, ]
testSet <- cdat[-trainIndex, ]

TestData1 <- testSet[,c("Keyword.1","Keyword.2","Keyword.3","Keyword.4")]
TrainData1 <- trainingSet[,c("Keyword.1","Keyword.2","Keyword.3","Keyword.4")]

TestData2 <- testSet[,c("Actor.1","Actor.2","Actor.3")]
TrainData2 <- trainingSet[,c("Actor.1","Actor.2","Actor.3")]

TestData3 <- testSet[,c("Director","Producer","Writer","Actor.1","Actor.2","Actor.3")]
TrainData3 <- trainingSet[,c("Director","Producer","Writer","Actor.1","Actor.2","Actor.3")]

TestData4 <- testSet[,c("Director","Producer","Writer","Actor.1","Actor.2","Actor.3","Keyword.1","Keyword.2","Keyword.3","Keyword.4")]
TrainData4 <- trainingSet[,c("Director","Producer","Writer","Actor.1","Actor.2","Actor.3","Keyword.1","Keyword.2","Keyword.3","Keyword.4")]

TrainClasses <- trainingSet[,12]
TestClasses <- testSet[,12]
summary(TestData)

#Genre Prediction : NaiveBayes
m1 <- naiveBayes(TrainData1, TrainClasses)
confusionMatrix(predict(m1, TestData1), TestClasses)

m2 <- naiveBayes(TrainData2, TrainClasses)
confusionMatrix(predict(m2, TestData2), TestClasses)

m3 <- naiveBayes(TrainData3, TrainClasses)
confusionMatrix(predict(m3, TestData3), TestClasses)

m4 <- naiveBayes(TrainData4, TrainClasses)
confusionMatrix(predict(m4, TestData4), TestClasses)

#Genre Prediction : Cforty
cforty1 <- J48(Genre.1 ~ ., data = trainingSet[,c("Keyword.1","Keyword.2","Keyword.3","Keyword.4","Genre.1")])
confusionMatrix(predict(cforty1, TestData1), TestClasses)

cforty2 <- J48(Genre.1 ~ ., data = trainingSet[,c("Actor.1","Actor.2","Actor.3","Genre.1")])
confusionMatrix(predict(cforty2, TestData2), TestClasses)

cforty3 <- J48(Genre.1 ~ ., data = trainingSet[,c("Director","Producer","Writer","Actor.1","Actor.2","Actor.3","Genre.1")])
confusionMatrix(predict(cforty3, TestData3), TestClasses)

cforty4 <- J48(Genre.1 ~ ., data = trainingSet[,c("Director","Producer","Writer","Actor.1","Actor.2","Actor.3","Keyword.1","Keyword.2","Keyword.3","Keyword.4", "Genre.1")])
confusionMatrix(predict(cforty4, TestData4), TestClasses)

# Unsupervised Genre Prediction using a derived dataset with missing Genre values 
genreMissdat <- read.csv(file = "genreMissing.csv")
# 1. using the naive bayes model trained from data subset : "Director","Producer","Writer",
# "Actor.1","Actor.2","Actor.3","Keyword.1","Keyword.2","Keyword.3","Keyword.4", "Genre.1"
naivePredict <- predict(m4, genreMissdat)
plot(naivePredict)

# 2. using the cforty model trained from data subset : "Actor.1","Actor.2","Actor.3","Genre.1"
cfortyPredict <- predict(cforty2, genreMissdat)
plot(cfortyPredict)

cor(as.numeric(naivePredict), as.numeric(cfortyPredict), method = "kendall")

# genre prediction not aligned with association rules
part2 <- read.csv(file = "ClassVsAssociation.csv")
part2classes <- part2[,8]
set.seed(61904)

sampleSize <- floor(nrow(part2))
Index <- sample(seq_len(nrow(part2)), size = sampleSize)
part2data <- part2[Index,]
part2classes <- part2[Index, 8]

predict(m3, part2data[,2:7])
predict(cforty3, part2data[,2:7])

detach("package:rJava", unload = TRUE)
detach("package:RWeka", unload = TRUE)
detach("datasets", unload = TRUE)
detach("partykit", unload = TRUE)
detach("class", unload = TRUE) 
detach("e1071", unload = TRUE)
detach("caret", unload = TRUE)
detach("klaR", unload = TRUE)
