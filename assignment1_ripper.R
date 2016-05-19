
library(datasets)
library(RWeka)
library(caret)
library(class) 
library(e1071)

data(iris)

## set the seed to make your partition reproducable
set.seed(6716)

## 80% of the sample size
smp_size <- floor(0.80 * nrow(iris))
train_idx <- sample(seq_len(nrow(iris)), size = smp_size)
train_set <- iris[train_idx, ]
test_set <- iris[-train_idx, ]

TrainData <- train_set[,1:4]
TrainClasses <- train_set[,5]

TestData <- test_set[,1:4]
TestClasses <- test_set[,5]

"Summary of Iris Dataset: "
summary(iris)
"Summary of Test Dataset:  "
summary(test_set)

"Classification using RIPPER algorithm: "
jripFit <- train(TrainData, TrainClasses,method = "JRip")
"Output of Training Dataset after classifying using RIPPER: "
summary(jripFit)
library(partykit)
plot(jripFit)

"Output of Test Dataset after classifying using RIPPER: " 
result <- (predict(jripFit, TestData))
summary(result)
table(result, TestClasses)

############################################################

mydata = read.csv(file = "life_expectancy.csv")
## set the seed to make your partition reproducable
set.seed(6716)

## 80% of the sample size
smp_size <- floor(0.80 * nrow(mydata))
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]
 
# smp_size <- floor(1 * nrow(test))
# testRandIdx = sample(seq_len(nrow(test)), size = smp_size)
# test <- test[testRandIdx, ]

summary(test)
#TrainData <- train[,1:7]
TrainData <- train[,c("Rank","Life.Expectancy","Male.Rank","Male.Expectancy","Female.Rank","Female.Life.Expectancy")]
TrainClasses <- train[,8]

TestData <- test[,c("Rank","Life.Expectancy","Male.Rank","Male.Expectancy","Female.Rank","Female.Life.Expectancy")]
TestClasses <- test[,8]

"Summary of Continent Dataset: "
summary(mydata)
"Summary of Test Dataset:  "
summary(test)
summary(train)
"Classification using RIPPER algorithm: "
jripFit <- train(TrainData, TrainClasses,method = "JRip")
"Output of Training Dataset after classifying using RIPPER: "
summary(jripFit)
summary(test)
"Output of Test Dataset after classifying using RIPPER: "
table(predict(jripFit,TestData), TestClasses)