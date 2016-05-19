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
train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
train <- iris[train_ind, ]
test <- iris[-train_ind, ]

TrainData <- train[,1:4]
TrainClasses <- train[,5]

TestData <- test[,1:4]
TestClasses <- test[,5]

"Summary of Iris Dataset: "
summary(iris)
"Summary of Test Dataset:  "
summary(test)

"C4.5 Classifier: "
cforty <- J48(Species ~ ., data = train)
table(predict(cforty, TestData), TestClasses)

plot(cforty)
#################################################################################
mydata = read.csv(file = "life_expectancy.csv")
## set the seed to make your partition reproducable
set.seed(6716)

## 80% of the sample size
smp_size <- floor(0.80 * nrow(mydata))
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

smp_size <- floor(1 * nrow(test))
testRandIdx = sample(seq_len(nrow(test)), size = smp_size)
test <- test[testRandIdx, ]

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

"C4.5 Classifier: "
ctrn <- train[,c("Rank","Life.Expectancy","Male.Rank","Male.Expectancy","Female.Rank","Female.Life.Expectancy","Continent")]
cforty <- J48(Continent ~., data = ctrn)
table(predict(cforty, TestData), TestClasses)

plot(cforty)