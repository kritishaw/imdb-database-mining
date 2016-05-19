library(datasets)
library(RWeka)
library(caret)

library(class) 
library(e1071)

data(iris)

## set the seed to make your partition reproductible
set.seed(6716)

## training set = 80% of the dataset
train_size <- floor(0.80 * nrow(iris))

train_idx <- sample(seq_len(nrow(iris)), size = train_size)

train_set <- iris[train_idx, ]
test_set <- iris[-train_idx, ]

TrainData <- train_set[,1:4]
TrainClasses <- train_set[,5]
TestData <- test_set[,1:4]
#TestClasses <- test[,5]
jripFit <- train(TrainData, TrainClasses,method = "JRip")
#jripFittest <- train(TestData, TestClasses,method = "JRip")
summary(iris)
summary(jripFit)
summary(test_set)

confusionMatrix(sort(iris[,5]))
result <- (predict(jripFit, (TestData[1:30,1:4])))
result
summary(result)

#m <- naiveBayes(TrainData, TrainClasses)

#table(predict(m, TestData), TestClasses)

#iris_pred <- knn(train = TrainData, test = TestData, cl = TrainClasses, k=3)
#summary(iris_pred)
