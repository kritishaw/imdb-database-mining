library(datasets)
library(RWeka)
library(caret)
library(class) 
library(e1071)
library(plyr)
library(oblique.tree)
library(partykit)

data(iris)

## set the seed to make your partition reproducable
set.seed(6716)

## 80% of the sample size
smp_size <- floor(0.80 * nrow(iris))
train_idx <- sample(seq_len(nrow(iris)), size = smp_size)
train_set <- iris[train_idx, ]
test_set <- iris[-train_idx, ]

smp_size <- floor(1 * nrow(test_set))
testSetRand <- sample(seq_len(nrow(test_set)), size = smp_size)
test_set <- test_set[testSetRand, ]

TrainData <- train_set[,1:4]
TrainClasses <- train_set[,5]

TestData <- test_set[,1:4]
TestClasses <- test_set[,5]

"Summary of Iris Dataset: "
summary(iris)
"Summary of Train Dataset:  "
summary(train_set)
"Summary of Test Dataset:  "
summary(test_set)

"Classification using RIPPER algorithm: "
jripFit <- train(TrainData, TrainClasses,method = "JRip")
"Output of Training Dataset after classifying using RIPPER: "
summary(jripFit)
plot(jripFit)

"Output of Test Dataset after classifying using RIPPER: " 
result <- (predict(jripFit, TestData))
summary(result)
table(result, TestClasses)

"Naive Bayes Classifier:"
bayesFit <- naiveBayes(TrainData, TrainClasses)
table(predict(bayesFit, TestData), TestClasses)
plot(predict(bayesFit, TestData))

"K Nearest Neighbours Classifier:"
set.seed(6716)
iris_pred <- knn(train = TrainData, test = TestData, cl = TrainClasses, k=11)
summary(iris_pred)
table(iris_pred, TestClasses)
plot.default(iris_pred)

"C4.5 Classifier: "
cforty <- J48(Species ~ ., data = train_set)
table(predict(cforty, TestData), TestClasses)
plot(cforty)

"Oblique Tree Classifier: "
set.seed(6716)
ob.tree <- oblique.tree(formula = Species~.,
                        data = train_set,
                        oblique.splits = "only")
plot(ob.tree);text(ob.tree);title(main="Oblique Tree")

#Oblique tree method requires the test-data matrix to have a class column, so create a dummy column
TestData1 <- TestData
TestData1$Species <- ""
ob_pred <- predict(ob.tree, TestData1, type = c("class"), update.tree.predictions = FALSE)
table(ob_pred,TestClasses)

############################################################

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

TrainData <- train[,c("Rank","Life.Expectancy","Male.Rank","Male.Expectancy","Female.Rank","Female.Life.Expectancy")]
TrainClasses <- train[,8]

TestData <- test[,c("Rank","Life.Expectancy","Male.Rank","Male.Expectancy","Female.Rank","Female.Life.Expectancy")]
TestClasses <- test[,8]

"Summary of Continent Dataset: "
summary(mydata)
"Summary of Train Dataset:  "
summary(train)
"Summary of Test Dataset:  "
summary(test)

"Classification using RIPPER algorithm: "
jripFit <- train(TrainData, TrainClasses,method = "JRip")
"Output of Training Dataset after classifying using RIPPER: "
summary(jripFit)

"Output of Test Dataset after classifying using RIPPER: "
table(predict(jripFit,TestData), TestClasses)
plot(jripFit)

"Classification using Naives algorithm: "
bayesFit <- naiveBayes(TrainData, TrainClasses)
table(predict(bayesFit, TestData), TestClasses)
plot(predict(bayesFit, TestData))

"K Nearest Neighbours Classifier: "
set.seed(6716)
mydata_pred <- knn(train = TrainData, test = TestData, cl = TrainClasses, k=14)
table(mydata_pred, TestClasses)
mydata_pred
plot.default(mydata_pred)

"C4.5 Classifier: "
ctrn <- train[,c("Rank","Life.Expectancy","Male.Rank","Male.Expectancy","Female.Rank","Female.Life.Expectancy","Continent")]
cforty <- J48(Continent ~., data = ctrn)
table(predict(cforty, TestData), TestClasses)
plot(cforty)

"Oblique Tree Classifier: "
set.seed(6716)
ctrn <- train[,c("Rank","Life.Expectancy","Male.Rank","Male.Expectancy","Female.Rank","Female.Life.Expectancy","Continent")]
ob.tree <- oblique.tree(formula = Continent~.,
                        data = ctrn,
                        oblique.splits = "only")
plot(ob.tree);text(ob.tree);title(main="Oblique Tree")
#Oblique tree method requires the test-data matrix to have a class column, so create a dummy column
TestData1 <- TestData
TestData1$Continent <- ""
ob_pred <- predict(ob.tree, TestData1, type = c("class"), update.tree.predictions = FALSE)
table(ob_pred,TestClasses)


detach("package:RWeka", unload = TRUE)
detach("package:caret", unload = TRUE)
detach("package:e1071", unload = TRUE)
detach("package:oblique.tree", unload = TRUE)
detach("package:partykit", unload = TRUE)