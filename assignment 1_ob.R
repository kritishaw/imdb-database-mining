library(datasets)
library(RWeka)
library(caret)
library(class) 
library(e1071)
library(oblique.tree)

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

"Oblique Tree Classifier: "
set.seed(6716)
ob.tree <- oblique.tree(formula = Species~.,
                        data = train_set,
                        oblique.splits = "only")
plot(ob.tree);text(ob.tree);title(main="Oblique Tree")

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

"Oblique Tree Classifier: "
set.seed(6716)
ctrn <- train[,c("Rank","Life.Expectancy","Male.Rank","Male.Expectancy","Female.Rank","Female.Life.Expectancy","Continent")]
ob.tree <- oblique.tree(formula = Continent~.,
                        data = ctrn,
                        oblique.splits = "only")
plot(ob.tree);text(ob.tree);title(main="Oblique Tree")
# predict(ob.tree, test, type = c("class"), update.tree.predictions = FALSE)
TestData1 <- TestData
TestData1$Continent <- ""
ob_pred <- predict(ob.tree, TestData1, type = c("class"), update.tree.predictions = FALSE)
table(ob_pred,TestClasses)
#table(predict(ob.tree, test, type = c("class"), update.tree.predictions = FALSE),TestClasses)

