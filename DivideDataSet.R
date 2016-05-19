dat1 = read.csv(file = "imdb1.csv")
dat = read.csv(file = "MovieData_GenrePred.csv")

  
  summary(dat)

    dat <- na.omit(dat)
  sampleSize <- floor(0.8 * nrow(dat))
  trainIndex <- sample(seq_len(nrow(dat)), size = sampleSize)
  trainingSet <- dat[trainIndex, ]
  testSet <- dat[-trainIndex, ]
  summary(trainingSet)
  TestData <- testSet[,1:9]
  TrainData <- trainingSet[,1:9]
  TrainClasses <- trainingSet[,10]
  TestClasses <- testSet[,10]
  TestData[1:10,]
  
  #Genre Prediction : NaiveBayes
  jripFit <- train(TrainData, TrainClasses,method = "JRip")
  
  m <- naiveBayes(TrainData, TrainClasses)
  table(predict(m, TestData), TestClasses)
  
  
  #Association Apriori
  library(arules)
  dat[1:4,]
  fdat[1:4,]
  fdat <- merge(dat[, 2:7], as.data.frame(sapply(dat[,8:9], as.factor)), by="row.names")
  fdat <- fdat[2:9]
  summary(fdat)
  rules <- apriori(fdat)
  inspect(rules)
  
  #  westeros <- as.data.frame(sapply(westeros, gsub,pattern="House ",replacement=""))
  gc_rules <- apriori(westeros, parameter = list(supp = 0.01, conf = 0.9), appearance = list(rhs=c("Survives=TRUE", "Survives=FALSE"), default = "lhs"))
  inspect(gc_rules)
  
  #Clustering kmeans
  
  plot(dat)
  classes <- dat[,10]
  filter_data <- dat[,1:9]
  
  kclus <- kmeans(filter_data, centers=5)
  table(data.frame(origclasses, kclus$cluster))
  
  
  
