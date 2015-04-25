## Quiz 3

## Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

inTrain = createDataPartition(segmentationOriginal$Case, p = 3/4)[[1]]
training = segmentationOriginal[ inTrain,]
testing = segmentationOriginal[-inTrain,]

set.seed(125)

mod1 <- train(Class~., method='rpart', data=training)


pp  <- data.frame(TotalIntench2 = 23000 , FiberWidthCh1 = 10, PerimStatusCh1 = 2)
pp[2,] <- cbind(50000,10,100)
pp[3,] <- cbind(57000,8,100)
pp[4,] <- cbind(50000,10,100)

mod1$finalModel

# install.packages("rattle")
install.packages("rpart.plot")
library(rattle)
fancyRpartPlot(mod1$finalModel)

42*10^3


## Q3
require(caret)
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]

inTrain = createDataPartition(olive$Area, p = 3/4)[[1]]
training = olive[ inTrain,]
testing = olive[-inTrain,]

mod2 <- train(Area~., method='rpart', data=training)

newdata = as.data.frame(t(colMeans(olive)))

predict(mod2, newdata)
2.777

## Q4
install.packages('ElemStatLearn')
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

?SAheart
require(caret)
modfit1 <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,   method="glm", family="binomial", data=trainSA )

predict_train <- predict(modfit1, newdata=trainSA)
miss_class_err_train <- missClass(trainSA$chd, predict_train)

predict_test <- predict(modfit1, newdata=testSA)
miss_class_err_test <- missClass(testSA$chd, predict_test)


## Q5
require(caret)
library(ElemStatLearn)
data(vowel.train); head(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

head(vowel.train)

modfit2 <- train(y~., method='rf', data=vowel.train)
print(modfit2$finalModel)

install.packages('pROC')
varImp(modfit2, useModel=TRUE)








