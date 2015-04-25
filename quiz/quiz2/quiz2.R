
## Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

require(Hmisc)

head(training)
index <- seq(1,nrow(training))

# Plot CompressiveStrength vs index
qplot(index, CompressiveStrength, color = cut2(training$FlyAsh, g=4), data=training)


## Question 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

# Show Superplasticizer is skewed
qplot( log(training$Superplasticizer), color = cut2(log(training$Superplasticizer), g=4), data=training)
hist(training$Superplasticizer)
hist(log(training$Superplasticizer))


which(training$Superplasticizer > 0,  arr.ind = T)

unique(training$Superplasticizer )


## Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# find all columns begining with 'IL'
ILs <- names(training)[grepl("^IL_\\w+",names(training))]

mySubset <- subset(training, select=ILs )

# Variables needed to capture 90% variance
preProcess(mySubset, method="pca", thresh=0.9 )


## Question 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


# find all columns begining with 'IL'
ILs <- names(training)[grepl("^IL_\\w+",names(training))]

mySubset <- subset(training, select=ILs )

# Variables needed to capture 90% variance
pca <- preProcess(mySubset, method="pca", thresh=0.8 )



# Model 1 - As is
df1 <- cbind(mySubset, diagnosis=training$diagnosis)

fit1 <- train(diagnosis ~ . , method = "glm", data = df1)
print(fit1)
Accuracy = 0.669878

# Model 2 - PCA applied
?train
df2 <- cbind(mySubset, diagnosis=training$diagnosis)
fit2 <- train(diagnosis ~ . , method = "glm", preProcess = c("pca",thresh=0.8), data = df2)
print(fit2)

## Interestingly the model built with less information retained had a higher accuracy on the training set??












