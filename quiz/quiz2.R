# install.packages("AppliedPredictiveModeling")
# install.packages("caret")

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer)

        ?which
        training[which(training$Superplasticizer==0, arr.in=T),]

install.packages("e1071")

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

mySubset <- subset(training , select = names(training)[grepl('^IL_\\w+', names(training))] )
head(mySubset)    

preProcess(mySubset, method="pca", thresh=0.9)

df <- cbind(diagnosis=training$diagnosis, mySubset)

head(df)
# question 5
train(diagnosis ~ ., method = "glm" , data=df)

# Do PCA retaining 80% of the variance in the new predictors
my_pca <- preProcess(mySubset, method="pca", thresh=0.8)

# Make the new predictors using the original dataset
Z <- predict(my_pca, mySubset )

df2 <- cbind(diagnosis=training$diagnosis, Z)

train(diagnosis ~ ., preProcess='BoxCox', method = "glm" , data=df2)

















