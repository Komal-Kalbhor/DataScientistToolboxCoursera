# AppliedPredictiveModeling: v1.1.6
# 
# caret: v6.0.47
# 
# ElemStatLearn: v2012.04-0
# 
# pgmm: v1.1
# 
# rpart: v4.1.8
# 
# gbm: v2.1
# 
# lubridate: v1.3.3
# 
# forecast: v5.6
# 
# e1071: v1.6.4

### 1 
library(ElemStatLearn)

data(vowel.train)

data(vowel.test)
names(vowel.train)
names(vowel.test)
str(vowel.train)
str(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

library(caret)
rf <- train(y~.,data=vowel.train,method = "rf")
gbm <- train(y~., data=vowel.train, method="gbm")

rf.result <- predict(rf, vowel.test)
gbm.result <- predict(gbm, vowel.test)

confusionMatrix(vowel.test$y, rf.result)$overall['Accuracy']
confusionMatrix(vowel.test$y,gbm.result)$overall['Accuracy']

idx_agreed <- (rf.result == gbm.result)
confusionMatrix(vowel.test$y[idx_agreed], rf.result[idx_agreed])$overall['Accuracy']


##### 2
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
rf <- train(diagnosis~.,data=training,method = "rf")
gbm <- train(diagnosis~.,data=training,method="gbm")
lda <- train(diagnosis~.,data = training,method="lda")

rfresult <- predict(rf, testing)
gbmresult <- predict(gbm, testing)
ldaresult <- predict(lda, testing)
combined.data <- data.frame(rfresult, gbmresult, ldaresult, diagnosis=testing$diagnosis)
combined.model <- train(diagnosis~., data=combined.data, method="rf")
combined.result <- predict(combined.model, testing)

confusionMatrix(testing$diagnosis, rfresult)$overall['Accuracy']
confusionMatrix(testing$diagnosis, gbmresult)$overall['Accuracy']
confusionMatrix(testing$diagnosis, ldaresult)$overall['Accuracy']
confusionMatrix(testing$diagnosis, combined.result)$overall['Accuracy']


##### 3 
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
lasso.model <- train(CompressiveStrength~., data=training, method="lasso")
plot.enet(lasso.model$finalModel, xvar="penalty", use.color=TRUE)

##### 4
library(lubridate) # For year() function below

dat = read.csv("D:/BI/Coursera/Courses/10PracticalMachineLearning/gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

library(forecast)
model <- bats(tstrain)
h <- dim(testing)[1]

fc <- forecast(model,level = 95,h=h)
accuracy(fc,testing$visitsTumblr)

result <- c()
l <- length(fc$lower)

for (i in 1:l){
  x <- testing$visitsTumblr[i]
  a <- fc$lower[i] < x & x < fc$upper[i]
  result <- c(result, a)
}

sum(result)/l * 100


### 5
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]
set.seed(325)
library(e1071)
svm <- train(CompressiveStrength~., data=training, method="svmRadial")
prediction <- predict(svm, testing)

accuracy(prediction, testing$CompressiveStrength)
