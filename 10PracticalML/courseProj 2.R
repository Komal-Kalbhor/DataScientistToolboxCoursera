library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(e1071)
library(data.table)
#install.packages("readr")
library(readr)
setwd("D:/BI/Coursera/Courses/10PracticalMachineLearning/")
training <- read.csv("D:/BI/Coursera/Courses/10PracticalMachineLearning/pml-training.csv")
testing <- read.csv("D:/BI/Coursera/Courses/10PracticalMachineLearning/pml-testing.csv")

head(training$classe)
prop.table(table(training$classe))*100
str(training)
for(i in 1:dim(training)[2])
{
  print(colnames(training)[i])
  print(sum(is.na(training[,i])))
  
}
training_compCases<- complete.cases(training)
table(training_compCases)

complete_training  <- training[training_compCases,]

# Set seed
set.seed(227)

# # Remove variables having high missing percentage (50%)
 training <- training[, colMeans(is.na(training)) <= .5]
 dim(training)

# Remove Zero and Near Zero-Variance Predictors
nzv.result <- nearZeroVar(training)
training2 <- training[, -nzv.result]
dim(training2)

# Identifying numeric variables
numericData <- training2[sapply(training2, is.numeric)]

# Calculate correlation matrix
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)


# Check Correlation Plot
library(corrplot)
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# Indentifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]

# Print highly correlated attributes
highlyCorCol

# Remove highly correlated variables and create a new dataset
training3 <- training2[, -which(colnames(training2) %in% highlyCorCol)]
dim(training3)

training4 <- training3[,c(-1,-36)]
names(training4)
testing4 <- testing[,colnames(training4)]
names(testing4)
names(training3)

training3 <- training3[,-1]

## CROSS VALIDATION - RANDOM SAMPLING W/O REPLACEMENT
# split training set into sub-training/test sets
inTrain <- createDataPartition(y=training3$classe,p=0.60, list=FALSE)
sub_training <- training3[inTrain,]
sub_testing <- training3[-inTrain,]


#SVM
svm.result <- train(classe~., data = sub_training,method = "svmLinear")
summary(svm.result)
predictions <- predict(svm.result, sub_testing)
# compare results
confusionMatrix(sub_testing$classe,predictions)
predictions2 <- predict(svm.result,testing4)
#A B C A A E D A A A B C B A E E A B A B

#CART
rf.result<-train(classe~., data = sub_training, method = "rpart")
summary(rf.result)
predictions <- predict(rf.result, sub_testing)
confusionMatrix(sub_testing$classe,predictions)
predictions2 <- predict(rf.result,testing4)
#C C C A A D C D A A D C B A E E E D C B

#GBM
set.seed(12345)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1)

gbmFit1 <- train(classe ~ ., data=sub_training, method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)


gbmFinMod1 <- gbmFit1$finalModel

gbmPredTest <- predict(gbmFit1, newdata=sub_testing)
gbmAccuracyTest <- confusionMatrix(sub_testing$classe,gbmPredTest)
gbmAccuracyTest
predictions2 <- predict(gbmFit1,testing4)
predictions2

# results <- resamples(list(SVM=svm.result, GBM=gbmFit1, CART=rf.result))
# # summarize the distributions
# summary(results)
# # boxplots of results
# bwplot(results)
# # dot plots of results
# dotplot(results)
