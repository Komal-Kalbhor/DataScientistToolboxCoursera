library(AppliedPredictiveModeling)
data(AlzheimerDisease)

library(caret)
adData = data.frame(diagnosis,predictors)
testIndex1 = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training1 = adData[-testIndex1,]
testing1 = adData[testIndex1,]


# adData = data.frame(diagnosis,predictors)
# trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
# training = adData[trainIndex,]
# testing = adData[-trainIndex,]

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

suppressMessages(library(dplyr))
suppressMessages(library(Hmisc))
suppressMessages(library(gridExtra))
training <- mutate(training, index=1:nrow(training))
cutIndex <- cut2(training$index, g=10)
breaks <- 10
qplot(index, CompressiveStrength, data=training, color=cut2(training$Cement, g=breaks))

#byCement <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Cement, g=breaks))
#byBlastFurnaceSlag <- qplot(index, CompressiveStrength, data=training, color=cut2(training$BlastFurnaceSlag, g=breaks))
#byFlyAsh <- qplot(index, CompressiveStrength, data=training, color=cut2(training$FlyAsh, g=breaks))
#byWater <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Water, g=breaks))
#bySuperplasticizer <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Superplasticizer, g=breaks))
#byCoarseAggregate <- qplot(index, CompressiveStrength, data=training, color=cut2(training$CoarseAggregate, g=breaks))
#byFineAggregate <- qplot(index, CompressiveStrength, data=training, color=cut2(training$FineAggregate, g=breaks))
#byAge <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Age, g=breaks))
#grid.arrange(byCement, byBlastFurnaceSlag, byFlyAsh, byWater, bySuperplasticizer, byCoarseAggregate, byFineAggregate, byAge)


library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer, breaks=20)
hist(log(training$Superplasticizer), breaks=20)


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_col_idx <- grep("^[Ii][Ll].*", names(training))
preObj <- preProcess(training[, IL_col_idx], method=c("center", "scale", "pca"), thresh=0.9)
preObj


IL_col_idx <- grep("^[Ii][Ll].*", names(training))
suppressMessages(library(dplyr))
new_training <- training[, c(names(training)[IL_col_idx], "diagnosis")]
names(new_training)


IL_col_idx <- grep("^[Ii][Ll].*", names(testing))
suppressMessages(library(dplyr))
new_testing <- testing[, c(names(testing)[IL_col_idx], "diagnosis")]
names(new_testing)

library(e1071)
# compute the model with non_pca predictors
non_pca_model <- train(diagnosis ~ ., data=new_training, method="glm")
# apply the non pca model on the testing set and check the accuracy
non_pca_result <- confusionMatrix(new_testing[, 13], predict(non_pca_model, new_testing[, -13]))
non_pca_result


# perform PCA extraction on the new training and testing sets
pc_training_obj <- preProcess(new_training[, -13], method=c('center', 'scale', 'pca'), thresh=0.8)
pc_training_preds <- predict(pc_training_obj, new_training[, -13])
pc_testing_preds <- predict(pc_training_obj, new_testing[, -13])
# compute the model with pca predictors
pca_model <- train(new_training$diagnosis ~ ., data=pc_training_preds, method="glm")
# apply the PCA model on the testing set
pca_result <- confusionMatrix(new_testing[, 13], predict(pca_model, pc_testing_preds))
pca_result