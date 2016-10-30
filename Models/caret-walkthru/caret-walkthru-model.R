# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")
# follow load data, cat reduction pt1, pt2

################
# test harness design
####################


##########################################
# Create split train set
set.seed(212312)
trainIndex <- createDataPartition(train$loss, p = 0.8, list=FALSE, times=1)
subTrain <- train[trainIndex,]
subTest <- train[-trainIndex,]

rm(trainIndex, subTrain, subTest, fullSet)

# drop  id from trainSet
trainSet <- subTrain[-c(1)]
testSet <- subTest[-c(1)]


# Test Options
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 27


metric <- "RMSE"

#preProcess=c("center", "scale")

# Logistic Regression
set.seed(seed)
fit.glm <- train(loss~., data=trainSet, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed)
fit.glmnet <- train(loss~., data=trainSet, method="glmnet", metric=metric, trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(Survived~., data=trainSet, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(Survived~., data=trainSet, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(Survived~., data=trainSet, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(Survived~., data=trainSet, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(seed)
fit.c50 <- train(Survived~., data=trainSet, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(Survived~., data=trainSet, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(loss~., data=trainSet, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(Survived~., data=trainSet, method="gbm", metric=metric, trControl=control, verbose=FALSE)


############
# Model selection
############

results <- resamples(list(logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

###########################
# Choosing glmnet
#######
MyTrainControl=trainControl(
  method = "cv",
  number=10,
  repeats=5
)

model <- train(Survived~.,data=trainSet,method='glmnet',
               tuneGrid = expand.grid(.alpha=(1:10) * 0.05,.lambda = (1:10) * 0.05),
               trControl=MyTrainControl,
               metric = "Accuracy")
model
plot(model, metric='Accuracy')

importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


testSet$Survived <- predict(model, newdata = testSet)

submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "ds-dojo-10-6-2016-v5-glmnet.csv", col.names = TRUE, row.names = FALSE, sep = ",")
