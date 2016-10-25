# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/1-load data.R")

#train <- train %>% mutate_each(funs(factor), starts_with("cat"))
#test <- test %>% mutate_each(funs(factor), starts_with("cat"))

set.seed(3456)
trainIndex <- createDataPartition(train$loss, p = 0.8, list=FALSE, times=1)
subTrain <- train[trainIndex,]
subTest <- train[-trainIndex,]

# drop  id from trainSet
trainSet <- subTrain[-c(1)]
testSet <- subTest[-c(1)]

#fit <- TDboost(loss ~. , data=trainSet, cv.folds=5, n.trees=300, interaction.depth = 20)
fit <- TDboost(loss ~. , 
               data=trainSet, 
               cv.folds=5, 
               n.trees=100, 
               interaction.depth = 20, 
               distribution = list(name="EDM",alpha=1.5))

# print out optimal iteration number
best.iter <- TDboost.perf(fit, method="test")


# check performance using 5-fold cross-validation
best.iter <- TDboost.perf(fit,method="cv")

# plot the performance and variable influence
summary(fit,n.trees=1)         # based on the first tree

summary(fit,n.trees=best.iter) # based on the estimated best number of trees

# model prediction / scoring
f.predict <- predict.TDboost(fit, testSet, best.iter)

# least squares error
print(sum((testSet$y - f.predict)^2))

# get predictions
test$loss <- predict.TDboost(fit, test, best.iter)
solution <- data.frame(id = test$id, loss = round(test$loss,2))

# Write the solution to file
write.csv(solution, file = 'Submissions/tdboost-learning-102616-v1.csv', row.names = F)
