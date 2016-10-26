# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/1-load data.R")

#train <- train %>% mutate_each(funs(factor), starts_with("cat"))
#test <- test %>% mutate_each(funs(factor), starts_with("cat"))
train$loss <- log(train$loss)
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
               cv.folds=3, 
               n.trees=5000,
               shrinkage = 0.0005,
               interaction.depth = 12,
               bag.fraction = 0.5,
               train.fraction = 0.8,
               n.minobsinnode = 20
               )

# print out optimal iteration number
best.iter <- TDboost.perf(fit, method="test", overlay = TRUE, plot.it = TRUE)
print(best.iter)


# check performance using 5-fold cross-validation
best.iter <- TDboost.perf(fit,method="cv")
print(best.iter)



# plot the performance and variable influence
summary(fit,n.trees=1)         # based on the first tree

summary(fit,n.trees=best.iter) # based on the estimated best number of trees

# model prediction / scoring
f.predict <- predict.TDboost(fit, testSet, best.iter)

# least squares error
print(sum((testSet$loss - f.predict)^2))

# create marginal plots
# plot variable X1 after "best" iterations
#plot.TDboost(fit,c(1:3),best.iter)



# get predictions
test$loss <- predict.TDboost(fit, test, best.iter)
solution <- data.frame(id = test$id, loss = round(exp(test$loss),2))

# Write the solution to file
write.csv(solution, file = 'Submissions/tdboost-learning-102716-v2.csv', row.names = F)
