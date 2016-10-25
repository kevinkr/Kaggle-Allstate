# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/1-load data.R")

train <- train %>% mutate_each(funs(factor), starts_with("cat"))
test <- test %>% mutate_each(funs(factor), starts_with("cat"))

# Harmonize factor levels
for (attr in colnames(train))
{
  
}

# 
# for(attr in colnames(training))
# {
#   if (is.factor(training[[attr]]))
#   {
#     new.levels <- setdiff(levels(training[[attr]]), levels(testing[[attr]]))
#     if ( length(new.levels) == 0 )
#     { print(paste(attr, '- no new levels')) }
#     else
#     {
#       print(c(paste(attr, length(new.levels), 'of new levels, e.g.'), head(new.levels, 2)))
#       levels(testing[[attr]]) <- union(levels(testing[[attr]]), levels(training[[attr]]))
#     }
#   }
# }

set.seed(3456)
trainIndex <- createDataPartition(train$loss, p = 0.8, list=FALSE, times=1)
subTrain <- train[trainIndex,]
subTest <- train[-trainIndex,]

# drop  id from trainSet
trainSet <- subTrain[-c(1)]
testSet <- subTest[-c(1)]

# fix missing category value in cat69
id <- which(!(train$cat89 %in% levels(test$cat89)))
#hacked
test$cat89[67038] <- "E"
test$cat89[88842] <- "E"

test$cat92[85977] <- "D"
test$cat92[90191] <- "D"
test$cat92[1770] <- "D"

#relevel
test <- test %>% mutate_each(funs(factor), starts_with("cat"))

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
solution <- data.frame(id = testSet$id, loss = round(testSet$loss,2))

