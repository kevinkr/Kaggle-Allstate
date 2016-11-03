# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")

## load the library

par(mfrow=c(1,1))
# follow load data, cat reduction pt1, pt2

################
# test harness design
####################

#refactor levels
test$loss <- NA
test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))
#bind train and test
fullSet <- rbind(test,train)
# set factor levels all full set
fullSet <- fullSet %>% mutate_each(funs(factor), starts_with("cat"))
# split back into test and train
test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
# drop loss from test set
test <- subset(test, select = -c(loss))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))

##########################################
# Create split train set
set.seed(212312)
trainIndex <- createDataPartition(train$loss, p = 0.8, list=FALSE, times=1)
subTrain <- train[trainIndex,]
subTest <- train[-trainIndex,]

# drop  id from trainSet
trainSet <- subTrain[-c(1)]
testSet <- subTest[-c(1)]

rm(trainIndex, subTrain, subTest, fullSet)

## Fit decision model to training set
trainSet.rf.model <- randomForest(log(loss) ~ ., 
                                  data=trainSet, 
                                  importance=TRUE, 
                                  ntree=201,
                                  nodesize = 1000,
                                  mtry = 43,
                                  proximity = FALSE
                                  )
print(trainSet.rf.model)

# Variable Importance Table
var.imp <- data.frame(importance(trainSet.rf.model, type=1))
var.imp$Variables <- row.names(var.imp)
# make row names as columns
var.imp[order(var.imp$Variables, decreasing = T),]
var.imp[order(var.imp, decreasing = T),]


# Variable Importance Table
var.imp <- data.frame(importance(trainSet.rf.model, type=2))
var.imp$Variables <- row.names(var.imp)
# make row names as columns
var.imp[order(var.imp$Variables, decreasing = T),]
var.imp[order(var.imp, decreasing = T),]


varImpPlot(trainSet.rf.model)

plot(trainSet.rf.model)

mean(trainSet.rf.model == testSet$loss)


test$loss <- predict(trainSet.rf.model, test)
solution <- data.frame(id = test$id, loss = exp(test$loss))

# Write the solution to file
write.csv(solution, file = 'Submissions/rf-v3-110316.csv', row.names = F)
