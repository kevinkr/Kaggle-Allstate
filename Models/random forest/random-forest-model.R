# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")

## load the library
library(randomForest)
par(mfrow=c(1,1))
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

# drop  id from trainSet
trainSet <- subTrain[-c(1)]
testSet <- subTest[-c(1)]

rm(trainIndex, subTrain, subTest, fullSet)

## Fit decision model to training set
trainSet.rf.model <- randomForest(loss ~ ., 
                                  data=trainSet, 
                                  importance=TRUE, 
                                  ntree=200,
                                  nodesize = 1950,
                                  mtry = 1,
                                  proximity=F
                                  )
print(trainSet.rf.model)

varImpPlot(trainSet.rf.model)
plot(trainSet.rf.model)
