# Allstate Kaggle competition
# Start: 10-13-16

#load packages
options(scipen=999) # remove scientific notation

source("Code/packages.R")

#Load data
train <- fread("Data/Raw/train.csv", stringsAsFactors=FALSE, header = TRUE)
test <- fread("Data/Raw/test.csv", stringsAsFactors=FALSE, header = TRUE)

# Harmonize factors

#set test loss to NA
refactor_levels <- function() {
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
}