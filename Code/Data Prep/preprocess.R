# Allstate Kaggle competition
library("Amelia")
library("dplyr")

#Load data
train <- read.csv("Data/Raw/train.csv",header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("Data/Raw/test.csv",header = TRUE, stringsAsFactors = FALSE)

#printing some missing data
sum(is.na(train[,]))
sum(is.na(test[,]))

summary(train)
summary(test)

#Create dummy variable in test
test<- mutate(test, loss = "none")

#Create sorting variable dataset before combining
test <- mutate(test, dataset = "testset")
train <- mutate(train, dataset = "trainset")

#Combine training and test datasets for feature engineering
allstate.combined <- rbind(test, train)
str(allstate.combined)

#Rename and create local data frame for simplicity
data<- tbl_df (allstate.combined)

#Examine data
str(train, list.len = 999) 
str(test, list.len = 999) 
str(data, list.len = 999) 


