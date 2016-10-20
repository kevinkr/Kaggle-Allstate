# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")

#Load data
train <- fread("Data/Raw/train.csv", stringsAsFactors=FALSE, header = TRUE)
test <- fread("Data/Raw/test.csv", stringsAsFactors=FALSE, header = TRUE)


