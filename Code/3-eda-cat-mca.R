# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")
source("Code/1-load data.R")
source("Code/2-plot functions.R")

#Examine data
str(train, list.len = 999) 
str(test, list.len = 999) 

# Create lists of column names
cat.var <- names(train)[which(sapply(train, is.factor))]
num.var <- names(train)[which(sapply(train, is.numeric))]
num.var <- setdiff(num.var, c("id", "loss"))

train.cat <- train[,.SD,.SDcols = cat.var]
train.cat <- train[,1:116]
train.num <- train[, .SD, .SDcols = num.var]



mca1 = MCA(train.cat, graph = FALSE)
