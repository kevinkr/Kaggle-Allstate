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
cat.var <- names(train)[which(sapply(train, is.character))]
num.var <- names(train)[which(sapply(train, is.numeric))]
num.var <- setdiff(num.var, c("id", "loss"))

train.cat <- train[,.SD,.SDcols = cat.var]
train.num <- train[, .SD, .SDcols = num.var]


#######################
# Plot categories vs. loss
#######################

doPlots(train.cat, fun = plotBox, ii =1:12, lab=log(train$loss), ncol = 3)
doPlots(train.cat, fun = plotBox, ii =13:24, lab=log(train$loss), ncol = 3)
doPlots(train.cat, fun = plotBox, ii =25:36, lab=log(train$loss), ncol = 3)
doPlots(train.cat, fun = plotBox, ii =37:48, lab=log(train$loss), ncol = 3)
doPlots(train.cat, fun = plotBox, ii =49:60, lab=log(train$loss), ncol = 3)
doPlots(train.cat, fun = plotBox, ii =61:72, lab=log(train$loss), ncol = 3)
doPlots(train.cat, fun = plotBox, ii =73:84, lab=log(train$loss), ncol = 3)
doPlots(train.cat, fun = plotBox, ii =85:96, lab=log(train$loss), ncol = 3)
doPlots(train.cat, fun = plotBox, ii =97:108, lab=log(train$loss), ncol = 3)
doPlots(train.cat, fun = plotBox, ii =109:116, lab=log(train$loss), ncol = 3)


###########
# Categories
##############
# number of categories per variable
cats = apply(train.cat, 2, function(x) nlevels(as.factor(x)))
cats

#distribution of variables per cateogry


cat.frame <- apply(train.cat,2,table)

# Count of labels per category
library(plyr)
library(tidyr)
cat.frame <- apply(train.cat, 2, count) 


######
# Reduction in categories by representation
########
#
# Example cat116
#
table(train$cat116)

sort(table(train$cat116), decreasing = T)

# Bucket lowly represented values into single category
# What is the cutoff? This will become a tuning algorithm
# Phuc likes to have 1-5% of my data represented by a category

# view as proportion
options(scipen=999) # remove scientific notation
prop.table <- sort(prop.table(table(train$cat116)), decreasing = T)
# identify those category names with weak categories
weak.prop.table <- prop.table < 0.01
# grab the names
weak.prop.names <- names(prop.table[prop.table < 0.01])

# filter data set by categories that are in the weak prop names vector using %in% search'
# first convert to character
train$cat116 <- as.character(train$cat116)
train[train$cat116 %in% weak.prop.names, "cat116"] <- "OTHER"

# review as prop.table
prop.table(table(train$cat116))



######
# category reduction fucntion
######
# inputs category name, cutoff value
#
reduce_cats <- function(cat.name, cutoff.val) {
  prop.table <- sort(prop.table(table(train[,cat.name])), decreasing = T)
  #return(prop.table)
  weak.prop.table <- prop.table < cutoff.val
  #return(weak.prop.table)
  # grab the names
  weak.prop.names <- names(prop.table[prop.table < 0.01])
  return(weak.prop.names)
  
}

############continue here########
for (n in cat.var) {
  
  print(n)
}

# set up category name
cat.name <- as.character("cat116")
# call function to return category names for reduction, number is cutoff val
weak.prop.names <- reduce_cats(cat.name, 0.01)

# filter data set by categories that are in the weak prop names vector using %in% search'
# first convert to character
train[,cat.name] <- as.character(train[,cat.name])
#train[train$cat116 %in% weak.prop.names, "cat116"] <- "OTHER"
train[train[,cat.name] %in% weak.prop.names, cat.name] <- "OTHER"
# review as prop.tables
prop.table(table(train[,cat.name]))
train[,cat.name] <- as.factor(train[,cat.name])
str(train$cat116)
