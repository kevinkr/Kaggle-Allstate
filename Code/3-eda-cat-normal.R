# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")
source("Code/1-load data.R")
source("Code/2-plot functions.R")


#printing some missing data
sum(is.na(train[,]))
sum(is.na(test[,]))

summary(train)
summary(test)

#Combine training and test datasets for feature engineering
allstate.combined <- rbind(test, train)
str(allstate.combined)

#Rename and create local data frame for simplicity
data<- tbl_df (allstate.combined)

#Examine data
str(train, list.len = 999) 
str(test, list.len = 999) 
str(data, list.len = 999) 

# Create lists of column names
cat.var <- names(train)[which(sapply(train, is.character))]
num.var <- names(train)[which(sapply(train, is.numeric))]
num.var <- setdiff(num.var, c("id", "loss"))


train.cat <- train[,.SD,.SDcols = cat.var]
train.num <- train[, .SD, .SDcols = num.var]


#######################
# Plot categories
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


# Examine counts of factors in categories
train.cat.factored <- train.cat %>% mutate_each(funs(factor), starts_with("cat"))


###########
# Categories
##############
# number of categories per variable
cats = apply(train.cat, 2, function(x) nlevels(as.factor(x)))
cats


# Piechart of categorical
pie(table(train.cat$cat100))

barplot(table(train.cat$cat101))



#distribution of variables per cateogry
lapply(train.cat, table)

#display counts of category
ggp <- ggplot(train.cat,aes(x=cat109))
ggp + geom_bar()

# Count of labels per category
library(plyr)
apply(train.cat, 2, count)

#Examine proportions
mosaicplot(cat1 ~cat4, data = train.cat, col = c('lightskyblue2', 'tomato'))

mosaicplot(table(traincat$cat1, traincat$cat2), col = TRUE, las = 2, cex.axis = 0.8, shade=TRUE)

