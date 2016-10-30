# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")
source("Code/1-load data.R")
source("Code/2-plot functions.R")


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
reduce_cats <- function(cat.name, cutoff.val) {
  prop.table <- sort(prop.table(table(train[[cat.name]])), decreasing = T)
  #return(proptable)
  weak.prop.table <- prop.table < cutoff.val
  #return(weak.prop.table)
  # grab the names
  weak.prop.names <- names(prop.table[prop.table < 0.01])
  return(weak.prop.names)
}

############full loop attempt
for (n in cat.var) {
  #print(n)
  # call function to return category names for reduction, number is cutoff val
  #weak.prop.names <- reduce_cats(cat.name, 0.01)
  weak.prop.names <- reduce_cats(n, 0.01)
  # filter data set by categories that are in the weak prop names vector using %in% search'
  # first convert to character
  train[[n]] <- as.character(train[[n]])
  train[train[[n]] %in% weak.prop.names, n] <- "OTHER"
  train[[n]] <- as.factor(train[[n]])
}

# review as prop.tables
prop.table(table(train[,cat108]))
train[,cat.name] <- as.factor(train[,cat.name])
str(train$cat114)


##### Examining categories in further detail
# review factor plots after reduction in labels
library(factoextra)
library(FactoMineR)
par(mfrow=c(4,3))
for (i in 1:ncol(train.cat)) {
  plot(train[[i]], main=colnames(train)[i],
       ylab = "Count", col="steelblue", las = 2)
}














#similar plots...combine buckets
#transform(mydata, newvar=paste(gender, country))






# # MCA analysis
# # see http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining
# res.mca <- MCA(train.cat, graph = FALSE)
# 
# summary(res.mca, nb.dec = 2, ncp = 2)
# 
# fviz_screeplot(res.mca)
# 
# par(mfrow=c(1,1))
# plot(res.mca)
# 
# var <- get_mca_var(res.mca)
# plot(res.mca, choix = "var")
# head(round(var$contrib,2))
# categories <- rownames(var$coord)
# library("corrplot")
# corrplot(var$contrib, is.corr = FALSE)
# # Contributions of variables on Dim.1
# fviz_contrib(res.mca, choice = "var", axes = 1)
# 
# 
# 
# 
# # contingency analysis
# library(gplots)
# dt <- as.table(as.matrix(train.cat))
# balloonplot(t(dt), main ="cats", xlab ="", ylab="",
#             label = FALSE, show.margins = FALSE)
