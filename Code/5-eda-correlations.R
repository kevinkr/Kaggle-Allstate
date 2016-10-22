# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")
source("Code/1-load data.R")
source("Code/2-plot functions.R")

# Create lists of column names
cat.var <- names(train)[which(sapply(train, is.character))]
num.var <- names(train)[which(sapply(train, is.numeric))]
num.var <- setdiff(num.var, c("id", "loss"))

# split off categorical and numerical columns
train.cat <- train[,.SD,.SDcols = cat.var]
train.num <- train[, .SD, .SDcols = num.var]

#########################
#Examine correlations
##########################

#############################
# Correlations of continuous
##############################
correlations <- cor(train.num)
corrplot(correlations, method="square", order="hclust")



# factor analysis - see http://rtutorialseries.blogspot.com/2011/10/r-tutorial-series-exploratory-factor.html

corMat <- cor(train.num)
solution <- fa(r = corMat, nfactors = 2, rotate = "oblimin", fm = "pa")
solution

###############################
# Intro to Anova test
##############################

anova(lm(loss ~ cont1 + cont2 + cont3 + cont4 + cont5 + 
           cont6 + cont7 + cont8 + cont9 + cont10 + cont11 + 
           cont12 + cont13 + cont14, train))



##################
# Correlations of categorical variables
########################
library(gmodels)
CrossTable(train$cat5,train$cat2, prop.t=FALSE,prop.r=FALSE,prop.c=FALSE, prop.chisq = TRUE)

chisq.test(train$cat110,train$cat2, correct=FALSE)

# 3-Way Frequency Table
mytable <- xtabs(~cat1+cat110, data=train.cat)
ftable(mytable) # print table 
summary(mytable) # chi-square test of indepedenc




###############################
# Correlations of loss vs. categorical
#############################
model.lm <- lm(loss ~ cat1, data = train)
summary(model.lm)



