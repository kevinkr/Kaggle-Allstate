# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")
source("Code/1-load data.R")
source("Code/2-plot functions.R")


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

cor(train$cat5,train$cat2, method="kendall")

library(GoodmanKruskal)
GKtau(train$cat1,train$cat2)

Frame1 <- subset(train, select = c(cat41:cat50,cat81:cat90))
GKmatrix1 <- GKtauDataframe(Frame1)
plot(GKmatrix1)


Frame1 <- subset(train, select = c(cat11:cat20))
GKmatrix1 <- GKtauDataframe(Frame1)
plot(GKmatrix1)

Frame1 <- subset(train, select = c(cat21:cat30))
GKmatrix1 <- GKtauDataframe(Frame1)
plot(GKmatrix1)

Frame1 <- subset(train, select = c(cat31:cat50))
GKmatrix1 <- GKtauDataframe(Frame1)
plot(GKmatrix1)

Frame1 <- subset(train, select = c(cat51:cat70))
GKmatrix1 <- GKtauDataframe(Frame1)
plot(GKmatrix1)

Frame1 <- subset(train, select = c(cat71:cat90))
GKmatrix1 <- GKtauDataframe(Frame1)
plot(GKmatrix1)


Frame1 <- subset(train, select = c(cat91:cat116))
GKmatrix1 <- GKtauDataframe(Frame1)
plot(GKmatrix1)

levels(train$cat103)



# If you want a correlation matrix of categorical variables, you can use the 
# following wrapper function (requiring the 'vcd' package):
# vars is a string vector of categorical variables you want to correlate
# dat is a data.frame containing the variables
# The result is a matrix of Cramer's V's.

library(vcd)
catcorrm <- function(vars, dat) sapply(vars, 
              function(y) sapply(vars, 
              function(x) assocstats(table(dat[,x], dat[,y]))$cramer))

catcorrMatrix <- catcorrm(cat.var, train)


###
# What this code does is, it is trying to fit in Linear Model for each level of X2. This 
# gave me all P-value and R-square, Residual standard error which I understand and 
# can interpret. 
by(train,train$cat2,function(x) summary(lm(loss~cat1,data=train)))
by(train,train$cat116,function(x) summary(lm(loss~cat1,data=train)))


###############################
# Correlations of loss vs. continuous
#############################
model.lm <- lm(loss ~ cat1, data = train)
summary(model.lm)


#############################
# Baseline model
######################
best.guess <- mean(subTrain$loss)

# Evaluate RMSE abd MAE on the test data
RMSE.baseline <- sqrt(mean(best.guess - subTest$loss)^2)
RMSE.baseline


MAE.baseline <- mean(abs(best.guess-subTest$loss))
MAE.baseline

lin.reg <-lm(log(loss) ~ cont1 + cont2 + cont3 +cont4 + cont5 + cont6 + cont7
                + cont8 + cont9 + cont10 + cont11 + cont12 + cont13 + cont14, data=train)
summary(lin.reg)

test.pred.lin <- exp(predict(lin.reg,subTest))
RMSE.lin.reg <- sqrt(mean(test.pred.lin - subTest$loss)^2)
RMSE.lin.reg

MAE.lin.reg <- mean(abs(test.pred.lin-subTest$loss))
MAE.lin.reg


lin.reg <-lm(log(loss) ~ cont1 + cont2 +cont4 + cont7
             + cont8 + cont9 + cont10 + cont11 + cont12 + cont13 + cont14, data=train)
summary(lin.reg)

test.pred.lin <- exp(predict(lin.reg,subTest))
RMSE.lin.reg <- sqrt(mean(test.pred.lin - subTest$loss)^2)
RMSE.lin.reg

MAE.lin.reg <- mean(abs(test.pred.lin-subTest$loss))
MAE.lin.reg

# Examine cont3 and cont4
anova(lm(log(loss) ~ cont3 + cont5 + cont6, train))
anova(lin.reg)

#######################
# correlation between categorical and continuous
######################
boxplot(log(loss) ~ cat1, data = train, ylab = "Duration of conversation")

# Create dummy variables
# create subset 
cat1 <- subset(train, select = c(loss,cat1))
library(dummies)

cat1 <- dummy.data.frame(cat1, names=c("cat1"), sep="_")

model.lm <- lm(loss ~ ., data = cat1)
summary(model.lm)
alias(model.lm)


# cat116
boxplot(log(loss) ~ cat116, data = train, ylab = "Duration of conversation")

# Create dummy variables
# create subset 
cat116 <- subset(train, select = c(loss,cat116))

cat116 <- dummy.data.frame(cat116, names=c("cat116"), sep="_")

model.lm <- lm(loss ~ ., data = cat116)
summary(model.lm)
alias(model.lm)

# cat115
boxplot(log(loss) ~ cat115, data = train, ylab = "Duration of conversation")

# Create dummy variables
# create subset 
cat115 <- subset(train, select = c(loss,cat115))

cat115 <- dummy.data.frame(cat115, names=c("cat115"), sep="_")

model.lm <- lm(loss ~ ., data = cat115)
summary(model.lm)
alias(model.lm)
# get pvalues example - http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
summary(model.lm)$coefficients[,4]
summary(model.lm)$r.squared

# R2 
rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
# This isn't the correlation. What it actually represents is the correlation between 
# the observed durations, and the ones predicted (fitted) by our model.

# Just to check, the Pearson correlation between observed and fitted values is
cor(cat115$loss, model.lm$fitted)

# Visualize on a scatter plot
plot(x = model.lm$fitted, y = cat115$loss,
     xlab = "Fitted loss", ylab = "Observed loss")
abline(lm(cat115$loss ~ model.lm$fitted), col="red")


#######################
######################## cat114
boxplot(log(loss) ~ cat114, data = train, ylab = "Duration of conversation")

# Create dummy variables
# create subset 
cat114 <- subset(train, select = c(loss,cat114))

cat114 <- dummy.data.frame(cat114, names=c("cat114"), sep="_")

model.lm <- lm(loss ~ ., data = cat114)
summary(model.lm)
alias(model.lm)
# get pvalues example - http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
summary(model.lm)$coefficients[,4]
summary(model.lm)$r.squared

# R2 
rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
# This isn't the correlation. What it actually represents is the correlation between 
# the observed durations, and the ones predicted (fitted) by our model.

# Just to check, the Pearson correlation between observed and fitted values is
cor(cat114$loss, model.lm$fitted)

# Visualize on a scatter plot
plot(x = model.lm$fitted, y = cat114$loss,
     xlab = "Fitted loss", ylab = "Observed loss")
abline(lm(cat114$loss ~ model.lm$fitted), col="red")

#######################
######################## cat113
boxplot(log(loss) ~ cat113, data = train, ylab = "Duration of conversation")

# Create dummy variables
# create subset 
cat113 <- subset(train, select = c(loss,cat113))

cat113 <- dummy.data.frame(cat113, names=c("cat113"), sep="_")

model.lm <- lm(loss ~ ., data = cat113)
summary(model.lm)
alias(model.lm)
# get pvalues example - http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
summary(model.lm)$coefficients[,4]
summary(model.lm)$r.squared

# R2 
rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
# This isn't the correlation. What it actually represents is the correlation between 
# the observed durations, and the ones predicted (fitted) by our model.

# Just to check, the Pearson correlation between observed and fitted values is
cor(cat113$loss, model.lm$fitted)

# Visualize on a scatter plot
plot(x = model.lm$fitted, y = cat113$loss,
     xlab = "Fitted loss", ylab = "Observed loss")
abline(lm(cat113$loss ~ model.lm$fitted), col="red")



# if NA in results, Some of the variables are not defined because of singularity 
# means that the variables are not linearly independent. If you remove the variables 
# that are giving NA in the above summary, you will obtain the same result for the 
# rest of the variables. This is because the information given by those variables 
# is already contained in the other variables and thus redundant.


# Also
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
lmp(model.lm)

