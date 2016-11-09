# Allstate Kaggle competition
# Start: 10-13-16

#load packages
#source("Code/packages.R")
source("Code/1-load data.R")
source("Code/2-plot functions.R")


cat.var <- names(train)[which(sapply(train, is.factor))]
train.cat <- train[,1:116]
train.cat$loss <- train$loss
train.cat$logloss <- log(train$loss)


#########################
#Examine correlations
##########################

###############################
# Correlations of loss vs. continuous
#############################
g0 <- lm(loss ~ cont1 + cont2 + cont3 + cont4 + cont5 + 
     cont6 + cont7 + cont8 + cont9 + cont10 + cont11 + 
     cont12 + cont13 + cont14, train)
summary(g0)
L0 <- logLik(g0)
L0

g1 <- lm(loss ~ cont1 + cont2 + cont3 + cont4 + cont5 + 
           cont6 + cont7 + cont8 + cont9 + cont10 + cont11 + 
           cont12 + cont13 + cont14 + cat1, train)
summary(g1)
L1 <- logLik(g1)
L1
k <- levels(train$cat1)
k <- NROW(k)
k
pval <- 1-pchisq(2*(L1-L0),df=k-1)
pval

# F-test between 2 normal populations with hypothesis that variances 
# of the 2 populations are equal. If p much greater than 0.05, variances are equal
anova(g1,g0)

# ===============================
# adding cat1 + cat2
#
g0 <- lm(loss ~ cont1 + cont2 + cont3 + cont4 + cont5 + 
           cont6 + cont7 + cont8 + cont9 + cont10 + cont11 + 
           cont12 + cont13 + cont14 + cat1 + cat2 + cat3 + cat4 +
           cat5 + cat6 + cat7, train)
summary(g0)
L0 <- logLik(g0)
L0

g1 <- lm(loss ~ cont1 + cont2 + cont3 + cont4 + cont5 + 
           cont6 + cont7 + cont8 + cont9 + cont10 + cont11 + 
           cont12 + cont13 + cont14 + cat1 + cat2 + cat3 + cat4 +
           cat5 + cat6 + cat7 + cat8, train)
summary(g1)
L1 <- logLik(g1)
L1
k <- levels(train$cat9)
k <- NROW(k)
k
pval <- 1-pchisq(2*(L1-L0),df=k-1)
pval

# F-test between 2 normal populations with hypothesis that variances 
# of the 2 populations are equal. If p much greater than 0.05, variances are equal
anova(g1,g0)
