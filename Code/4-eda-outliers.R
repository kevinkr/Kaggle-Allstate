# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")
source("Code/1-load data.R")
source("Code/2-plot functions.R")


###############################################
# Analyze outliers
##############################################
#consider outlier cutoff
# upper outer fence: Q3 + 3*IQ
upper.outer.fence <- quantile(train$loss,0.75) + 3*(quantile(train$loss,0.75)-quantile(train$loss,0.25))
length(which(train$loss > upper.outer.fence))

#lower inner fence
# Q1-1.5(IQR)
lower.inner.fence <- quantile(train$loss,0.25) - 1.5*(quantile(train$loss,0.75)-quantile(train$loss,0.25))
length(which(train$loss < lower.inner.fence))
# Value is 0, lower fence outside lowest value(0)

# consider analysis of outliers
# based on https://www.kaggle.com/kb3gjt/allstate-claims-severity/allstateeda1
outliers <- train[train$loss > upper.outer.fence,]
outlier.index <- which(train$loss > upper.outer.fence)
notoutliers <- train[-outlier.index]

outliers.cat <- outliers[,.SD,.SDcols = cat.var]
outliers.num <- outliers[, .SD, .SDcols = num.var]

plot(outliers$loss)
hist(log(outliers$loss),100)

plot(notoutliers$loss)
hist(log(notoutliers$loss),100)

# Plots for outliers
doPlots(outliers.cat, fun = plotBox, ii =1:12, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.cat, fun = plotBox, ii =13:24, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.cat, fun = plotBox, ii =25:36, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.cat, fun = plotBox, ii =37:48, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.cat, fun = plotBox, ii =49:60, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.cat, fun = plotBox, ii =61:72, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.cat, fun = plotBox, ii =73:84, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.cat, fun = plotBox, ii =85:96, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.cat, fun = plotBox, ii =97:108, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.cat, fun = plotBox, ii =109:116, lab=log(outliers$loss), ncol = 3)

########################
# continuous density plot
doPlots(outliers.num, fun = plotDen, ii =1:6, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.num, fun = plotDen, ii =7:14, lab=log(outliers$loss), ncol = 3)

#continuous scatterplot
doPlots(outliers.num, fun = plotScatter, ii =1:6, lab=log(outliers$loss), ncol = 3)
doPlots(outliers.num, fun = plotScatter, ii =7:14, lab=log(outliers$loss), ncol = 3)



#######################
# Not outliers
notoutliers <- train[-outlier.index]

notoutliers.cat <- notoutliers[,.SD,.SDcols = cat.var]
notoutliers.num <- notoutliers[, .SD, .SDcols = num.var]


# Plots for nonoutliers
doPlots(notoutliers.cat, fun = plotBox, ii =1:12, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.cat, fun = plotBox, ii =13:24, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.cat, fun = plotBox, ii =25:36, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.cat, fun = plotBox, ii =37:48, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.cat, fun = plotBox, ii =49:60, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.cat, fun = plotBox, ii =61:72, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.cat, fun = plotBox, ii =73:84, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.cat, fun = plotBox, ii =85:96, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.cat, fun = plotBox, ii =97:108, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.cat, fun = plotBox, ii =109:116, lab=log(notoutliers$loss), ncol = 3)

########################
# continuous density plot
doPlots(notoutliers.num, fun = plotDen, ii =1:6, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.num, fun = plotDen, ii =7:14, lab=log(notoutliers$loss), ncol = 3)

#continuous scatterplot
doPlots(notoutliers.num, fun = plotScatter, ii =1:6, lab=log(notoutliers$loss), ncol = 3)
doPlots(notoutliers.num, fun = plotScatter, ii =7:14, lab=log(notoutliers$loss), ncol = 3)


summary(outliers$loss)
plot(outliers$loss)
hist(log(outliers$loss),100)


# factorize variables
train <- train %>% mutate_each(funs(factor), starts_with("cat"))
outliers <- outliers %>% mutate_each(funs(factor), starts_with("cat"))

# create vectors to hold correlation values against loss
cc <- rep(0,132)
cco <- rep(0,132)
for (i in 1:131) cc[i] <- cor(train$loss,as.numeric(train[,i]))
for (i in 1:131) cco[i] <- cor(outliers$loss,
                               as.numeric(outliers[,i]))


# capture standard deviations
sdo <- rep(0,132)
for (i in 1:132) sdo[i] <- sd(as.numeric(outliers[,i]))
which(sdo==0)
# 16 22 23 57 63 64 71

# plot the correlations
plot(cc)
points(cco, col="red")

which(abs(cc)>0.3)

which(abs(cco)>0.3)


#################################################
## re-run with spearman's method ####################
#####################################
# create vectors to hold correlation values against loss
ccs <- rep(0,132)
ccso <- rep(0,132)
for (i in 1:131) ccs[i] <- cor(train$loss,as.numeric(train[,i]), method = "spearman")
for (i in 1:131) ccso[i] <- cor(outliers$loss,
                               as.numeric(outliers[,i]), method = "spearman")

# plot the correlations
plot(ccs)
points(ccso, col="red")




