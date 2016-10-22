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

#IF NEEEDED##
#Combine training and test datasets for feature engineering
#allstate.combined <- rbind(test, train)
#str(allstate.combined)

#Rename and create local data frame for simplicity
#data<- tbl_df (allstate.combined)
#str(data, list.len = 999) 

#Examine data
str(train, list.len = 999) 
str(test, list.len = 999) 


# Create lists of column names
cat.var <- names(train)[which(sapply(train, is.character))]
num.var <- names(train)[which(sapply(train, is.numeric))]
num.var <- setdiff(num.var, c("id", "loss"))

# split off categorical and numerical columns
train.cat <- train[,.SD,.SDcols = cat.var]
train.num <- train[, .SD, .SDcols = num.var]


########################
# Examine continuous variables
########################
plot(train$loss)
hist(log(train$loss),100)

########################
# continuous density plot
doPlots(train.num, fun = plotDen, ii =1:6, lab=log(train$loss), ncol = 3)
doPlots(train.num, fun = plotDen, ii =7:14, lab=log(train$loss), ncol = 3)

#continuous scatterplot
doPlots(train.num, fun = plotScatter, ii =1:6, lab=log(train$loss), ncol = 3)
doPlots(train.num, fun = plotScatter, ii =7:14, lab=log(train$loss), ncol = 3)

densityplot(train$cont3, data=train, groups=train$cat1, 
            xlab=list(label="Kernel Density of cont3", fontsize=20), ylab="", 
            main=list(label="Density of cat1", fontsize=24), 
            auto.key=list(corner=c(0,0), x=0.4, y=0.8, cex=2), scales=list(cex=1.5))

g <- ggplot(train, aes(x=loss))
g + geom_histogram(binwidth = 5) + geom_density()

g <- ggplot(train, aes(x=loss))
g + geom_density()


g <- ggplot(train, aes(x=loss, y = cont1))
g + geom_point(aes(color=cat1)) + facet_wrap(~cat1)

g <- ggplot(train, aes(x=loss, y = cont1))
g + geom_point(aes(color=cat1)) + facet_wrap(~cat2)

g + geom_point(aes(color=cat110)) 


summary(train$loss)


# describe various statistics
describe(train$loss)
describe(train.num)


# view plot of continuous variables
boxplot(train.num, main ="Test Data Continuos Vars")


ggplot(train) + geom_histogram(mapping=aes(x=cont1))

plot(train$loss,exp(train.num$cont1))
plot(train$loss,train.num$cont1)


aggregate(train$loss, mean, sd)





#pairs(~ loss + cont1 + cont2 + cont3, data = train, main = "Continuous scatter plot")

#splom ( train[118:122] , groups = train$cat1, auto.key = TRUE )

