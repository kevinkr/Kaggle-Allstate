# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")
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
model.lm <- lm(loss ~ cat1, data = train)
summary(model.lm)


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
par(mfrow=c(2,1))
boxplot(log(loss) ~ cat113, data = train, ylab = "Log(Loss)")
boxplot(loss ~ cat113, data = train, ylab = "Loss")

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

par(mfrow=c(1,1))


##############################
# Function
##############################


myplot = function(df, x_string, y_string, z_string) {
  p1 <- ggplot(df, aes_string(x = x_string, y = y_string)) + geom_boxplot() 
  p2 <- ggplot(df, aes_string(x = x_string, y = z_string)) + geom_boxplot() 
  multiplot(p1, p2, cols=1)
}

lm_cats <- function(cat.name) {
  #print(cat.name)
  # plot first
  myplot(train.cat, cat.name, "loss", "logloss")
  # add save command
  
  # create subset and one hot encoded variables
  df <- subset(train, select = c(loss,cat113))
  
  cat113 <- dummy.data.frame(cat113, names=c("cat113"), sep="_")
}

lm_cats("cat1")


p <- ggplot(train, aes(x=cat1, y=loss)) + 
  geom_boxplot()

p









# test plot... print all cats
par(mfrow=c(3,3), mar=c(3, 3, 0.5, 0.5), mgp = c(1.5, 0.3, 0), tck = -0.01,
    oma=c(0, 0, 1, 0))

sapply(seq_along(train.cat)[-1], function(i) {
  y <- train.cat[, i]
  boxplot(train.cat$loss ~ y, outline=FALSE, ylab="VarLevel", tck = 1.0, 
          las=1)
})




# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
