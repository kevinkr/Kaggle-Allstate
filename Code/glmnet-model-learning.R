# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")
train <- fread("Data/Raw/train.csv", stringsAsFactors=FALSE, header = TRUE)
test <- fread("Data/Raw/test.csv", stringsAsFactors=FALSE, header = TRUE)

train <- train %>% mutate_each(funs(factor), starts_with("cat"))
test <- test %>% mutate_each(funs(factor), starts_with("cat"))

# Pass 1 GLMnet
library(glmnet)

# Based on http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
test.m <- data.matrix(test[,1:131])

train.m <- data.matrix(train[,1:131])
train.y <- data.matrix(train$loss)

# fit model
fit = glmnet(train.m, train.y)
# summarize the fit
summary(fit)
plot(fit)

# See http://stats.stackexchange.com/questions/188753/lasso-regression-for-predicting-continuous-variable-variable-selection
plot(fit, xvar = "lambda")

coef(fit,s=0.1)
log(0.1)

predictions <- predict(fit,newx=test.m,s=c(0.1,0.05))

pred.df <-as.data.frame(t(predictions))

solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'megan-risdal-orig-Solution-kk-v2.csv', row.names = F)
