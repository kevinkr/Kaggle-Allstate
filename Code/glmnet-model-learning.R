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
cv <- cv.glmnet(train.m,train.y,nfolds=3)
# summarize the fit
summary(fit)
plot(fit)

# See http://stats.stackexchange.com/questions/188753/lasso-regression-for-predicting-continuous-variable-variable-selection
plot(fit, xvar = "lambda")


predictions <- predict(fit,newx=test.m,s=cv$lambda.min)
test.df <- as.data.frame(test.m)
test.df$predictionColumn<-c(predictions)


#pred.df <-as.data.frame(t(predictions))

solution <- data.frame(id = test.df$id, loss = test.df$predictionColumn)

# Write the solution to file
write.csv(solution, file = 'Submissions/glmnet-learning-101916-1.csv', row.names = F)
