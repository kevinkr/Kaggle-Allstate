# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")
train <- fread("Data/Raw/train.csv", stringsAsFactors=FALSE, header = TRUE)
test <- fread("Data/Raw/test.csv", stringsAsFactors=FALSE, header = TRUE)

train <- train %>% mutate_each(funs(factor), starts_with("cat"))
test <- test %>% mutate_each(funs(factor), starts_with("cat"))

##########################################
# Create split train set
set.seed(212312)
trainIndex <- createDataPartition(train$loss, p = 0.8, list=FALSE, times=1)
subTrain <- train[trainIndex,]
subTest <- train[-trainIndex,]

# Based on http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
test.m <- data.matrix(test[,1:131])

subTrain.m <- data.matrix(subTrain[,1:131])
subTrain.y <- data.matrix(subTrain$loss)

subTest.m <- data.matrix(subTest[,1:131])
subTest.y <- data.matrix(subTest$loss)

# fit model
fit = glmnet(subTrain.m, subTrain.y)
# summarize the fit
summary(fit)
plot(fit)

cv.fit <- cv.glmnet(subTrain.m,subTrain.y)
plot(cv.fit)

test.pred <- predict(fit,newx=subTest.m,s=cv.fit$lambda.min)
mte <- apply((test.pred-subTest.y)^2,2,mean)

#points(log(fit$lambda),mte,col="blue",pch="*")
#legend("topleft",legend=c("10 fold CV","Test"),pch="*",col=c("red","blue"))

plot(fit,xvar="lambda")
plot(fit,xvar="dev")

# See http://stats.stackexchange.com/questions/188753/lasso-regression-for-predicting-continuous-variable-variable-selection
plot(fit, xvar = "lambda")


predictions <- predict(fit,newx=test.m,s=cv.fit$lambda.min)
test.df <- as.data.frame(test.m)
test.df$predictionColumn<-c(predictions)


#pred.df <-as.data.frame(t(predictions))

solution <- data.frame(id = test.df$id, loss = test.df$predictionColumn)

# Write the solution to file
write.csv(solution, file = 'Submissions/glmnet-learning-101916-1.csv', row.names = F)
