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
subTrain.y <- data.matrix(log(subTrain$loss))

subTest.m <- data.matrix(subTest[,1:131])
subTest.y <- data.matrix(log(subTest$loss))

# fit model
fit = glmnet(subTrain.m, subTrain.y)
# summarize the fit
summary(fit)
plot(fit)


##### chose alpha = 1 based on analysis below
cv.fit <- cv.glmnet(subTrain.m,subTrain.y,type.measure = "mse", alpha=1)
plot(cv.fit)

# Examining alpha across numerous folds
# see https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
foldid=sample(1:10,size=length(subTrain.y),replace=TRUE)
cv1=cv.glmnet(subTrain.m,subTrain.y,foldid=foldid,alpha=1)
cv.5=cv.glmnet(subTrain.m,subTrain.y,foldid=foldid,alpha=.5)
cv0=cv.glmnet(subTrain.m,subTrain.y,foldid=foldid,alpha=0)

par(mfrow=c(2,2))
plot(cv1);plot(cv.5);plot(cv0)
plot(log(cv1$lambda),cv1$cvm,pch=19,col="red",xlab="log(Lambda)",ylab=cv1$name)
points(log(cv.5$lambda),cv.5$cvm,pch=19,col="grey")
points(log(cv0$lambda),cv0$cvm,pch=19,col="blue")
legend("topleft",legend=c("alpha= 1","alpha= .5","alpha= 0"),pch=19,col=c("red","grey","blue"))

# perform test again test set . . .
# NOT WORKING NOW

#test.pred <- predict(fit,newx=subTest.m,s=cv.fit$lambda.min)
#mte <- apply((test.pred-subTest.y)^2,2,mean)

#points(log(fit$lambda),mte,col="blue",pch="*")
#legend("topleft",legend=c("10 fold CV","Test"),pch="*",col=c("red","blue"))
################

plot(fit,xvar="lambda")
plot(fit,xvar="dev")

# See http://stats.stackexchange.com/questions/188753/lasso-regression-for-predicting-continuous-variable-variable-selection
#plot(fit, xvar = "lambda")


predictions <- predict(fit,newx=test.m,s=cv.fit$lambda.1se)
test.df <- as.data.frame(test.m)
test.df$predictionColumn<-c(predictions)


#pred.df <-as.data.frame(t(predictions))

solution <- data.frame(id = test.df$id, loss = round(exp(test.df$predictionColumn),2))

# Write the solution to file
write.csv(solution, file = 'Submissions/glmnet-learning-logloss-101916-3.csv', row.names = F)
