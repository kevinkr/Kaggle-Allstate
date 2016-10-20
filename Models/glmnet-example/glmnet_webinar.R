library(glmnet)
?glmnet
### HIV data
load("hiv.rda")
dim(hiv.train$x)
fit=glmnet(hiv.train$x,hiv.train$y)
plot(fit)
fit
cv.fit=cv.glmnet(hiv.train$x,hiv.train$y)
plot(cv.fit)
tpred=predict(fit,hiv.test$x)
mte=apply((tpred-hiv.test$y)^2,2,mean)
points(log(fit$lambda),mte,col="blue",pch="*")
legend("topleft",legend=c("10 fold CV","Test"),pch="*",col=c("red","blue"))
plot(fit,xvar="lambda")
plot(fit,xvar="dev")

###Internet Ad Data
load("InternetAd.RData")
attach(InternetAd)

dim(x)
class(x)
nnzero(x)
100*nnzero(x)/prod(dim(x))
table(y)
fit=glmnet(x,y,standardize=FALSE,family="binomial")
plot(fit)
fitpos=update(fit,lower=-1,upper=1)
plot(fitpos,ylim=c(-1,2))
plot(fit,xvar="dev")
cfit=cv.glmnet(x,y,standardize=FALSE,family="binomial")
plot(cfit)
fit=glmnet(x,y,family="binomial")
plot(fit)
plot(fit,xvar="dev")
cvfit.a=cv.glmnet(x,y,family="binomial",type.measure="auc")
plot(cvfit.a)
?cv.glmnet

#### Multiclass classification
attach("tamayo.RData")
attach(tamayo.train)
table(y)
fit=glmnet(x3,y,family="multinomial")
par(mfrow=c(4,4));plot(fit,xvar="dev")
cvfit=cv.glmnet(x3,y,family="multinomial",nfolds=8)
par(mfrow=c(1,1));plot(cvfit)
pred=predict(cvfit,newx=tamayo.test$x3,type="class")
mean(pred!=tamayo.test$y)
fit2=glmnet(x3,y,family="multinomial",type.multinomial="grouped")
par(mfrow=c(4,4));plot(fit2,xvar="dev")

