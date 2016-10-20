# Allstate Kaggle competition
# Start: 10-13-16

#load packages
source("Code/packages.R")

# Pass 1 GLMnet
require(glmnet)
yvar <- train$loss
##returns variables from lasso variable selection, use alpha=0 for ridge
ezlasso=function(train,yvar,folds=10,trace=F,alpha=1){
  x<-model.matrix(as.formula(paste(yvar,"~.")),data=train)
  x=x[,-1] ##remove intercept
  
  glmnet1<-glmnet::cv.glmnet(x=x,y=df[,yvar],type.measure='mse',nfolds=folds,alpha=alpha)
  
  co<-coef(glmnet1,s = "lambda.1se")
  inds<-which(co!=0)
  variables<-row.names(co)[inds]
  variables<-variables[!(variables %in% '(Intercept)')];
  return( c(yvar,variables));
}

#converts ezvif or ezlasso results into formula
ezformula=function(v,operator=' + '){
  return(as.formula(paste(v[1],'~',paste(v[-1],collapse = operator))))
}

