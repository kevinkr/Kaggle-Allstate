library(mlr)
library(xgboost)
library(data.table)
library(parallelMap)
library(FeatureHashing)
library(BBmisc)
library(dplyr)

# create xgboost learner for mlr package
makeRLearner.regr.xgboost.latest = function() {
  makeRLearnerRegr(
    cl = "regr.xgboost.latest",
    package = "xgboost",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "obj_par", default = 2, lower = 0),
      makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 12L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "nrounds", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "silent", default = 0L, lower = 0L, upper = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "verbose", default = 1, lower = 0, upper = 2, tunable = FALSE),
      makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE, requires = quote(verbose == 1L))
      ),
    par.vals = list(nrounds = 1L, silent = 0L, verbose = 1L, obj_par = 2),
    properties = c("numerics", "factors", "weights"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` and `verbose` to `0` by default."
  )
}

# create xgboost train and predict methods for mlr package
trainLearner.regr.xgboost.latest = function(.learner, .task, .subset, .weights = NULL,  ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  target = data$target
  data = FeatureHashing::hashed.model.matrix( ~ . - 1, data$data)
  
  myobj = function(preds, dtrain, c) {
    labels = getinfo(dtrain, "label")
    x = preds-labels
    # introduce hyperparameter for objective function
    c = .learner$par.vals$obj_par
    grad = tanh(c*x)
    hess = c*sqrt(1-grad^2)
    return(list(grad = grad, hess = hess))
  }
  
  xgboost::xgboost(data = data, label = target, objective = myobj, ...)
}
predictLearner.regr.xgboost.latest = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  data = FeatureHashing::hashed.model.matrix( ~ . - 1, .newdata)
  xgboost:::predict.xgb.Booster(m, newdata = data, ...)
}

TRAIN_FILE = "Data/Raw/train.csv"
TEST_FILE = "Data/Raw/test.csv"
SUBMISSION_FILE = "Data/Raw/sample_submission.csv"

train = fread(TRAIN_FILE, showProgress = TRUE)
test = fread(TEST_FILE, showProgress = TRUE)

# Harmonize factors
#set test loss to NA
test$loss <- -99
test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))
#bind train and test
fullSet <- rbind(test,train)
# set factor levels all full set
fullSet <- fullSet %>% mutate_each(funs(factor), starts_with("cat"))
# drop vars
#fullSet <- subset(fullSet, select = -c(cont1, cont11, cont6, cont10))
#fullSet <- subset(fullSet, select = -c(cat7 , cat14 , cat15 , cat16 , cat17 , cat18 , cat19 , cat20 , cat21 , cat22 , cat24 , cat28 , cat29 , cat30 , cat31 , cat32 , cat33 , cat34 , cat35 , cat39 , cat40 , cat41 , cat42 , cat43 , cat45 , cat46 , cat47 , cat48 , cat49 , cat51 , cat52 , cat54 , cat55 , cat56 , cat57 , cat58 , cat59 , cat60 , cat61 , cat62 , cat63 , cat64 , cat65 , cat66 , cat67 , cat68 , cat69 , cat70 , cat74 , cat76 , cat77 , cat78 , cat85 , cat89 , cat96 , cat102))
#fullSet.cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
#ohe
#library(dummies)
#train <- dummy.data.frame(train, names=cat.var, sep="_")
#test <- dummy.data.frame(test, names=test.cat.var,sep="_")
#fullSet <- dummy.data.frame(fullSet, names=fullSet.cat.var, sep="_")

# split back into test and train
test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
# drop loss from test set
#test <- subset(test, select = -c(loss))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))
test <- subset(test, select = -c(id))
train <- subset(train, select = -c(id))
rm(fullSet)
#train <- as.data.table(train)
#test <- as.data.table(test)

# remove id
#train[, id := NULL]
#test[, id := NULL]

# transform target variable and use factor variables 
train$loss = log(train$loss + 200)


#dat = as.data.frame(dat)


# create mlr measure for log-transformed target
mae.log = mae
mae.log$fun = function (task, model, pred, feats, extra.args) {
  measureMAE(exp(pred$data$truth), exp(pred$data$response))
}

# create mlr train and test task
trainTask = makeRegrTask(data = train, target = "loss")
testTask = makeRegrTask(data = test, target = "loss")

# specify mlr learner with some nice hyperpars
set.seed(123)
lrn = makeLearner("regr.xgboost.latest")
lrn = setHyperPars(lrn, 
  base_score = 7.7,
  subsample = 0.95,
  colsample_bytree = 0.45,
  max_depth = 10,
  lambda = 10,
  min_child_weight = 2.5, 
  alpha = 8,
  nthread = 16, 
  nrounds = 5000,
  eta = 0.055,
  print_every_n = 50
)

## This is how you could do hyperparameter tuning with random search
# 1) Define the set of parameters you want to tune (here we use only 'obj_par')
ps = makeParamSet(
  makeNumericParam("obj_par", lower = 1.5, upper = 2),
  makeIntegerParam("max_depth", lower = 2, upper = 6),
  makeNumericParam("min_child_weight", lower = 1.5, upper = 5),
  makeNumericParam("gamma", lower = 0, upper = 2)
)

# 2) Use 3-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 10L)

# 3) Here we use random search (with 5 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 5)

# 4) now use the learner on the training Task with the 3-fold CV to optimize your set of 
# parameters in parallel
#parallelStartMulticore(4)
res = tuneParams(lrn, task = trainTask, resampling = rdesc,
  par.set = ps, control = ctrl, measures = mae.log)
#parallelStop()
 res$x

# 5) We fit model using the hyperparameter we found from 4)
set.seed(123)
lrn = setHyperPars(lrn, obj_par = 1.911025, max_depth=6, min_child_weight = 2.781703, gamma=0.07342729)
mod = train(lrn, trainTask)
 
# 6) make Prediction
pred = exp(getPredictionResponse(predict(mod, testTask))) - 200
summary(pred)

submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = pred
write.csv(submission, "Submissions/xgb-mlr-drop-some-cont-v2-11-18-16.csv", row.names = FALSE)
