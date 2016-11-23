library(mlr)
library(xgboost)
library(data.table)
library(parallelMap)
library(FeatureHashing)
library(BBmisc)
library(Metrics)

options(scipen=999) # remove scientific notation

TRAIN_FILE = "Data/Raw/train.csv"
TEST_FILE = "Data/Raw/test.csv"
SUBMISSION_FILE = "Data/Raw/sample_submission.csv"

train = fread(TRAIN_FILE, showProgress = TRUE)
test = fread(TEST_FILE, showProgress = TRUE)

# remove id
train[, id := NULL]
test[, id := NULL]

# transform target variable and use factor variables 
train$loss = log(train$loss + 200)
test$loss = -99

# feature preprocess
dat = rbind(train, test)

char.feat = vlapply(dat, is.character)
char.feat = names(char.feat)[char.feat]

for (f in char.feat) {
  dat[[f]] = as.integer(as.factor(dat[[f]]))
}

dat = as.data.frame(dat)

# create task
train = dat[dat$loss != -99, ]
test = dat[dat$loss == -99, ]


########################
# MLR settings
########################

# create xgboost learner for mlr package
makeRLearner.regr.xgboost.latest = function() {
  makeRLearnerRegr(
    cl = "regr.xgboost.latest",
    package = "xgboost",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "obj_par", default = 2, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "eval_metric", default = "mae", tunable = FALSE),
      makeUntypedLearnerParam(id = "objective", default = "reg:linear", tunable = FALSE),
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
  
  myobj = function(preds, train, c) {
    labels = getinfo(train, "label")
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
  #xgboost:::predict.xgb.Booster(m, newdata = data, ...)
  xgboost:::predict(m, newdata = data.matrix(.newdata), ...)
}

# create mlr train and test task
trainTask = makeRegrTask(data = as.data.frame(train), target = "loss")
testTask = makeRegrTask(data = as.data.frame(test), target = "loss")


# create mlr measure for log-transformed target
mae.log = mae
mae.log$fun = function (task, model, pred, feats, extra.args) {
  measureMAE(exp(pred$data$truth), exp(pred$data$response))
}

# mae <- function(preds, train) {
#   labels <- getinfo(train, "label")
#   err <- as.numeric(sum(abs(labels - preds)))/length(labels)
#   return(list(metric = "mae", err))
# }


## create mlr learner
set.seed(123)
## Regression gradient boosting machine, specify hyperparameters via a list
#lrn = makeLearner("regr.xgboost")
lrn = makeLearner("regr.xgboost.latest")
lrn = setHyperPars(lrn, 
                   subsample = 0.892, #
                    colsample_bytree = 0.6, #
                    max_depth = 11, #
                    lambda = 0.185,
                    alpha = 8,
                    min_child_weight=1.12, #
                    nthread = 16, 
                    nrounds = 320, #
                    eta = 0.0636 #
 )

# Access parameters
lrn$par.vals
# 
# par.set = makeParamSet(
#   makeNumericLearnerParam(id = "eta", default = 0.1, lower = 0, upper = 1),
#   makeNumericLearnerParam(id = "obj_par", default = 2, lower = 0),
#   makeIntegerLearnerParam(id = "max_depth", default = 12L, lower = 1L),
#   makeNumericLearnerParam(id = "min_child_weight", default = 5, lower = 0),
#   makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
#   makeNumericLearnerParam(id = "colsample_bytree", default = 0.2, lower = 0, upper = 1),
#   makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
#   makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
#   makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
#   makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
#   makeIntegerLearnerParam(id = "nrounds", default = 100L, lower = 1L),
#   makeIntegerLearnerParam(id = "silent", default = 0L, lower = 0L, upper = 1L, tunable = FALSE),
#   makeIntegerLearnerParam(id = "verbose", default = 1, lower = 0, upper = 2, tunable = FALSE),
#   makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE, requires = quote(verbose == 1L))
# )
# 
# ## Get the set of hyperparameters
# lrn$par.set
# 
# # predict type
# lrn$predict.type

############
# Going back to the basics
############

# # 1) Define the set of parameters you want to tune (here 'eta')
# ps = makeParamSet(
#   makeNumericParam("eta", lower = 0.01, upper = 0.5),
#   makeNumericParam("colsample_bytree", lower = 1, upper = 2, trafo = function(x) x/2),
#   makeNumericParam("subsample", lower = 0.5, upper = 2, trafo = function(x) x/2),
#   makeIntegerParam("max_depth", lower = 2, upper = 12),
# 
# )

ps <- makeParamSet(
   makeDiscreteParam("eta", values = c(0.005, 0.01, 0.02)),
   makeDiscreteParam("max_depth", values = c(1, 2, 5, 10, 20)),
    makeNumericParam("lambda", lower = 0.005, upper = 1, trafo = function(x) x/2)
)

#ps = makeParamSet(
  #makeNumericParam("colsample_bytree", lower = .05, upper = 5, trafo = function(x) x/3) 
  #makeDiscreteParam("colsample_bytree", values = c(.4, .6, .8, 1.0))
  #makeDiscreteParam("nrounds", values = c(250, 260, 270, 280, 290, 300, 310, 320, 330, 340))
  #makeNumericParam("eta", lower = .001, upper = 1, trafo = function(x) x/2)
  #makeNumericParam("obj_par", lower = 1.5, upper = 2)

#)


# # 2) Use 3-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 5L)

# # 3) Here we use Random Search (with 10 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 100L)

# # 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters and evaluate it with SQWK
res = tuneParams(lrn, trainTask, resampling = rdesc, par.set = ps, 
                 control = makeTuneControlGrid())
res
data <- generateHyperParsEffectData(res)

#plotHyperParsEffect(data, x = "eta", y = "max_depth", z = "mse.test.mean",
  #                  plot.type = "heatmap", partial.dep.learn = "regr.xgboost")

# # 5) set the optimal hyperparameter
lrn = setHyperPars(lrn, par.vals = res$x)
lrn

## Train the learner on entire train set
mod = train(lrn, trainTask)

## Peak into mod
names(mod)

mod$learner
mod$features
mod$time

## Extract the fitted model
getLearnerModel(mod)


n = getTaskSize(trainTask)
train.set = seq(1, n, by = 2)
test.set = seq(2, n, by = 2)
mod = train(lrn, trainTask, subset = train.set)

task.pred = predict(mod, task = trainTask, subset = test.set)
task.pred



# 6) make Prediction
pred = exp(getPredictionResponse(predict(mod, testTask))) - 200
summary(pred)

submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = pred
write.csv(submission, "Submissions/xgb-mlr-myown-v2-11-24-16.csv", row.names = FALSE)
