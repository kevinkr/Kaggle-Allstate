library(mlr)
library(xgboost)
library(data.table)
library(parallelMap)
library(FeatureHashing)
library(BBmisc)

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

# create mlr train and test task
trainTask = makeRegrTask(data = as.data.frame(train), target = "loss")
testTask = makeRegrTask(data = as.data.frame(test), target = "loss")

# create xgboost learner for mlr package
makeRLearner.regr.xgboost = function() {
  makeRLearnerRegr(
    cl = "regr.xgboost",
    package = "xgboost",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "eta", default = 0.1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "obj_par", default = 2, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 12L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 5, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 0.2, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = "mae"),
      makeIntegerLearnerParam(id = "nrounds", default = 1000L, lower = 1L),
      makeIntegerLearnerParam(id = "silent", default = 0L, lower = 0L, upper = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "verbose", default = 1, lower = 0, upper = 2, tunable = FALSE),
      makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE, requires = quote(verbose == 1L))
    ),
    properties = c("numerics", "factors", "weights"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` and `verbose` to `0` by default."
  )
}

trainLearner.regr.xgboost = function(.learner, .task, .subset, .weights = NULL,  ...) {
  td = getTaskDescription(.task)
  data = getTaskData(.task, .subset, target.extra = TRUE)
  target = data$target
  data = data.matrix(data$data)
  
  parlist = list(...)
  obj = parlist$objective
  if (testNull(obj)) {
    obj = "reg:linear"
  }
  
  if (testNull(.weights)) {
    xgboost::xgboost(data = data, label = target, objective = obj, ...)
  } else {
    xgb.dmat = xgboost::xgb.DMatrix(data = data, label = target, weight = .weights)
    xgboost::xgboost(data = xgb.dmat, label = NULL, objective = obj, ...)
  }
}

predictLearner.regr.xgboost = function(.learner, .model, .newdata, ...) {
  td = .model$task.desc
  m = .model$learner.model
  predict(m, newdata = data.matrix(.newdata), ...)
}

# create mlr measure for log-transformed target
mae.log = mae
mae.log$fun = function (task, model, pred, feats, extra.args) {
  measureMAE(exp(pred$data$truth), exp(pred$data$response))
}

## create mlr learner
set.seed(123)
lrn = makeLearner("regr.xgboost")
lrn = setHyperPars(lrn, 
                   subsample = 1.0,
                   colsample_bytree = 0.2,
                   max_depth = 10,
                   lambda = 10,
                   min_child_weight = 5, 
                   alpha = 8,
                   nthread = 16, 
                   nrounds = 500,
                   eta = 0.1,
                   eval_metric = mae.log
)

# # 1) Define the set of parameters you want to tune (here 'eta')
ps = makeParamSet(
  makeNumericParam("eta", lower = 0.1, upper = 0.3),
  makeNumericParam("colsample_bytree", lower = 1, upper = 2, trafo = function(x) x/2),
  makeNumericParam("subsample", lower = 1, upper = 2, trafo = function(x) x/2)
)
# # 2) Use 3-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 3L)

# # 3) Here we use Random Search (with 10 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 10)

# # 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters and evaluate it with SQWK
res = tuneParams(lrn, task = trainTask, resampling = rdesc, par.set = ps, control = ctrl, 
                 measures = mae)
res
# # 5) set the optimal hyperparameter
lrn = setHyperPars(lrn, par.vals = res$x)

# perform crossvalidation in parallel
cv = crossval(lrn, trainTask, iter = 3, measures = SQWK, show.info = TRUE)