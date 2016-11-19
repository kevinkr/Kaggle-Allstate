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


########################
# MLR settings
########################

# create mlr train and test task
trainTask = makeRegrTask(data = as.data.frame(train), target = "loss")
testTask = makeRegrTask(data = as.data.frame(test), target = "loss")

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

## create mlr learner
set.seed(123)
## Regression gradient boosting machine, specify hyperparameters via a list
#lrn = makeLearner("regr.xgboost")
lrn = makeLearner("regr.xgboost")
lrn = setHyperPars(lrn, 
                    subsample = 1.0,
                    colsample_bytree = 0.2,
                    max_depth = 10,
                    lambda = 10,
                    min_child_weight = 5, 
                    alpha = 8,
                    nthread = 16, 
                    nrounds = 100,
                    eta = 0.1,
                    eval_metric = xg_eval_mae
 )

# Access parameters
lrn$par.vals

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
  makeIntegerLearnerParam(id = "nrounds", default = 100L, lower = 1L),
  makeIntegerLearnerParam(id = "silent", default = 0L, lower = 0L, upper = 1L, tunable = FALSE),
  makeIntegerLearnerParam(id = "verbose", default = 1, lower = 0, upper = 2, tunable = FALSE),
  makeIntegerLearnerParam(id = "print_every_n", default = 1L, lower = 1L, tunable = FALSE, requires = quote(verbose == 1L))
)

## Get the set of hyperparameters
lrn$par.set

# predict type
lrn$predict.type

############
# Going back to the basics
############

# # 1) Define the set of parameters you want to tune (here 'eta')
ps = makeParamSet(
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("colsample_bytree", lower = 1, upper = 2, trafo = function(x) x/2),
  makeNumericParam("subsample", lower = 0.5, upper = 2, trafo = function(x) x/2),
  makeIntegerParam("max_depth", lower = 2, upper = 12),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeIntegerParam("nrounds",lower=200,upper=600)
)

# # 2) Use 3-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 5L)

# # 3) Here we use Random Search (with 10 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 100L)

# # 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters and evaluate it with SQWK
res = tuneParams(lrn, task = trainTask, resampling = rdesc, par.set = ps, control = ctrl )
res
data <- generateHyperParsEffectData(res, partial.dep = TRUE)

#plotHyperParsEffect(data, x = "eta", y = "max_depth", z = "mse.test.mean",
                    plot.type = "heatmap", partial.dep.learn = "regr.xgboost")

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
write.csv(submission, "Submissions/xgb-mlr-myown-v1-11-21-16.csv", row.names = FALSE)
