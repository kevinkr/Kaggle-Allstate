library(mlr)
library(mboost)
library(data.table)
library(party)
#library(FeatureHashing)
#library(BBmisc)

#' @export
makeRLearner.regr.blackboost = function() {
  makeRLearnerRegr(
    cl = "regr.blackboost",
    package = c("mboost", "party"),
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", default = "Gaussian", values = c("Gaussian", "Laplace",
                                                                               "Huber", "Poisson", "GammaReg", "NBinomial", "Hurdle", "custom.family")),
      # families 'Poisson', 'NBinomial' and 'Hurdle' are for count data
      makeUntypedLearnerParam(id = "custom.family.definition", requires = quote(family == "custom.family")),
      makeNumericVectorLearnerParam(id = "nuirange", default = c(0,100), requires = quote(family %in% c("GammaReg", "NBinomial", "Hurdle"))),
      makeNumericLearnerParam(id = "d", requires = quote(family == "Huber")),
      makeIntegerLearnerParam(id = "mstop", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "nu", default = 0.1, lower = 0, upper = 1),
      makeDiscreteLearnerParam(id = "risk", default = "inbag", values = c("inbag", "oobag", "none")),
      makeLogicalLearnerParam(id = "stopintern", default = FALSE),
      # 'risk' and 'stopintern' will be kept for completeness sake
      makeLogicalLearnerParam(id = "trace", default = TRUE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "teststat", default = "max", values = c("quad", "max")),
      makeDiscreteLearnerParam(id = "testtype", default = "Teststatistic", values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
      makeNumericLearnerParam(id = "mincriterion", default = 0, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", default = 7L, lower = 1L),
      makeLogicalLearnerParam(id = "stump", default = FALSE),
      makeIntegerLearnerParam(id = "nresample", default = 9999L, lower = 1L, requires = quote(testtype == "MonteCarlo")),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 0L, lower = 0L),
      makeIntegerLearnerParam(id = "mtry", default = 0L, lower = 0L),
      makeLogicalLearnerParam(id = "savesplitstats", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "maxdepth", default = 2L, lower = 0L)
    ),
    properties = c("numerics", "factors", "weights", "missings"),
    name = "Gradient Boosting with Regression Trees",
    short.name = "blackboost",
    note = "See `?ctree_control` for possible breakage for nominal features with missingness."
  )
}

trainLearner.regr.blackboost = function(.learner, .task, .subset, .weights = NULL, family = "Gaussian", nuirange = c(0,100), d = NULL, custom.family.definition, mstop, nu, risk, stopintern, trace, teststat, testtype, mincriterion, maxdepth, savesplitstats, ...) {
  ctrl = learnerArgsToControl(mboost::boost_control, mstop, nu, risk, stopintern, trace)
  defaults = getDefaults(getParamSet(.learner))
  if (missing(teststat)) teststat = defaults$teststat
  if (missing(testtype)) testtype = defaults$testtype
  if (missing(mincriterion)) mincriterion = defaults$mincriterion
  if (missing(maxdepth)) maxdepth = defaults$maxdepth
  if (missing(savesplitstats)) savesplitstats = defaults$savesplitstats
  tc =  learnerArgsToControl(party::ctree_control, teststat, testtype, mincriterion,
                             maxdepth, savesplitstats, ...)
  family = switch(family,
                  Gaussian = mboost::Gaussian(),
                  Laplace = mboost::Laplace(),
                  Huber = mboost::Huber(d),
                  Poisson = mboost::Poisson(),
                  GammaReg = mboost::GammaReg(nuirange = nuirange),
                  NBinomial = mboost::NBinomial(nuirange = nuirange),
                  Hurdle = mboost::Hurdle(nuirange = nuirange),
                  custom.family = custom.family.definition
  )
  f = getTaskFormula(.task)
  if (!is.null(.weights))
    mboost::blackboost(f, data = getTaskData(.task, .subset), control = ctrl, tree_controls = tc, weights = .weights, family = family, ...)
  else
    mboost::blackboost(f, data = getTaskData(.task, .subset), control = ctrl, tree_controls = tc, family = family, ...)
}

predictLearner.regr.blackboost = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)[, 1L]
}


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

# drop vars
dat <- subset(dat, select = -c(cont3, cont5, cont6))
dat <- subset(dat, select = -c(cat7 , cat14 , cat15 , cat16 , cat17 , cat18 , cat19 , cat20 , cat21 , cat22 , cat24 , cat28 , cat29 , cat30 , cat31 , cat32 , cat33 , cat34 , cat35 , cat39 , cat40 , cat41 , cat42 , cat43 , cat45 , cat46 , cat47 , cat48 , cat49 , cat51 , cat52 , cat54 , cat55 , cat56 , cat57 , cat58 , cat59 , cat60 , cat61 , cat62 , cat63 , cat64 , cat65 , cat66 , cat67 , cat68 , cat69 , cat70 , cat74 , cat76 , cat77 , cat78 , cat85 , cat89 , cat96 , cat102))


char.feat = vlapply(dat, is.character)
char.feat = names(char.feat)[char.feat]



for (f in char.feat) {
 # dat[[f]] = as.integer(as.factor(dat[[f]]))
  dat[[f]] = as.factor(dat[[f]])
}
library(dplyr)
dat <- dat %>% mutate_each(funs(factor), starts_with("cat"))

dat = as.data.frame(dat)

# create task
train = dat[dat$loss != -99, ]
test = dat[dat$loss == -99, ]

# create mlr measure for log-transformed target
mae.log = mae
mae.log$fun = function (task, model, pred, feats, extra.args) {
  measureMAE(exp(pred$data$truth), exp(pred$data$response))
}

# create mlr train and test task
trainTask = makeRegrTask(data = as.data.frame(train), target = "loss")
testTask = makeRegrTask(data = as.data.frame(test), target = "loss")

# specify mlr learner with some nice hyperpars
set.seed(123)
learner = makeLearner("regr.blackboost")

# Choose resampling strategy and define grid
rdesc <- makeResampleDesc("CV", iters = 5)

ps <- makeParamSet(makeIntegerParam("mtry", 3, 11),
                   makeIntegerParam("maxdepth", 5, 10))

res = tuneParams(learner, trainTask, rdesc, par.set = ps,
                 control = makeTuneControlGrid(), measures = mae.log, show.info = TRUE)

mod = train(learner, trainTask)


# 6) make Prediction
pred = exp(getPredictionResponse(predict(mod, testTask))) - 200
summary(pred)

submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = pred
write.csv(submission, "xgb-mlr-starter-v1-11-18-16.csv", row.names = FALSE)