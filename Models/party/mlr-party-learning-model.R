library(party)
library(data.table)
#library(Matrix)
#library(xgboost)
#library(Metrics)
library(dplyr)
library(caret)
library(e1071)
library(MASS)
library(mlr)
library(forecast)

ID = 'id'
TARGET = 'loss'

TRAIN_FILE = "Data/Raw/train.csv"
TEST_FILE = "Data/Raw/test.csv"
SUBMISSION_FILE = "Data/Raw/sample_submission.csv"


train = fread(TRAIN_FILE, showProgress = TRUE)
test = fread(TEST_FILE, showProgress = TRUE)

# remove skewness in train
for (f in colnames(train)[colnames(train) %like% "^cont"]) {
  tst <- e1071::skewness(train[, eval(as.name(f))])
  if (tst > .25) {
    if (is.na(train[, BoxCoxTrans(eval(as.name(f)))$lambda])) next
    train[, eval(as.name(f)) := BoxCox(eval(as.name(f)), BoxCoxTrans(eval(as.name(f)))$lambda)]
  }
}

# scale train
for (f in colnames(train)[colnames(train) %like% "^cont"]) {
  train[, eval(as.name(f)) := scale(eval(as.name(f)))]
}

# remove skewness in test
for (f in colnames(test)[colnames(test) %like% "^cont"]) {
  tst <- e1071::skewness(test[, eval(as.name(f))])
  if (tst > .25) {
    if (is.na(test[, BoxCoxTrans(eval(as.name(f)))$lambda])) next
    test[, eval(as.name(f)) := BoxCox(eval(as.name(f)), BoxCoxTrans(eval(as.name(f)))$lambda)]
  }
}

# scale test
for (f in colnames(test)[colnames(test) %like% "^cont"]) {
  test[, eval(as.name(f)) := scale(eval(as.name(f)))]
}

cat.var <- names(train)[which(sapply(train, is.character))]
test.cat.var <- names(test)[which(sapply(test, is.character))]

# manually adjust factors after reviewing boxplot
levels(train$cat98)[levels(train$cat98)=="D"] <- "A"
levels(train$cat109)[levels(train$cat109)=="BD"] <- "T"
levels(train$cat110)[levels(train$cat110)=="DJ"] <- "AR"
levels(train$cat111)[levels(train$cat111)=="O"] <- "M"
levels(train$cat112)[levels(train$cat112)=="AY"] <- "AA"
levels(train$cat112)[levels(train$cat112)=="X"] <- "M"
levels(train$cat112)[levels(train$cat112)=="C"] <- "G"
levels(train$cat113)[levels(train$cat113)=="AO"] <- "J"
levels(train$cat115)[levels(train$cat115)=="N"] <- "K"
levels(train$cat116)[levels(train$cat116)=="LO"] <- "MD"
levels(train$cat116)[levels(train$cat116)=="EL"] <- "GI"
levels(train$cat116)[levels(train$cat116)=="BP"] <- "FB"
levels(train$cat116)[levels(train$cat116)=="JW"] <- "AC"
levels(train$cat116)[levels(train$cat116)=="HN"] <- "L"

levels(test$cat98)[levels(test$cat98)=="D"] <- "A"
levels(test$cat109)[levels(test$cat109)=="BD"] <- "T"
levels(test$cat110)[levels(test$cat110)=="DJ"] <- "AR"
levels(test$cat111)[levels(test$cat111)=="O"] <- "M"
levels(test$cat112)[levels(test$cat112)=="AY"] <- "AA"
levels(test$cat112)[levels(test$cat112)=="X"] <- "M"
levels(test$cat112)[levels(test$cat112)=="C"] <- "G"
levels(test$cat113)[levels(test$cat113)=="AO"] <- "J"
levels(test$cat115)[levels(test$cat115)=="N"] <- "K"
levels(test$cat116)[levels(test$cat116)=="LO"] <- "MD"
levels(test$cat116)[levels(test$cat116)=="EL"] <- "GI"
levels(test$cat116)[levels(test$cat116)=="BP"] <- "FB"
levels(test$cat116)[levels(test$cat116)=="JW"] <- "AC"
levels(test$cat116)[levels(test$cat116)=="HN"] <- "L"

######
# category reduction fucntion
######
# inputs category name, cutoff value
reduce_cats <- function(cat.name, cutoff.val) {
  prop.table <- sort(prop.table(table(train[[cat.name]])), decreasing = T)
  #return(proptable)
  weak.prop.table <- prop.table < cutoff.val
  #return(weak.prop.table)
  # grab the names
  weak.prop.names <- names(prop.table[prop.table < cutoff.val])
  return(weak.prop.names)
}

############full loop attempt
for (n in cat.var) {
  #print(n)
  # call function to return category names for reduction, number is cutoff val
  #weak.prop.names <- reduce_cats(cat.name, 0.01)
  weak.prop.names <- reduce_cats(n, 0.01)
  # filter data set by categories that are in the weak prop names vector using %in% search'
  # first convert to character
  train[[n]] <- as.character(train[[n]])
  train[train[[n]] %in% weak.prop.names, n] <- "OTHER"
  train[[n]] <- as.factor(train[[n]])
}


####
# test version
#####
reduce_cats <- function(cat.name, cutoff.val) {
  prop.table <- sort(prop.table(table(test[[cat.name]])), decreasing = T)
  #return(proptable)
  weak.prop.table <- prop.table < cutoff.val
  #return(weak.prop.table)
  # grab the names
  weak.prop.names <- names(prop.table[prop.table < cutoff.val])
  return(weak.prop.names)
}

############full loop attempt
for (n in test.cat.var) {
  #print(n)
  # call function to return category names for reduction, number is cutoff val
  #weak.prop.names <- reduce_cats(cat.name, 0.01)
  weak.prop.names <- reduce_cats(n, 0.01)
  # filter data set by categories that are in the weak prop names vector using %in% search'
  # first convert to character
  test[[n]] <- as.character(test[[n]])
  test[test[[n]] %in% weak.prop.names, n] <- "OTHER"
  test[[n]] <- as.factor(test[[n]])
}

# Harmonize factors
#set test loss to NA
test$loss <- -99
train$loss <- train$loss + 200
test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))

#bind train and test
fullSet <- rbind(test,train)
# set factor levels all full set
fullSet <- fullSet %>% mutate_each(funs(factor), starts_with("cat"))
# split back into test and train
test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
# drop loss from test set
test <- subset(test, select = -c(loss))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))
rm(fullSet, train.cat, test.cat)

gc()

#' @export
makeRLearner.regr.cforest = function() {
  makeRLearnerRegr(
    cl = "regr.cforest",
    package = "party",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", lower = 1L, default = 500L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L, default = 5L),
      makeLogicalLearnerParam(id = "replace", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeNumericLearnerParam(id = "fraction", lower = 0, upper = 1, default = 0.632),
      makeDiscreteLearnerParam(id = "teststat", values = c("quad", "max"), default = 'quad'),
      makeDiscreteLearnerParam(id = "testtype",
                               values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"),
                               default = 'Univariate'),
      makeNumericLearnerParam(id = "mincriterion", lower = 0, default = 0),
      makeIntegerLearnerParam(id = "minsplit", lower = 1L, default = 20L),
      makeIntegerLearnerParam(id = "minbucket", lower = 1L, default = 7L),
      makeLogicalLearnerParam(id = "stump", default = FALSE),
      makeIntegerLearnerParam(id = "nresample", lower = 1L, default = 9999L),
      makeIntegerLearnerParam(id = "maxsurrogate", lower = 0L, default = 0L),
      makeIntegerLearnerParam(id = "maxdepth", lower = 0L, default = 0L),
      makeLogicalLearnerParam(id = "savesplitstats", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors", "ordered", "weights", "missings", "featimp"),
    par.vals = list(),
    name = "Random Forest Based on Conditional Inference Trees",
    short.name = "cforest",
    note = "See `?ctree_control` for possible breakage for nominal features with missingness."
  )
}

#' @export
trainLearner.regr.cforest = function(.learner, .task, .subset, .weights = NULL,
                                     ntree, mtry, replace, fraction, trace,
                                     teststat, testtype, mincriterion,
                                     minsplit, minbucket, stump,
                                     nresample, maxsurrogate, maxdepth,
                                     savesplitstats,...) {
  f = getTaskFormula(.task)
  d = getTaskData(.task, .subset)
  defaults = getDefaults(getParamSet(.learner))
  if (missing(teststat)) teststat = defaults$teststat
  if (missing(testtype)) testtype = defaults$testtype
  if (missing(mincriterion)) mincriterion = defaults$mincriterion
  if (missing(replace)) replace = defaults$replace
  if (missing(fraction)) fraction = defaults$fraction
  ctrl = learnerArgsToControl(party::cforest_control, ntree, mtry, replace, fraction,
                              trace, teststat, testtype, mincriterion,
                              minsplit, minbucket, stump,
                              nresample, maxsurrogate, maxdepth, savesplitstats)
  party::cforest(f, data = d, controls = ctrl, weights = .weights, ...)
}

#' @export
predictLearner.regr.cforest = function(.learner, .model, .newdata, ...) {
  as.vector(predict(.model$learner.model, newdata = .newdata, ...))
}

#' @export
getFeatureImportanceLearner.regr.cforest = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.cforest(.learner, .model, auc = FALSE, ...)
}


###########
# mlr training
##########
# create mlr train and test task
trainTask = makeRegrTask(data = as.data.frame(train), target = "loss")
testTask = makeRegrTask(data = as.data.frame(test), target = "loss")

lrn = makeLearner("regr.cforest")

getParamSet(lrn)

## Train the learner
mod = train(lrn, trainTask)
mod



#####
train.ct <- ctree(loss ~ ., data = train, controls = cforest_unbiased(ntree=50, mtry=3))
#controls = cforest_unbiased
#control = cforest_unbiased(mtry = 2, ntree = 50)
#plot(train.ct)
varimp(train.ct)
