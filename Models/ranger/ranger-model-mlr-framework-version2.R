library(mlr)
library(mlbench)
library(dplyr)
library(data.table)
library(ranger)

options(scipen=999) # remove scientific notation

#Load data
train <- fread("Data/Raw/train.csv", stringsAsFactors=FALSE, header = TRUE)
test <- fread("Data/Raw/test.csv", stringsAsFactors=FALSE, header = TRUE)

# Drop cont columns 1, 6, 10, 11
#train <- subset(train, select =-c(cont1,cont6,cont11,cont10))
#test <- subset(test, select =-c(cont1,cont6,cont11,cont10))
# Harmonize factors
#set test loss to NA
test$loss <- NA
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
rm(fullSet)

cat.var <- names(train)[which(sapply(train, is.factor))]
test.cat.var <- names(test)[which(sapply(test, is.factor))]

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

# Harmonize factors
#set test loss to NA
test$loss <- NA
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

rm(fullSet)

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
test$loss <- NA
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

train$loss <- log(train$loss + 200)
test$loss <- -99
rm(fullSet)

##########################################


makeRLearner.regr.ranger = function() {
  makeRLearnerRegr(
    cl = "regr.ranger",
    package = "ranger",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
      # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars))
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "min.node.size", lower = 1L, default = 5L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeNumericLearnerParam(id = "sample.fraction", lower = 0L, upper = 1L),
      makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "always.split.variables"),
      makeLogicalLearnerParam(id = "respect.unordered.factors", default = TRUE),
      makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), default = "none", tunable = FALSE),
      makeLogicalLearnerParam(id = "write.forest", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE, requires = quote(importance == "permutation"), tunable = FALSE),
      makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both", tunable = FALSE),
      makeLogicalLearnerParam(id = "save.memory", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both", tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", when = "both", tunable = FALSE),
      makeDiscreteLearnerParam(id = "splitrule", values = c("variance", "maxstat"), default = "variance"),
      makeNumericLearnerParam(id = "alpha", lower = 0L, upper = 1L, default = 0.5, requires = quote(splitrule == "maxstat")),
      makeNumericLearnerParam(id = "minprop", lower = 0L, upper = 1L, default = 0.1, requires = quote(splitrule == "maxstat")),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = TRUE),
    properties = c("numerics", "factors", "ordered", "featimp"),
    name = "Random Forests",
    short.name = "ranger",
    note = "By default, internal parallelization is switched off (`num.threads = 1`), `verbose` output is disabled, `respect.unordered.factors` is set to `TRUE`. All settings are changeable."
  )
}

#' @export
trainLearner.regr.ranger = function(.learner, .task, .subset, .weights, ...) {
  tn = getTaskTargetNames(.task)
  ranger::ranger(formula = NULL, dependent.variable = tn, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.ranger = function(.learner, .model, .newdata, ...) {
  p = predict(object = .model$learner.model, data = .newdata, ...)
  return(p$predictions)
}

#' @export
getFeatureImportanceLearner.regr.ranger = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.ranger(.learner, .model, ...)
}

# create mlr train and test task
trainTask = makeRegrTask(data = as.data.frame(train), target = "loss")
testTask = makeRegrTask(data = as.data.frame(test), target = "loss")

# specify mlr learner with some nice hyperpars
set.seed(123)
lrn = makeLearner("regr.ranger")
lrn = setHyperPars(lrn, 
                   num.trees = 200,
                   min.node.size = 5,
                   respect.unordered.factors = TRUE,
                   verbose = TRUE,
                   mtry = 5,
                   importance = "impurity"
)

# 1) make parameter set
ps = makeParamSet(
  #makeIntegerParam("num.trees", lower = 5, upper = 200),
  makeDiscreteParam("num.trees", values = c(200, 250, 500, 750, 1000)),
  makeLogicalParam("respect.unordered.factors", TRUE),
  makeDiscreteParam("importance", "impurity"),
  makeIntegerParam("mtry", lower = 1, upper = 10)
)

# 2) Use 3-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 5L)

# 3) Here we use random search (with 5 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 5)

# 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters in parallel
#parallelStartMulticore(5)
#res = tuneParams(lrn, task = trainTask, resampling = rdesc,
#                 par.set = ps, control = ctrl)
res = tuneParams(lrn, task = trainTask, resampling = rdesc,
                 par.set = ps, control = makeTuneControlGrid())

res

# Train on entire dataset (using best hyperparameters)
lrn = setHyperPars(lrn, par.vals = res$x)
mod = train(lrn, trainTask)

task.pred = predict(mod, task = trainTask, subset = test.set)
task.pred



# 6) make Prediction
pred = exp(getPredictionResponse(predict(mod, testTask))) - 200
summary(pred)
SUBMISSION_FILE = "Data/Raw/sample_submission.csv"
submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = pred
write.csv(submission, "Submissions/mlr-ranger-framework-v4-11-30-16.csv", row.names = FALSE)
