# For all those R users that want a competitive starter
# a shameless port of Faron's super python script to R
# https://www.kaggle.com/mmueller/allstate-claims-severity/yet-another-xgb-starter/code
# scores 1128 on public leaderboard but produced 1126 on my local run

library(data.table)
library(Matrix)
library(xgboost)
library(Metrics)
library(dplyr)

ID = 'id'
TARGET = 'loss'

TRAIN_FILE = "Data/Raw/train.csv"
TEST_FILE = "Data/Raw/test.csv"
SUBMISSION_FILE = "Data/Raw/sample_submission.csv"


train = fread(TRAIN_FILE, showProgress = TRUE)
test = fread(TEST_FILE, showProgress = TRUE)

# Harmonize factors
#set test loss to NA
test$loss <- NA
test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))
#bind train and test
fullSet <- rbind(test,train)
# set factor levels all full set
fullSet <- fullSet %>% mutate_each(funs(factor), starts_with("cat"))
# drop vars
fullSet <- subset(fullSet, select = -c(cont3, cont5, cont6))
fullSet <- subset(fullSet, select = -c(cat7 , cat14 , cat15 , cat16 , cat17 , cat18 , cat19 , cat20 , cat21 , cat22 , cat24 , cat28 , cat29 , cat30 , cat31 , cat32 , cat33 , cat34 , cat35 , cat39 , cat40 , cat41 , cat42 , cat43 , cat45 , cat46 , cat47 , cat48 , cat49 , cat51 , cat52 , cat54 , cat55 , cat56 , cat57 , cat58 , cat59 , cat60 , cat61 , cat62 , cat63 , cat64 , cat65 , cat66 , cat67 , cat68 , cat69 , cat70 , cat74 , cat76 , cat77 , cat78 , cat85 , cat89 , cat96 , cat102))
fullSet.cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
#ohe
library(dummies)
#train <- dummy.data.frame(train, names=cat.var, sep="_")
#test <- dummy.data.frame(test, names=test.cat.var,sep="_")
fullSet <- dummy.data.frame(fullSet, names=fullSet.cat.var, sep="_")

# split back into test and train
test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
# drop loss from test set
test <- subset(test, select = -c(loss))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))
rm(fullSet)
train <- as.data.table(train)
test <- as.data.table(test)

y_train = log(train[,TARGET, with = FALSE])[[TARGET]]

train[, c(ID, TARGET) := NULL]
test[, c(ID) := NULL]

# ntrain = nrow(train)
# train_test = rbind(train, test)

# features = names(train)

# for (f in features) {
#   if (class(train_test[[f]])=="character") {
#     #cat("VARIABLE : ",f,"\n")
#     levels <- unique(train_test[[f]])
#     train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
#   }
# }


#x_train = train_test[1:ntrain,]
#x_test = train_test[(ntrain+1):nrow(train_test),]


dtrain = xgb.DMatrix(as.matrix(train), label=y_train)
dtest = xgb.DMatrix(as.matrix(test))


xgb_params = list(
  colsample_bytree = 0.5,
  subsample = 0.7,
  eta = 0.01,
  alpha = 1,
  gamma = 1,
  objective = 'reg:linear',
  max_depth = 12,
  num_parallel_tree = 1,
  min_child_weight = 1,
  base_score = 7
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

set.seed(123)
res = xgb.cv(xgb_params,
             dtrain,
             nrounds=5000,
             nfold=4,
             early_stopping_rounds=10,
             print_every_n = 10,
             verbose= 1,
             feval=xg_eval_mae,
             maximize=FALSE)

best_nrounds = res$best_iteration
cv_mean = res$evaluation_log$test_error_mean[best_nrounds]
cv_std = res$evaluation_log$test_error_std[best_nrounds]
cat(paste0('CV-Mean: ',cv_mean,' ', cv_std))

gbdt = xgb.train(xgb_params, dtrain, best_nrounds)

submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = exp(predict(gbdt,dtest))
write.csv(submission,file = 'Submissions/xgb-faron-starter-v4-111816.csv',row.names = FALSE)

#1126.19439 on PLB