# For all those R users that want a competitive starter
# a shameless port of Faron's super python script to R
# https://www.kaggle.com/mmueller/allstate-claims-severity/yet-another-xgb-starter/code
# scores 1128 on public leaderboard but produced 1126 on my local run

library(data.table)
library(Matrix)
library(xgboost)
library(Metrics)
library(dplyr)
require("ggplot2")
require("Metrics")



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
test <- subset(test, select = -c(loss))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))
rm(fullSet)
train$loss <- train$loss + 200
#test$loss <- -99
train <- as.data.table(train)
test <- as.data.table(test)

y_train = log(train[,TARGET, with = FALSE])[[TARGET]]

train[, c(ID, TARGET) := NULL]
test[, c(ID) := NULL]

 ntrain = nrow(train)
  train_test = rbind(train, test)

 features = names(train)

 for (f in features) {
   if (class(train_test[[f]])=="factor") {
     cat("VARIABLE : ",f,"\n")
     levels <- unique(train_test[[f]])
     train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
   }
 }


x_train = train_test[1:ntrain,]
x_test = train_test[(ntrain+1):nrow(train_test),]


dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtest = xgb.DMatrix(as.matrix(x_test))


xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

ntrees <- 500

xgb_params = list(
  colsample_bytree = 0.6,
  subsample = 0.5, #
  eta = 10/ntrees, #2-10/ntrees
  alpha = 8,
  gamma = 1,
  lambda = 0.185,
  max_depth = 11,
  min_child_weight = 1.12,
  nthread = 4,
  eval_metric = 'mae' #evaluation metrics for validation data
)

set.seed(123)
xgb.model1 = xgb.cv(xgb_params,
                    dtrain,
                    objective = 'reg:linear',
                    nfold=3,
                    nrounds=ntrees,
                    early_stopping_rounds=10,
                    print_every_n = 10,
                    verbose= 1,
                    maximize=FALSE)

#xgb.model1$evaluation_log
xgb.model1_df <- as.data.frame(xgb.model1$evaluation_log)

xgb_params = list(
  colsample_bytree = 0.6,
  subsample = 0.75, #
  eta = 10/ntrees, #2-10/ntrees
  alpha = 8,
  gamma = 1,
  lambda = 0.185,
  max_depth = 11,
  min_child_weight = 1.12,
  nthread = 4,
  eval_metric = 'mae' #evaluation metrics for validation data
)

set.seed(123)
xgb.model2 = xgb.cv(xgb_params,
                    dtrain,
                    objective = 'reg:linear',
                    nfold=3,
                    nrounds=ntrees,
                    early_stopping_rounds=10,
                    print_every_n = 10,
                    verbose= 1,
                    maximize=FALSE)

#xgb.model2$evaluation_log
xgb.model2_df <- as.data.frame(xgb.model1$evaluation_log)

ggplot() + 
  geom_line(data=xgb.model1_df, aes(x = iter, y = test_mae_mean), color='green') + 
  geom_line(data=xgb.model2_df, aes(x = iter, y = test_mae_mean), color='red', linetype='dashed')

best_nrounds1 = xgb.model1$best_iteration
best_nrounds2 = xgb.model2$best_iteration
cv_mean1 = xgb.model1$evaluation_log$test_mae_mean[best_nrounds1]
cv_mean2 = xgb.model2$evaluation_log$test_mae_mean[best_nrounds2]
cv_std1 = xgb.model1$evaluation_log$test_mae_std[best_nrounds1]
cv_std2 = xgb.model2$evaluation_log$test_mae_std[best_nrounds2]
as.data.frame(xgb.model1$params)
cat(paste0('Model1 CV-Mean: ',cv_mean1,' '," CV-Std: ", cv_std1, " best rounds:", best_nrounds1))
as.data.frame(xgb.model2$params)
cat(paste0('Model2 CV-Mean: ',cv_mean2,' '," CV-Std: ", cv_std2, " best rounds:", best_nrounds2))



watchlist <- list(eval = dtest, train = dtrain)

gbdt = xgb.train(xgb_params, dtrain, best_nrounds2, watchlist)

importance_matrix <- xgb.importance(model = gbdt)
xgb.plot.importance(importance_matrix[1:20,])

submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = exp(predict(gbdt,dtest))-200
write.csv(submission,file = 'Submissions/xgb-mybasic-v2-11-29-16.csv',row.names = FALSE)

#1121.39082 on PLB