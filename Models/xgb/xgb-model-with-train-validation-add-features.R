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
library(caret)

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
train$loss <- train$loss + 200

test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))

#bind train and test
fullSet <- rbind(test,train)
# set factor levels all full set
fullSet <- fullSet %>% mutate_each(funs(factor), starts_with("cat"))

features = names(fullSet)

for (f in features) {
  if (class(fullSet[[f]])=="factor") {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(fullSet[[f]])
    fullSet[[f]] <- as.integer(factor(fullSet[[f]], levels=levels))
  }
}

test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))

train.lin.reg <-lm(log(loss) ~ ., data=train)

train.cont.lin.reg <- lm(log(loss) ~ cont1 + cont2 + cont4 + cont7
                         + cont8 + cont9 + cont10 + cont11 + cont12 + cont13 + cont14, data=train)

test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))
test$loss <- -99
#bind train and test
fullSet <- rbind(test,train)
# set factor levels all full set
#fullSet <- fullSet %>% mutate_each(funs(factor), starts_with("cat"))
# drop vars
#fullSet <- subset(fullSet, select = -c(cont3, cont5, cont6))
fullSet$linearPred <- predict(train.lin.reg,fullSet)
fullSet$contlinearPred <- predict(train.cont.lin.reg,fullSet)
# split back into test and train
test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
# drop loss from test set
test <- subset(test, select = -c(loss))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))
rm(fullSet)


trainIndex <- createDataPartition(train$loss, p = .8,list = FALSE,times = 1)
trainMain=train[trainIndex,]
trainTest=train[-trainIndex,]

trainTestIndex <- createDataPartition(trainTest$loss, p = .7,list = FALSE,times = 1)
trainTestMain <- trainTest[trainTestIndex,]
trainTestCV <- trainTest[-trainTestIndex,]

train <- as.data.table(trainMain)
trainTest <- as.data.table(trainTestMain)
trainTestCV <- as.data.table(trainTestCV)

test <- as.data.table(test)

y_train = log(train[,TARGET, with = FALSE])[[TARGET]]
y_trainTest = log(trainTest[,TARGET, with = FALSE])[[TARGET]]
y_trainTestCV = log(trainTestCV[,TARGET, with = FALSE])[[TARGET]]

train[, c(ID, TARGET) := NULL]
trainTest[, c(ID, TARGET) := NULL]
trainTestCVid <- trainTestCV$id
trainTestCV[, c(ID, TARGET) := NULL]

test[, c(ID) := NULL]

 ntrain = nrow(train)
  train_test = rbind(train, test)

 features = names(train)

 for (f in features) {
   if (class(train_test[[f]])=="factor") {
     #cat("VARIABLE : ",f,"\n")
     levels <- unique(train_test[[f]])
     train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
   }
 }
 
 for (f in features) {
   if (class(trainTest[[f]])=="factor") {
     #cat("VARIABLE : ",f,"\n")
     levels <- unique(trainTest[[f]])
     trainTest[[f]] <- as.integer(factor(trainTest[[f]], levels=levels))
   }
 }
 
 for (f in features) {
   if (class(trainTestCV[[f]])=="factor") {
     #cat("VARIABLE : ",f,"\n")
     levels <- unique(trainTestCV[[f]])
     trainTestCV[[f]] <- as.integer(factor(trainTestCV[[f]], levels=levels))
   }
 }


x_train = train_test[1:ntrain,]
x_test = train_test[(ntrain+1):nrow(train_test),]


dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtrainTest = xgb.DMatrix(as.matrix(trainTest), label=y_trainTest)
dtrainTestCV = xgb.DMatrix(as.matrix(trainTestCV), label=y_trainTestCV)
dtest = xgb.DMatrix(as.matrix(x_test))

ntrees <- 1000

xgb_params = list(
  colsample_bytree = 0.6,
  subsample = .75, #.8 1821 .6 1822 .4 1821
  eta = .0075, #0.01
  alpha = 8,
  gamma = 1,
  lambda = .185,
  max_depth = 12, #10-12,1832 14,1833 8,1832 4,1817 15,1833
  min_child_weight = 1.12, #4-5,10  1817
  nthread = 4,
  num_parallel_tree = 2,
  eval_metric = 'mae' #evaluation metrics for validation data, 
)

set.seed(123)

watchlist <- list('valid' = dtrainTest, 'train' = dtrain)

gbdt = xgb.train(xgb_params, 
                 dtrain, 
                 nrounds=1000, 
                 watchlist, 
                 maximize=FALSE, 
                 early_stopping_rounds=10,
                 print_every_n = 20)

gbdt_df <- as.data.frame(gbdt$evaluation_log)

ggplot() + 
  geom_line(data=gbdt_df, aes(x = iter, y = valid_mae), color='green') +
  geom_line(data=gbdt_df, aes(x = iter, y = train_mae), color='red')

dtrainTestCVpred <- exp(predict(gbdt,dtrainTestCV))-200
#dtrainTestCVpred <- predict(gbdt,dtrainTestCV)
cvpred <- as.data.frame(dtrainTestCVpred)
cvpred <- as.data.frame(cbind(trainTestCVid,dtrainTestCVpred,y_trainTestCV))
cvpred$y_trainTestCV <- exp(cvpred$y_trainTestCV)-200
#cvpred$y_trainTestCV <- cvpred$y_trainTestCV
cvpred$error <-  cvpred$y_trainTestCV - cvpred$dtrainTestCVpred
mae_val <- mae(cvpred$y_trainTestCV, cvpred$dtrainTestCVpred)

best_nrounds = gbdt$best_iteration
cv_mae = gbdt$evaluation_log$valid_mae[best_nrounds]
train_mae = gbdt$evaluation_log$train_mae[best_nrounds]
as.data.frame(gbdt$params)
cat(paste0('Model CV-MAE: ',cv_mae,' '," Train-MAE: ", train_mae, " best rounds:", best_nrounds, "   MAE: ", mae_val))

ggplot() +
     coord_cartesian(ylim = c(0, 12000)) +
     geom_point(data=cvpred, aes(x = row.names(cvpred), y = y_trainTestCV), color='green') +
     geom_point(data=cvpred, aes(x = row.names(cvpred), y = dtrainTestCVpred), color='red') +
     geom_hline(yintercept=mae_val, color='black', size=1) +
     geom_hline(yintercept=1098, color='blue', linetype=3, size = 1) +
     ggtitle("Predicted vs. Expected Loss on Holdout Set") +
     labs(x="row",y="Loss") +
     theme(plot.title = element_text(family = "Arial", color="#666666", face="bold", size=16, hjust=0)) +
     theme(axis.title = element_text(family = "Arial", color="#666666", face="bold", size=16)) 

apply(cvpred,2, sum)

xgb.model1 = xgb.cv(xgb_params,
                    dtrain,
                    objective = 'reg:linear',
                    nfold=10,
                    nrounds=ntrees,
                    early_stopping_rounds=25,
                    print_every_n = 20,
                    verbose= 1,
                    maximize=FALSE)

#xgb.model1$evaluation_log
#xgb.model1_df <- as.data.frame(xgb.model1$evaluation_log)

best_nrounds1 = xgb.model1$best_iteration
cv_mean1 = xgb.model1$evaluation_log$test_mae_mean[best_nrounds1]
cv_std1 = xgb.model1$evaluation_log$test_mae_std[best_nrounds1]
as.data.frame(xgb.model1$params)
cat(paste0('Model1 CV-Mean: ',cv_mean1,' '," CV-Std: ", cv_std1, " best rounds:", best_nrounds1))



importance_matrix <- xgb.importance(feature_names = names(train), model = gbdt)
xgb.plot.importance(importance_matrix[1:20,])

submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = exp(predict(gbdt,dtest))-200
write.csv(submission,file = 'Submissions/xgb-mybasic-v4-12-2-16.csv',row.names = FALSE)
