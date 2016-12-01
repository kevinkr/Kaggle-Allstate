# Kaggle Allstate 11-28-16 my version of XGboost
# xgboost install from repo install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")


library(data.table)
library(Matrix)
require(xgboost)
library(Metrics)

  ID = 'id'
  TARGET = 'loss'
  SEED = 0
  
  TRAIN_FILE = "Data/Raw/train.csv"
  TEST_FILE = "Data/Raw/test.csv"
  SUBMISSION_FILE = "Data/Raw/sample_submission.csv"
  
  
  train = fread(TRAIN_FILE, showProgress = TRUE)
  test = fread(TEST_FILE, showProgress = TRUE)
  
  train$loss <- train$loss + 200
  
  y_train = log(train[,TARGET, with = FALSE])[[TARGET]]
  
  train[, c(ID, TARGET) := NULL]
  test[, c(ID) := NULL]
  
  ntrain = nrow(train)
  train_test = rbind(train, test)
  
  features = names(train)
  
  for (f in features) {
    if (class(train_test[[f]])=="character") {
      #cat("VARIABLE : ",f,"\n")
      levels <- unique(train_test[[f]])
      train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
    }
  }
  
  
  x_train = train_test[1:ntrain,]
  x_test = train_test[(ntrain+1):nrow(train_test),]
  
  rm(train_test, test, train)
  
  dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
  dtest = xgb.DMatrix(as.matrix(x_test))


xgb_params = list(
  colsample_bytree = 0.6,
  subsample = 0.892,
  eta = 0.056,
  alpha = 8,
  gamma = 1,
  lambda = 0.185,
  objective = 'reg:linear',
  max_depth = 11,
  num_parallel_tree = 1,
  min_child_weight = 1.12,
  base_score = 7,
  eval_metric = "mae"
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

# searchGridSubCol <- expand.grid(subsample = c(0.5, 0.75), 
#                                 colsample_bytree = c(0.6, 0.8))
# ntrees <- 10
# 
# rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
#   
#   #Extract Parameters to test
#   currentSubsampleRate <- parameterList[["subsample"]]
#   currentColsampleRate <- parameterList[["colsample_bytree"]]
#   
#   xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = ntrees, nfold = 3, showsd = TRUE, 
#                            metrics = "mae", verbose = TRUE, "eval_metric" = "mae",
#                            "objective" = "reg:linear", "max.depth" = 6, "eta" = 2/ntrees,                               
#                            "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
#   
#   xvalidationScores <- xgboostModelCV
#   #Save rmse of the last iteration
#   rmse <- tail(xvalidationScores$test.rmse.mean, 1)
#   
#   return(c(rmse, currentSubsampleRate, currentColsampleRate))
#   
# })


set.seed(123)
res = xgb.cv(xgb_params,
             dtrain,
             nrounds=20,
             nfold=4,
             early_stopping_rounds=10,
             print_every_n = 10,
             verbose= 1,
             #feval=xg_eval_mae,
             maximize=FALSE)

res

best_nrounds = res$best_iteration
cv_mean = res$evaluation_log$test_mae_mean[best_nrounds]
cv_std = res$evaluation_log$test_mae_std[best_nrounds]
cat(paste0('CV-Mean: ',cv_mean,' ', cv_std))

gbdt = xgb.train(xgb_params, dtrain, best_nrounds)

importance_matrix <- xgb.importance(model = gbdt)
xgb.plot.importance(importance_matrix[1:20,])

submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$loss = exp(predict(gbdt,dtest))-200
write.csv(submission,file = 'Submissions/xgb-mybasic-v1-11-28-16.csv',row.names = FALSE)

#1121.39082 on PLB