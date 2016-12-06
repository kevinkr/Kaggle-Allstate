# For all those R users that want a competitive starter
# a shameless port of Faron's super python script to R
# https://www.kaggle.com/mmueller/allstate-claims-severity/yet-another-xgb-starter/code
# scores 1128 on public leaderboard but produced 1126 on my local run

library(data.table)
#library(Matrix)
library(xgboost)
#library(Metrics)
library(dplyr)
library(caret)
library(e1071)
library(MASS)
#library(forecast)
#library(scales)
#library(Hmisc)
#library(stringer)


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
test$loss <- NA
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
rm(fullSet)

trainIndex <- createDataPartition(train$loss, p = .7,list = FALSE,times = 1)
trainMain=train[trainIndex,]
trainTest=train[-trainIndex,]

trainTestIndex <- createDataPartition(trainTest$loss, p = .6,list = FALSE,times = 1)
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

rm(train,test,train_test,trainIndex,trainTestIndex,x_test,x_train)
gc()

# fair objective 2 for XGBoost

amo.fairobj2 <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  con <- 2
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  hess <- con ^ 2 / (abs(x) + con) ^ 2
  
  return(list(grad = grad, hess = hess))
  
}

# MAE Metric for XGBoost
# 
# amm_mae <- function(preds
#                     , dtrain) {
#   
#   labels <- xgboost::getinfo(dtrain, "label")
#   elab <- as.numeric(labels)
#   epreds <- as.numeric(preds)
#   err <- mae(elab, epreds)
#   
#   return(list(metric = "amm_mae", value = err))
#   
# }

amm_mae <- function(preds , dtrain) {
  
  labels <- xgboost::getinfo(dtrain, "label") 
  elab <- as.numeric(labels) 
  epreds <- as.numeric(preds) 
  err <- mae(exp(elab)+200, exp(epreds)+200)
  return(list(metric = "amm_mae", value = round(err,4)))
  
}

tuner_mae = data.frame("Rounds" = numeric(), 
                       "Depth" = numeric(),
                       "r_sample" = numeric(),
                       "c_sample" = numeric(), 
                       "minMAE:Test" = numeric(),
                       "best_round" = numeric(),
                       "eta" = numeric(),
                       "depth" = numeric(),
                       "min child weight" = numeric()
)

xgb_params = list(
  gamma = 0,
  nthread = 4,
  num_parallel_tree = 2,
  objective = amo.fairobj2
)

min_cw = 50

for (rounds in seq(100, 1000, 100)){
  
  for (depth in c(4, 6, 8, 10, 12, 14)) {
    
    for (r_sample in c(0.8)) {
      
      for (c_sample in c(0.8)) {
        
        for (eta_val in c(0.3)) {
        
          set.seed(1024)
          
          cv.res = xgb.cv(data = dtrain, 
                          nfold = 3, 
                          nrounds = rounds, 
                          eta = eta_val, 
                          max_depth = depth,
                          subsample = r_sample,
                          min_child_weight = min_cw,
                          colsample_bytree = c_sample,
                          early_stopping_rounds = 25,
                          print_every_n = 20,
                          #eval_metric = 'mae',
                          feval = amm_mae,
                          verbose = FALSE,
                          maximize=FALSE)
          
          cv.res.log <- as.data.frame(cv.res$evaluation_log)
          
          
          print(paste(rounds, depth, r_sample, c_sample, round(min(cv.res.log[,4]),4) ))
          tuner_mae[nrow(tuner_mae)+1, ] = c(rounds, 
                                               depth, 
                                               r_sample, 
                                               c_sample, 
                                               min(cv.res.log[,4]), 
                                               which.min(cv.res.log[,4]),
                                               eta_val,
                                               depth,
                                               min_cw)
          gc()
        }
      }
    }
  }
}























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
