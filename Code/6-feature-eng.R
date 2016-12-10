# Feature Engineering

library(data.table)
#library(Matrix)
library(xgboost)
#library(Metrics)
library(dplyr)
library(caret)
library(e1071)
library(MASS)
library(forecast)
#library(scales)
library(Hmisc)
#library(stringer)

ID = 'id'
TARGET = 'loss'

TRAIN_FILE = "Data/Raw/train.csv"
TEST_FILE = "Data/Raw/test.csv"
SUBMISSION_FILE = "Data/Raw/sample_submission.csv"


train = fread(TRAIN_FILE, showProgress = TRUE)
test = fread(TEST_FILE, showProgress = TRUE)

# Bin continuous variables
# cont2
train$cont2_bin <- as.numeric(cut2(train$cont2, g=50))
test$cont2_bin <- as.numeric(cut2(test$cont2, g=50))
                             
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

######
# Feature: [binary] If any column in row contains 'OTHER', sum the vales *DONE*
fullSet$hasOther <- 0
#This code counts the number of occurences of  OTHER by column. True values are coded as 1, 
# so summing all the values gives us the number of occurences of OTHER
fullSet$hasOther <- rowSums(fullSet=="OTHER")

#ASCII sum
dt <- as.data.table(fullSet)
dt[, asciiSum := do.call(paste, mget(cat.var))]
fullSet <- as.data.frame(dt)

ord <- function(x) {
  s <- x[1]
  
  intbits <- .Machine$sizeof.long * 4
  bits <- raw(nchar(s) * intbits)
  
  r <- charToRaw(x)
  idx <- if (.Platform$endian == "little") 1:8 else 25:32
  for (i in 1:nchar(s)) {
    bits[idx + ((i - 1) * intbits)] <- rawToBits(r[i])
  }
  packBits(bits, 'integer')
}

fullSet$value <- sapply(fullSet$asciiSum, function(a) paste(sum(as.numeric(charToRaw(a))), collapse = ' '))


# Create columns with counts of all categorical values *NOT DONE*
# is.fact <- sapply(fullSet, is.factor)
# factors.df <- fullSet[, is.fact]
# 
# unique(factors.df)
# 
# DT <- data.table(factors.df)
# 
# sapply(factors.df ,function(x) length(unique(x))) 
# 
# rapply(fullSet, class = "factor", f = levels, how = "list")

# OHE cat 80, 79, 12, 81, 100, 1, 10   *DONE*
library(dummies)
ohe_cats <- c("cat80", "cat79", "cat12", "cat81", "cat100", "cat1", "cat10")
fullSet <- dummy.data.frame(fullSet, names=ohe_cats, sep="_")




# split back into test and train
test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
# drop loss from test set
test <- subset(test, select = -c(loss))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))
rm(fullSet)
