# Read in files
ID = 'id'
TARGET = 'loss'

TRAIN_FILE = "Data/Raw/train.csv"
TEST_FILE = "Data/Raw/test.csv"
SUBMISSION_FILE = "Data/Raw/sample_submission.csv"

library(data.table) #fread
train = fread(TRAIN_FILE, showProgress = TRUE)
test = fread(TEST_FILE, showProgress = TRUE)



# Bin continuous variables
# cont2
library(Hmisc) #cut2
train$cont2_bin <- as.numeric(cut2(train$cont2, g=50))
test$cont2_bin <- as.numeric(cut2(test$cont2, g=50))



# remove skewness from continuous variables
# scale continuous variables
# remove skewness in train
library(MASS)
library(forecast)
library(caret) #BoxCoxTrans
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




# creating list of variable names
cat.var <- names(train)[which(sapply(train, is.character))]
test.cat.var <- names(test)[which(sapply(test, is.character))]


# Manually adjust factor levels
# manually adjust factors after reviewing boxplot
levels(train$cat98)[levels(train$cat98)=="D"] <- "A"
levels(train$cat109)[levels(train$cat109)=="BD"] <- "T"
levels(train$cat110)[levels(train$cat110)=="DJ"] <- "AR"


# Harmonize factors across entire data set
#set test loss to NA
test$loss <- NA
train$loss <- train$loss + 200 # Example
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


# OHE (one hot encoding) cat 80, 79, 12, 81, 100, 1, 10
library(dummies)
ohe_cats <- c("cat80", "cat79", "cat12", "cat81", "cat100", "cat1", "cat10")
fullSet <- dummy.data.frame(fullSet, names=ohe_cats, sep="_")


# Create sum of all cateogorical variables converted to their ASCI code
dt <- as.data.table(fullSet)
dt[, asciiSum := do.call(paste, mget(cat.var))] #get category variables in data table column
fullSet <- as.data.frame(dt)

#char to Raw function
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
# convert ascii values to numeric and sum into field
fullSet$value <- sapply(fullSet$asciiSum, function(a) paste(sum(as.numeric(charToRaw(a))), collapse = ' '))


