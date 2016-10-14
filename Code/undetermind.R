# Allstate Kaggle competition
# Start: 10-13-16

source("packages.R")




#############################
# Merge data sets
#############################
#
#Create dummy variable in test
test<- mutate(test, loss = "none")

#Create sorting variable dataset before combining
test <- mutate(test, dataset = "testset")
train <- mutate(train, dataset = "trainset")

# factorize categoricals
data <- data %>% mutate_each(funs(factor), starts_with("cat"))
