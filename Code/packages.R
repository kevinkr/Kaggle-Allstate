# Allstate Kaggle competition
# Start: 10-13-16

#install packages
# install.packages("ggplot2", dependencies = TRUE)
# install.packages("Amelia", dependencies = TRUE)
# install.packages("dplyr", dependencies = TRUE)
# install.packages("data.table", dependencies = TRUE)
# install.packages("gridExtra", dependencies = TRUE)
# install.packages("e1071", dependencies = TRUE)
# install.packages("vcd", dependencies = TRUE)
# install.packages("FactoMineR", dependencies = TRUE)
# install.packages("corrplot", dependencies = TRUE)
# install.packages("psych", dependencies = TRUE)
# install.packages("glmnet", dependencies = TRUE)
# install.packages("caret", dependencies = TRUE)
# install.packages("TDboost", dependencies = TRUE)
# install.packages("HDtweedie", dependencies = TRUE)
#install.packages("factoextra", dependencies = TRUE)

library("Amelia")
library(randomForest)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(e1071)
library(vcd)
library(FactoMineR)
library(corrplot)
library(psych)
library(factoextra)
library(glmnet)
library(caret)
library(data.table)
library("dplyr")
library(TDboost)
library(HDtweedie)

#parallel computing
#install.packages("doMC", dependencies = TRUE)
#require(doMC)

