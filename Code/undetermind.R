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






### Misfit code

######################################################################
# Not useful in this case with so many variables
# correspondence (see http://gastonsanchez.com/how-to/2012/10/13/MCA-in-R/)
cats = apply(train.cat.factored[,110:116], 2, function(x) nlevels(as.factor(x)))
mca1 = MCA(train.cat.factored[,110:116], graph = FALSE)

# data frame with variable coordinates
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")
#####################################################################
