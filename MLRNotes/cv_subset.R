
########################################################
###### Combining CV with 'best' subset selection #######
########################################################
library(ISLR2)
head(Hitters)
Hitters = na.omit(Hitters)
dim(Hitters)

## Validation set approach: 
## Use training set to perform all aspects of model-fitting, including model selection
## If we use the full dataset to carry out model selection, 
# we are essentially cheating (seeing the data twice) 
# and the test errors will be overly optimistic 

n = dim(Hitters)[1]

set.seed(10)
train_index = sample(1:n,n/2,rep=FALSE)

train = Hitters[train_index,]
test = Hitters[-train_index,]

library(leaps)
best.train = regsubsets(Salary~.,data=train,nbest=1,nvmax=19)

## the predict() function does not work on regsubsets objects, 
# so we need to write our own code to obtain our predicted values:

val.errors = rep(NA,19)
for(i in 1:19){
  test.mat = model.matrix(Salary~.,data=test)
  
  coef.m = coef(best.train,id=i)
  
  pred = test.mat[,names(coef.m)]%*%coef.m
  val.errors[i] = mean((test$Salary-pred)^2)
}


#########################################################
## We can extend this to k-fold CV. 
# This approach is somewhat involved because we must perform 
# best subset selection within each of the k training sets. 

## This approach is often used to determine model SIZE, 
# not which predictors will end up in the model. 
# Once we have determined the optimal model SIZE, 
# best subset selection is then performed on the full data set
# and the best model of size ____ is chosen. 

########################
## In-class Activity  ##
########################

## Adapt the code above to perform 10-fold CV using subset selection.
## Your code should involve TWO loops: 
## (1) one outer loop to iterate through the k-folds
## (2) one inner loop to iterate throuhgh the 19 candidate models selected by subset selection
## Report the 10-fold CV error for each model size (19 in total). 
## What is the optimal model size based on 10-fold CV? Report that in your solutions. 

## Work in groups to come up with a solution. 
## Copy and paste any relevant code on Ed Discussion. 
## Please be sure to list all your group members names. 
## Only one group member needs to post on Ed Discussion.

library(caret)

folds <- createFolds(Hitters$Salary, k = 10)

val.errors = matrix(NA, nrow = 10, ncol = 19)

for (i in 1:10)
{
  test = Hitters[folds == i, ]
  train = Hitters[folds != i, ]
  best.fit = regsubsets(Salary~.,data=train,nbest=1,nvmax=19)
  
  for (j in 1:19)
  {
    test.mat = model.matrix(Salary~ ., data= test)
    
    coef.m = coef(best.train, id = j)
    pred = test.mat[, names(coef.m)] %*% coef.m
    val.errors[i, j] = mean((test$Salary - pred)^2)
  }
}




