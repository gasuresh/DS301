library(MASS)

m1 = lm(medv~lstat,data=Boston)
m5 = lm(medv~poly(lstat,5,raw=TRUE),data=Boston)
m9 = lm(medv~poly(lstat,9,raw=TRUE),data=Boston)

par(mfrow=c(1,1))
plot(medv~lstat,data=Boston)
points(Boston$lstat,m1$fitted.values,col='red')
## m1$fitted values are the predicted values we obtain from the model m1

plot(medv~lstat,data=Boston)
points(Boston$lstat,m5$fitted.values,col='green')
## m5$fitted values are the predicted values we obtain from the model m5

plot(medv~lstat,data=Boston)
points(Boston$lstat,m9$fitted.values,col='blue')
## m9$fitted values are the predicted values we obtain from the model m9

set.seed(13)
n = dim(Boston)[1]
train_index = sample(1:n,n/2,replace=F)
train_boston = Boston[train_index,]
test_boston = Boston[-train_index,]

M1 = lm(medv~lstat,data=train_boston)
M5 = lm(medv~poly(lstat,5,raw=TRUE),data=train_boston)
M9 = lm(medv~poly(lstat,9,raw=TRUE),data=train_boston)

## training MSE from M1
mean((train_boston$medv - M1$fitted.values)^2)

## predicted values from the test set
Yhat_test = predict(M1,newdata=test_boston)
## how do we compute the test MSE from M1? 

###########################
#### In-class Activity ####
###########################

#### fit 9 models of increasing complexity on the training set: 
## M1 = lm(medv~poly(lstat,1,raw=TRUE),data=train_boston)
## M2 = lm(medv~poly(lstat,2,raw=TRUE),data=train_boston)
## M3 = lm(medv~poly(lstat,3,raw=TRUE),data=train_boston)
## ..
## ..
## M9 = lm(medv~poly(lstat,9,raw=TRUE),data=train_boston)

## obtain the training MSE and test MSE for each of these models. 
## Create a plot where the training MSE is on the y-axis and the 
## model complexity (1 - 9) is on the x-axis. 
## Create a similar plot for the test MSE. 

## Work in groups to come up with a collective solution. 
## Copy/paste your code and plots into Ed Discussion 
## Please make sure you set.seed(13) so that we all get comparable results!
## Please be sure to list all your group members names so that everyone gets participation credit. 
## Only one group member needs to post on Ed Discussion.


library(ggplot2)

# Create Data Frame to store M's
ms = data.frame(M=numeric(), MSE=numeric())

# Loop through
for(i in 1:9) {
  m = lm(medv~poly(lstat,i,raw=TRUE),data=train_boston)
  res = mean((train_boston$medv - m$fitted.values)^2)
  ms = rbind(ms, data.frame(M=i, MSE=res))
}

# ggplot to print
ggplot(ms, aes(x=M, y=MSE)) +
  geom_point()
