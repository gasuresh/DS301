## Forward and Backward Stepwise Selection

library(ISLR2)
Hitters = na.omit(Hitters)

library(leaps)

# we can also use regsubsets() to perform backward/forward stepwise selection 

regfit.fwd = regsubsets(Salary~.,data=Hitters,nvmax=19, nbest = 1, method="forward")
regfit.bwd = regsubsets(Salary~.,data=Hitters,nvmax=19, method="backward")

regfit.fwd.sum = summary(regfit.fwd)
names(regfit.fwd.sum)
n = dim(Hitters)[1]
p = rowSums(regfit.fwd.sum$which) #number of predictors + intercept in the model 
adjr2 = regfit.fwd.sum$adjr2
cp = regfit.fwd.sum$cp
rss = regfit.fwd.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)

plot(AIC,type='b')
plot(BIC,type='b')
plot(cp,type='b')
plot(adjr2,type='b')

