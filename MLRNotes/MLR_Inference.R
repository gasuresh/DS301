library(ISLR2)
fit = lm(medv~crim+lstat, data=Boston)
summary(fit)
names(fit)

## hypothesis test: 
#H0: beta1 = 3 vs. H1: beta1 /neq 3
#ts = 
ts =  -0.07045/0.03602
#null distribution = t distribution with 503 df
n = dim(Boston)[1]
p = 2
df = n - (p+1)
#p-value: 0.0511
#conclusion: Set alpha = 0.05. 
#Do not reject H0. There is not evidence
#that B1 is significantly difference from 0, 
#at alpha level 0.05. 

pt(abs(ts),df,lower.tail=FALSE)*2 # two-sided
pt(ts,df,lower.tail=FALSE) # one-sided 

# Confidence intervals for beta0, beta1, beta2
confint(fit,level=0.99)

# Confidence intervals for E(Y) = f(x)
predict(fit,data.frame(lstat=5,crim=0.5),interval='confidence',level=0.95)

# Prediction intervals for Y
predict(fit,data.frame(lstat=5,crim=0.5),interval='prediction',level = 0.95)

# estimate of sigma^2  
# sigma^2 represents the variance of Y
p = 2
sum(fit$residual^2)/(n-(p+1))



