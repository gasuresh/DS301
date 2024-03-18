set.seed(1) # so we all get the same x values. 
n = 100
x = runif(n, min = 0, max = 2) 
error = rnorm(n,0,1)
y = 4 + x + x^2 + x^3 + error
train_set = data.frame(x,y)

M1 = lm(y~x,data=train_set)
M2 = lm(y~poly(x,degree=2),data=train_set)
M3 = lm(y~poly(x,degree=3),data=train_set)
M4 = lm(y~poly(x,degree=5),data=train_set)
M5 = lm(y~poly(x,degree=11),data=train_set)


