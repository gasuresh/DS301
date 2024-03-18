#########################################################
######### Accuracy of our prediction? ################### 

patient = read.table("patient.txt",header=FALSE)
names(patient) = c("satisf","age","severe","anxiety")

## divide our data into a training set and a test set
n = dim(patient)[1]
set.seed(100)
train_index = sample(1:n,n/2,rep=FALSE)

train_patients = patient[train_index,]
test_patients = patient[-train_index,]

model_train = lm(satisf~age+severe+anxiety,data=train_patients)
MSE_train = mean((train_patients$satisf - model_train$fitted.values)^2) 
MSE_train

predicted_values = predict(model_train,test_patients)
MSE_test = mean((test_patients$satisf - predicted_values)^2)
MSE_test

# will the training MSE always be smaller than the test MSE?


#########################################################
####### Properties of our linear regression model #######
#########################################################

## let's simulate data where we know the true population 
## regression line

## Simple linear regression 
## suppose we have 1 single predictor (X1)
n = 100
beta_0 = 
beta_1 = 
X1 = seq(0,10, length.out=n)

## Generate (or sample) Y based on true population regression 
## line
error = rnorm(n,0,1) ## generate 100 error values from 
                    ## normal distribution with mean 0 and sd 1.
Y = beta_0 + beta_1*X1 + error 

length(error)
length(Y)

## Then we use this sample to estimate the least square line. 
## If we were to take a different sample of Y, we would get a different least square line
## and different estimates for beta0 and beta1. Re-run lines 42 - 53 many times to see that the
## estimate coefficients would change. 
lm(Y~X1)

## We hope that over many many many samples, on average, our estimates 
## would exactly equal the truth. 
## This is the idea of an unbiased estimator.
## How can we check this using simulations?  

########################
## In-class Activity  ##
########################

## 1. Design a simulation to check whether or not our least square estimates for beta_0 and beta_1 are unbiased. 

## 2. After finishing HW 1, explain why using many simple linear regression models is not sufficient 
## compared to a multiple linear regression model.
## Note that fitting a simple linear regression model is computationally
## instantaneous since there are analytical solutions for our least square estimates. 

## Work in groups to come up with a solution. 
## Copy and paste any relevant code on Ed Discussion. 
## Please be sure to list all your group members names. Only one group member needs to post on Ed Discussion. 
## link: https://edstem.org/us/courses/50620/discussion/

n = 100
beta_0 = 7
beta_1 = 12
X1 = seq(0, 10, length.out = n)
B = 5000

b0 = b1 = rep(NA, B)

for (i in 1:B)
{
  error = rnorm(n, 0, 1)
  Y = beta_0 + beta_1*X1 + error
  m1 = lm(Y ~ X1)
  b0[i] = m1$coefficients[[1]]
  b1[i] = m1$coefficients[[2]]
}

mean(b0) - beta_0
mean(b1) - beta_1

