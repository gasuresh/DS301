##############################
### Qualitative predictors ###
##############################

library(ISLR2)
head(Credit)
str(Credit)

summary(lm(Balance~Limit+Region,data=Credit))


# What are the fitted regression lines for each category of region? 

## Can change baseline:
Credit$Region <- relevel(Credit$Region, ref = "South")

summary(lm(Balance~Limit+Region,data=Credit))

lm.fit <- lm(Balance~Limit+Region+Student+Married,data=Credit)
lm.fit2 <- lm(Balance~Limit+Student+Married,data=Credit)

# partial F-test 
anova(lm.fit2, lm.fit)


##############################
##### Multicollinearity ######
##############################

##### simulation illustrating perfect correlation 

x1 = rnorm(100, mean=70, sd=15)
x2 = rnorm(100, mean=70, sd=15)

# Add in a linear combination of X1 and X2
x3 = (x1+x2)/2

# X4 is somewhat correlated with X1 but not relevant to Y
x4 = x1+runif(100,min=-2,max=2)

# Y is a linear combination of X1 and X2 plus noise
y = 0.7*x1 + 0.3*x2 + rnorm(100, 0, sqrt(15))

summary(lm(y~x1+x2+x3+x4))
summary(lm(y~x1+x2+x4))

#install.packages("car")
library(car)

vif(lm(y~x1+x2+x3+x4))

#######################
#### data example #####
#######################

library(ISLR2)
head(Credit)

fit0 = lm(Balance~Age+Limit+Rating, data=Credit)

fit1a = lm(Balance~Age+Limit,data=Credit)
summary(fit1a)

fit1b = lm(Balance~Age+Rating,data=Credit)
summary(fit1b)

## rating and limit are highly correlated

fit2 = lm(Balance~Rating+Limit,data=Credit)
summary(fit2)

plot(Rating~Limit,Credit)

vif(fit2)

## simple solutions to multicollinearity? 

########################
## In-class Activity  ##
########################

## Download the insurance.csv file on Canvas. 
## You can load your data into R using: 
insurance=read.csv("/.../insurance.csv") 

## Specify your pathway to be where you saved this file. 

## 1. This data set contains a few categorical predictors. 
## Check that all the categorical predictors in our dataset are stored correctly using str()
## If they are not, fix it. Copy and paste your output here. 

## 2. Fit a model with the response (Y) as health care charges and predictors
## x_1 age, x2 = bmi, and x3 = gender. 
## Based on your output, write out the fitted model for males only (gendermale = 1) and 
## write out the fitted model for females only (gendermale = 0). 

## 3. Your classmate tells you that including gender as a dummy variable in the model is not necessary. 
## Instead you can just fit a model for males only and a separate model for females only.
## Your classmate claims this is approach is equivalent to what you did in part 2. 

## To see whether or not your classmate's approach makes sense, 
## subset your data into two groups: data for males and data for females. 
## Fit a model with bmi and age for the male group only. Call this model fit_males. 
## Now do the same for the female group. Call this model fit_females. 
## Based on your output, write out both model's estimated regression coefficients.

## 4. Compare your results in part 2 with part 3. Are they equivalent? 
## Explain in plain language to your classmate 
## why these two approaches will not give the same results. 

## Work in groups to come up with a solution. 
## Copy and paste any relevant code on Ed Discussion. 
## Please be sure to list all your group members names. 
## Only one group member needs to post on Ed Discussion.


insurance=read.csv("./insurance.csv") 
str(insurance)
insurance$gender <- factor(insurance$gender)
insurance$smoker <- factor(insurance$smoker)
insurance$region <- factor(insurance$region)

insurance$gender <- relevel(insurance$gender, ref = "female")
summary(lm(charges~age+bmi+gender, data = insurance))

# Male: charges = -6986.82 + 243.19(age) + 327.54(bmi) + 1344.46
# Female: charges = -6986.82 + 243.19(age) + 327.54(bmi)

male_data <- subset(insurance, gender == "male")
female_data <- subset(insurance, gender == "female")

fit_males <- lm(charges ~ age + bmi, data = male_data)
summary(fit_males)
# charges = -8012.79 + 238.63(age) + 406.87(bmi)

fit_females <- lm(charges ~ age + bmi, data = female_data)
summary(fit_females)
# charges = -4515.22 + 246.92(age) + 241.32(bmi)

# They are not equivalent. 
# Including gender as a dummy variable in the model allows for estimating 
# the overall effect of gender on charges while controlling for other predictors.
# It considers the possibility that gender influences healthcare charges differently 
# across age and BMI groups.
# On the other hand, fitting separate models for males 
# and females treats gender as a categorical predictor with no interaction effect.
# It assumes that the relationships between age, BMI, and charges are the same 
# for both genders, but with potentially different intercepts and coefficients.


