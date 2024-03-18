#### Multiple Linear Regression ####

# read in data using table read.table()
# make sure you specify the pathway where you saved the data set
patient = read.table("patient.txt",header=FALSE)

head(patient) #look at the first few rows of the data, make sure it's been loaded into R correctly

# give each column its variable name
names(patient) = c("satisf","age","severe","anxiety")

head(patient)
str(patient)

pairs(patient) #pairwise scatterplot

# linear regression model
# use the lm function
model=lm(satisf~age+severe+anxiety,data=patient)

#shortcut: 
lm(satisf~.,data=patient)

### What are our least square coefficients?
### What is our fitted model? 

summary(model)
names(model)
model$coefficients
model$residuals
model$fitted.values

#use this model to predict Y for specific values of X 
x = data.frame(age=40,severe=40,anxiety=1.5)

predict(model,newdata=x)

########################
## In-class Activity  ##
########################

# 1. Fit a linear regression model with the response as patient satisfaction
# and use all other variables as predictors. Report the least square regression coefficients. 
# 2. Based on this model, what are the predicted patient satisfaction scores 
# for observations 1, 3, and 20?
# 3. Based on this model, what is the predicted patient satisfication score for a patient of age 50, disease severity 
# of 27 and anxiety of 3? 
# 4. Propose a way to quantify how well our model is able to predict patient 
# satisfaction (Y).  

## Work in groups to come up with a solution. 
## Copy and paste any relevant code on Ed Discussion. 
## Please be sure to list all your group members names. Only one group member needs to post on Ed Discussion. 
## link: https://edstem.org/us/courses/50620/discussion/


mlr_model = lm(satisf~.,data=patient)
print(mlr_model$coefficients)

predict(model)

temp = data.frame(age=50,severe=27,anxiety=3)
predict(model, newdata = temp)
