## this should be updated - k-fold CV should be done on training set only
k = 10
folds = sample(1:k,nrow(Hitters),replace=TRUE)

val.errors = matrix(NA,k,19)

## loop over k folds (for j in 1:k)
for(j in 1:k){
  test = Hitters[folds==j,]
  train = Hitters[folds!=j,]
  
  best.fit = regsubsets(Salary~.,data=train,nbest=1,nvmax=19)
  
  for(i in 1:19){
    test.mat = model.matrix(Salary~.,data=test)
    
    coef.m = coef(best.fit,id=i)
    
    pred = test.mat[,names(coef.m)]%*%coef.m
    val.errors[j,i] = mean((test$Salary-pred)^2)
  }
  
}

cv.errors = apply(val.errors,2,mean)
which.min(cv.errors)

full.reg = regsubsets(Salary~.,data=Hitters,nvmax=19,nbest=1)
coef(full.reg,10)
