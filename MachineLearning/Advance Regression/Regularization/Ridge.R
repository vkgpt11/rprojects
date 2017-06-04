setwd("E:\\Git\\rprojects\\MachineLearning\\Advance Regression\\Regularization")
carPrice = read.csv("carPrice.csv")
str(carPrice)
sum(is.na(carPrice))

#use glmnet package for regularization
#install.packages("glmnet")
library(glmnet)

#create a matrix x of all independent variables 
#and store dependent variable  in y
x = model.matrix(price~.,data=carPrice)[,-1]
y = carPrice$price


#training & test
set.seed(1)
train = sample(1:nrow(x),.7*nrow(x))

test = -train


y.test = y[test]


#cross validation
cv.out = cv.glmnet(x[train,],y[train],alpha=0) #alpha =0 means ridge regression

plot(cv.out)


#optimal lambda
minlambda <- cv.out$lambda.min
minlambda


#apply model on train dataset at lambda equal to minlamda
ridge.mod = glmnet(x[train,],y[train],alpha=0,lambda = minlambda)


#prediction on test data set
ridge.pred = predict(ridge.mod,s=minlambda,newx = x[test,])

#compute mean squar error with ridge
mean((ridge.pred-y.test)^2)




# apply model on train dataset at lambda equal to zero
ridge.mod = glmnet(x[train,],y[train],alpha = 0,lambda = 0)



#linear regression model
ridge.pred_0 = predict(ridge.mod,newx = x[test,])


#compute mean squar error with linear regression 
mean((ridge.pred_0-y.test)^2)
