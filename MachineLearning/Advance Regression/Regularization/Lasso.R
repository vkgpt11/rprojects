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
cv.out = cv.glmnet(x[train,],y[train],alpha=1) #alpha =0 means lasso regression

plot(cv.out)


#optimal lambda
minlambda <- cv.out$lambda.min
minlambda


#apply model on train dataset at lambda equal to minlamda
lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda = minlambda)


#prediction on test data set
lasso.pred = predict(lasso.mod,s=minlambda,newx = x[test,])

#compute mean squar error with lasso
mean((lasso.pred-y.test)^2)


#all the coefficients from the model at optimal lambda s=403.4

lasso.coef = predict(lasso.mod,type="coefficients",s=403.4)
lasso.coef

lasso.coef = predict(lasso.mod,type="coefficients",s=403.4)[1:65,]

#non zero coefficents in final model
lasso.coef[lasso.coef!=0]
