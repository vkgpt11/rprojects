library(randomForest)

data = read.csv("credit-card-default.csv")

#ignore ids
data[,1] = NULL

str(data)
factorCols = c("SEX","EDUCATION","MARRIAGE","default.payment.next.month")

data[,factorCols] = lapply(factorCols,function(factorCols) as.factor(as.character(data[,factorCols])))

#shuffle data --> To make sure that it is not biased 
shuffleddata = data[sample(nrow(data)),]

#split the data into train and test

ntrain = as.integer(nrow(shuffleddata)*0.8)
traindata = shuffleddata[1:ntrain,]
testdata = shuffleddata[(ntrain+1):nrow(shuffleddata),]

#Build the random forest  --> generate the trees every time you run so set seed
set.seed(71)

data.rf = randomForest(default.payment.next.month ~ .,data=traindata,
                       proximity = FALSE, do.trace = TRUE, na.action = na.omit)

data.rf

testPred = predict(data.rf,newdata = testdata)

table(testPred,testdata$default.payment.next.month)
