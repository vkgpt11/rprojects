filepath = "E:/Git/rprojects/MachineLearning/DecisionTree/bank.csv"

library(rpart)

bankdata = read.csv(filepath)
str(bankdata)

#Cast the cols to the correct types
cols = c(1,6,13:15)
bankdata[,cols] = as.integer(as.character(unlist((bankdata[,cols]))))

cols= c(2:5,7:9,11,13,16)
bankdata[,cols] = lapply(bankdata[,cols],factor)


#does not add much value to tree rather creates unstable tree 
bankdata$day = NULL
bankdata$duration =NULL

#build the decision tree
bankTree = rpart(y~.,data=bankdata,method='class',
                 control =rpart.control(minsplit = 65, cp=0.0001))
plot(bankTree)
text(bankTree,pretty = TRUE)
